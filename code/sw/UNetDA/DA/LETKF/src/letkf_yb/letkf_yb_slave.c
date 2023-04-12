#include <stdio.h>
#include <slave.h>
#include <simd.h>
#include <crts.h>
#include <math.h>
#include "letkf_yb_fast.h"

//#define PERF

#define MEM_FENCE asm volatile("memb":::"memory")

//communication
static inline void* remote_ldm_addr(volatile void* p, uintptr_t id) {
  uintptr_t x = (uintptr_t)p, y = (uintptr_t)id << 20, z = 1ull << 45;
  return (void*)(x | y | z);
}

// static inline void rstore8(TYPE* buf, volatile void* p, uintptr_t id)
// {
//     TYPE_V vbuf;
//     TYPE *addr = remote_ldm_addr(p, id);
//     simd_loadu(vbuf, buf);
//     simd_storeu(vbuf, addr);
// }

#define a 6371.393
double latlon_dist(double x1, double y1, double x2, double y2)
{
        double pi;
        
        pi = atan(1.0) * 4;
        return a*acos(letkf_min(cos(x1/180*pi)*cos(x2/180*pi)* \
        cos(y1/180*pi-y2/180*pi)+sin(x1/180*pi)*sin(x2/180*pi),1.0));

}
#undef a

double schr2(double ri)
{
    double rci, rj, rk;
    if (ri >= 2.0)
    {
        rci = 0.0;
    }
    else if (ri >= 1.0)
    {
        rj=ri/12.0-0.5;
        rj=rj*ri+0.625;
        rj=rj*ri+1.666667;
        rci=(rj*ri-5.0)*ri+4.0-0.6666667/ri;
    }
    else
    {
        rk=ri*ri;
        rj=(-0.25*ri+0.5)*ri+0.625;
        rci=(rj*ri-1.666667)*rk+1.0;
    }

    return rci;
}

double schr(double ri)
{
    if (ri >= 2.0)
    {
        return 0;
    }
    else
    {
        return 1.0;
    }
}

void letkf_yb_slave(void* _info)
{
    #ifdef PERF
        unsigned long time1, time2;
        double judgetime, waittime;
    #endif

    yb_info info;
    CRTS_dma_get(&info, _info, sizeof(yb_info));

    int id = CRTS_smng_get_tid();
    int threads = THREAD_NUM;

    int ens = info.ens;
    int obs_num = info.obs_num, obs_nvar = info.obs_nvar, obs_nz = info.obs_nz;
    double center_lat = info.center_lat, center_lon = info.center_lon;
    double local_dist = info.local_dist;
    int MAX_OBS = info.MAX_OBS;
    int obs_polar = info.obs_polar;
    int gc_filter = info.gc_filter;

    //buffer communication
    volatile int done_k = 0; //neighbour_done_signal
    volatile int total_nobsl = *(info.nobsl); //neighbour_total_nobsl

    if (total_nobsl >= MAX_OBS) return;

    CRTS_ssync_array();

    int bsize;
    bsize = letkf_min(obs_num / threads, MAX_SPM / sizeof(yb_ctype) / ens / obs_nz / obs_nvar);
    //bsize = 32 / (obs_nvar * obs_nz);
    if (bsize == 0) bsize = 1;
    int bstride = bsize * threads;

    //if (id == 0) printf("obs_num %d bsize %d bstride %d\n", obs_num, bsize, bstride);
    
    if (id < threads)
    {
        //printf("id: %d yb start!\n", id);

        double lat[bsize], lon[bsize];
        yb_ctype yb[ens * bsize * obs_nz * obs_nvar];
        ayb_ctype yo[bsize * obs_nz * obs_nvar];
        double rinv_in[obs_nz * obs_nvar];
        float rinv[bsize * obs_nz * obs_nvar];
        yb_ctype rloc[bsize * obs_nz * obs_nvar];

        for (int bstart = id * bsize; bstart < obs_num; bstart+=bstride)
        {
#ifdef PERF
            time1 = CRTS_stime_cycle();
#endif

            int nobsl = 0;
            int nobs_points = 0;
            int length = letkf_min(obs_num - bstart, bsize);
            double gc;

            CRTS_dma_get(lat, info.lat + bstart, sizeof(double) * length);
            CRTS_dma_get(lon, info.lon + bstart, sizeof(double) * length);

            for (int i = 0; i < length; i++)
            {
                if (abs(lat[i]) <= obs_polar)
                {
                    double obs_dist = latlon_dist(lat[i], lon[i], \
                            center_lat, center_lon);
                    
                    if (gc_filter)
                    {
                        gc = schr2(obs_dist * 1.0 / local_dist);
                    }
                    else
                    {
                        gc = schr(obs_dist * 1.0 / local_dist);
                    }

                    if (gc > 0)
                    {
                        CRTS_dma_get(yb + nobs_points * obs_nz * obs_nvar * ens, \
                        info.dl_yb + (bstart + i) * obs_nz * obs_nvar * ens, \
                        sizeof(yb_ctype) * obs_nz * obs_nvar * ens);

                        CRTS_dma_get(yo + nobs_points * obs_nz * obs_nvar, \
                        info.dl_yo + (bstart + i) * obs_nz * obs_nvar, \
                        sizeof(ayb_ctype) * obs_nz * obs_nvar);

                        CRTS_dma_get(rinv_in, \
                        info.dl_rinv + (bstart + i) * obs_nz * obs_nvar, \
                        sizeof(double) * obs_nz * obs_nvar);

                        for (int m = 0; m < obs_nz; m++)
                            for (int n = 0; n < obs_nvar; n++)
                            {
                                int pos = nobs_points * obs_nz * obs_nvar;
                                int lpos = obs_nvar * m + n;
                                rinv[pos + lpos] = rinv_in[lpos];
                                rloc[pos + lpos] = gc;
                                nobsl++;
                            }

                        nobs_points++;
                    }
                }
            }

#ifdef PERF
            time2 = CRTS_stime_cycle();
            judgetime += (time2 - time1) / 2.25 / 1000 / 1000;
#endif

            while (done_k < bstart);

#ifdef PERF
            time1 = CRTS_stime_cycle();
            waittime += (time1 - time2) / 2.25 / 1000 / 1000;
#endif

            if (done_k == bstart)
            {
                //printf("%d nobsl %d\n", id, total_nobsl + nobsl);
                if (total_nobsl + nobsl >= MAX_OBS)
                {
                    nobsl = MAX_OBS - total_nobsl;

                    //printf("id %d should end MAX_OBS %d\n", id, MAX_OBS);
                    
                    *(info.nobsl) = MAX_OBS;

                    for (int nid = 0; nid < threads; nid++)
                    {
                        if (nid != id)
                        {
                            int* remote_r = remote_ldm_addr(&done_k, nid);
                            *remote_r = obs_num + 1; 
                        }
                    }

                    CRTS_dma_put(info.yb + total_nobsl * ens, \
                        yb, sizeof(yb_ctype) * nobsl * ens);
                    CRTS_dma_put(info.dis + total_nobsl, \
                        yo, sizeof(ayb_ctype) * nobsl);
                    CRTS_dma_put(info.rdiaginv + total_nobsl, \
                        rinv, sizeof(float) * nobsl);
                    CRTS_dma_put(info.rloc + total_nobsl, \
                        rloc, sizeof(yb_ctype) * nobsl);

                    //printf("id %d come to an end\n", id);
#ifdef PERF
                    printf("id %d judgetime %.4f\n", id, judgetime);
                    printf("id %d waittime %.4f\n", id, waittime);
#endif 
                    return;
                }
                else
                {

                    if (bstart + length < obs_num)
                    {
                        int* remote_r = remote_ldm_addr(&total_nobsl, (id + 1) % threads);
                        *remote_r = total_nobsl + nobsl;
                        remote_r = remote_ldm_addr(&done_k, (id + 1) % threads);
                        *remote_r = bstart + bsize;

                        CRTS_dma_put(info.yb + total_nobsl * ens, \
                            yb, sizeof(yb_ctype) * nobsl * ens);
                        CRTS_dma_put(info.dis + total_nobsl, \
                            yo, sizeof(ayb_ctype) * nobsl);
                        CRTS_dma_put(info.rdiaginv + total_nobsl, \
                            rinv, sizeof(float) * nobsl);
                        CRTS_dma_put(info.rloc + total_nobsl, \
                            rloc, sizeof(yb_ctype) * nobsl);
                    }
                    else
                    {
                        //printf("id %d should end?????\n", id);
                        *(info.nobsl) = total_nobsl + nobsl;
                        for (int nid = 0; nid < threads; nid++)
                        {
                            if (nid != id)
                            {
                                int* remote_r = remote_ldm_addr(&done_k, nid);
                                *remote_r = obs_num + 1; 
                            }
                        }
                       
                        CRTS_dma_put(info.yb + total_nobsl * ens, \
                            yb, sizeof(yb_ctype) * nobsl * ens);
                        CRTS_dma_put(info.dis + total_nobsl, \
                            yo, sizeof(ayb_ctype) * nobsl);
                        CRTS_dma_put(info.rdiaginv + total_nobsl, \
                            rinv, sizeof(float) * nobsl);
                        CRTS_dma_put(info.rloc + total_nobsl, \
                            rloc, sizeof(yb_ctype) * nobsl);
#ifdef PERF
                    printf("id %d judgetime %.4f\n", id, judgetime);
                    printf("id %d waittime %.4f\n", id, waittime);
#endif                        
                        return;
                    }

                }

            }
            else
            {
#ifdef PERF
                printf("id %d judgetime %.4f\n", id, judgetime);
                printf("id %d waittime %.4f\n", id, waittime);
#endif          
                return;
            }

        }
    }
}
