#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <crts.h>
#include "letkf_yb_fast.h"

extern void SLAVE_FUN(letkf_yb_slave)(void *);

void letkf_yb_fast_(int *ens, int *obs_num, int *obs_nz, int *obs_nvar, double *lat, double *lon, \
                    double *center_lat, double *center_lon, double *local_dist, int *nobsl, \
                    int *MAX_OBS, double *obs_polar, int *gc_filter, \
                    yb_ctype *dl_yb, ayb_ctype *dl_yo, double *dl_rinv, \
                    yb_ctype *yb, ayb_ctype *dis, float *rdiaginv, yb_ctype *rloc)
{
    yb_info info;
    info.ens = *ens;
    info.obs_num = *obs_num;
    info.obs_nz = *obs_nz;
    info.obs_nvar = *obs_nvar;
    info.lat = lat;
    info.lon = lon;
    info.center_lat = *center_lat;
    info.center_lon = *center_lon;
    info.local_dist = *local_dist;
    info.nobsl = nobsl;
    info.MAX_OBS = *MAX_OBS;
    info.obs_polar = *obs_polar;
    info.gc_filter = *gc_filter;
    info.dl_yb = dl_yb;
    info.dl_yo = dl_yo;
    info.dl_rinv = dl_rinv;
    info.yb = yb;
    info.dis = dis;
    info.rdiaginv = rdiaginv;
    info.rloc = rloc;

    //CRTS_init();

    CRTS_athread_spawn(letkf_yb_slave, &info);
    CRTS_athread_join();
    
    // printf("obs_list size %d\n", sizeof(obs_list));
    // int i;
    // for (i = 0; i < *lc_num; i++)
    // {
    //     printf("lc: %d east: %d west %d\n", i, obs_lc[i].east_n, obs_lc[i].west_n);
    // }
}