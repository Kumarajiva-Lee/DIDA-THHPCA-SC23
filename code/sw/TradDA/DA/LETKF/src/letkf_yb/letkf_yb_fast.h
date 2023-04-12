#ifndef LETKF_YB_H_
#define LETKF_YB_H_

#include "../../../utils/da_config.inc"

#if (yb_size == 8)
    #define yb_ctype double
#else
    #define yb_ctype float
#endif

#if (ayb_size == 8)
    #define ayb_ctype double
#else
    #define ayb_ctype float
#endif

#define letkf_max(a, b) (((a)>(b))?(a):(b))
#define letkf_min(a, b) (((a)<(b))?(a):(b))

#define MAX_SPM (128*1024)
#define THREAD_NUM 64

typedef struct
{
    int ens;
    int obs_num;
    int obs_nz;
    int obs_nvar;
    double *lat;
    double *lon;
    double center_lat;
    double center_lon;
    double local_dist;
    int *nobsl; // nobsl will be updated
    int MAX_OBS;
    double obs_polar;
    int gc_filter;
    yb_ctype *dl_yb;
    ayb_ctype *dl_yo;
    double *dl_rinv;
    yb_ctype *yb;
    ayb_ctype *dis;
    float *rdiaginv;
    yb_ctype *rloc;
}yb_info;


// typedef struct
// {
//     char obs_typename[10];
//     int obs_num;
//     int obs_nz;
//     int obs_nvar;
//     char (*obs_varname)[3];
//     int *obs_npos;
//     double *lat, *lon;
//     double *ph;
//     ayb_ctype *dis_yo;
//     yb_ctype *pro_yb;
//     double *obs_rinv;
// } obs_list;

// typedef struct
// {
//     int east_n, west_n;
//     obs_list *obs_data;
//     int *nid;
// } obs_latcircle;

#endif