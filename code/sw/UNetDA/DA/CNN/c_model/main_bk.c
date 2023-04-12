#include "include/swdnn.h"
#include "include/layers.h"
#include "include/utils.h"
#include "mpi.h"
#include <math.h>
#include <time.h>
#include <sys/time.h>
#include <assert.h>
#include <crts.h>
#define TIME(a,b) (1.0*((b).tv_sec-(a).tv_sec)+0.000001*((b).tv_usec-(a).tv_usec))
#define COMPARE(a,b) fabs((a-b)/(a+0.0000000001))


int main(int argc, char **argv) {
    //初始化
    CRTS_init();
    SWDNN_INIT();

    int N = 1;
    int C = 97;
    int H = 192;
    int W = 384;
    int64_t input_size = N *C * H * W;
    float * input_data0 =(float *)malloc(input_size*sizeof(float));
    swdnnReadData input_data;
    input_data.filename="./mid_res_bin/x_input.dat";
    input_data.read_len=input_size;
    input_data.data=input_data0;
    read_data(&input_data,1);

    //需要修改include/utils.h中的SAMPLE_NUM宏
    char * dir[]={"./parameters_bin", "./parameters_bin"};
    init_data(dir, N,C,H,W);

    int size = 1*1*192*384;
    float *out_t = (float *)malloc(size*sizeof(float));
    swdnnReadData Outt;
    Outt.filename="./mid_res_bin/out.dat";
    Outt.read_len=size;
    Outt.data=out_t;
    read_data(&Outt,1);

    float *out_0 = (float *)malloc(1*size*sizeof(float));

    float err;
    float temp = 0.0;
    int idx;
    int err_count = 0;
    int total_count = 0;
    float max_err = 0.0;
    int max_count = 0;
    struct timeval t1, t2;
    double cost_time1;

    gettimeofday(&t1, NULL);
    printf("begin\n");
    //ComplexUnet(&input_tensor, &Out, weight_data[0], bias_data[0], mean_data[0],var_data[0]);
    CNN_predict(input_data0,N,C,H,W,out_0,0);
    CNN_predict(input_data0,N,C,H,W,out_0,1);
    gettimeofday(&t2, NULL);
    cost_time1 = TIME(t1,t2);
    printf("Time:%f\n", cost_time1);
    
    for(int i=0;i<size;i++){
        //if(isnan(out_0[i]))printf("%f vs %f\n", out_t[i], out_0[i]);
        temp = COMPARE(out_t[i], out_0[i]);
        if(temp>max_err)
            max_err = temp;
        if(temp>=0.05){
            max_count++;
            //printf("%.10f vs %.10f, err=%f%\n", out_t[i], out_0[i], temp*100);
        }    
        err += temp;
    }  
    printf("max_err=%f%, max_count=%d, avg_err=%f, err=%f\n", max_err*100, max_count, err/size, err);
    
    free(input_data0);
    free(out_0);
    free(out_t);
    free_data();
	return 0;
}

