#include "include/swdnn.h"
#include "include/layers.h"
#include "include/utils.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <sys/time.h>
#define TIME(a,b) (1.0*((b).tv_sec-(a).tv_sec)+0.000001*((b).tv_usec-(a).tv_usec))


/*
struct swdnnReadDataStruct
{
    char * filename;
    int read_len;
    float * data;
};
typedef struct swdnnReadDataStruct swdnnReadData;
typedef struct swdnnReadDataStruct * swdnnReadData_t;

struct swdnn4DTensorStruct
{	
	  int N;
		int C;
		int H;
		int W;
    float * data;
};

typedef struct swdnn4DTensorStruct swdnn4DTensor;
typedef struct swdnn4DTensorStruct * swdnn4DTensor_t;
*/
void ModuleCnnInit(char *input_dir, int *st, int *Len, int *ens, int *lev, int *lat, int *lon)
{
	//printf("cnn_init_dir: %s\n", input_dir);
	char **my_dir;
	char *tmp_dir;
	char num_mem[10], num_lev[10];
	int i,j,tot,L,tp;

	CRTS_init();
	SWDNN_INIT();
	my_dir = (char**)malloc(sizeof(char*)*(*Len));
	tot = 0;
	for (i = (*st); i < (*st) + (*Len); i++) {
		if (i >= 10) {
			num_mem[0] = (i/10) + '0';
			num_mem[1] = (i%10) + '0';
			num_mem[2] = 0;
		}
		else {
			num_mem[0] = i + '0';
			num_mem[1] = 0;
		}
		tmp_dir = (char*)malloc(sizeof(char)*200);
		strcpy(tmp_dir, input_dir);
		L = strlen(tmp_dir);
		strcpy(tmp_dir + L, num_mem);
		L = L + strlen(num_mem);
		tmp_dir[L] = 0;
		my_dir[tot] = tmp_dir;
		
		//printf("cnn_init_dir %d: %s\n", tot, my_dir[tot]);
		
		tot++;
	}

	init_data(my_dir, (*ens),(*lev),(*lat),(*lon));
}

void ModuleCnnPredict(int *ens, int *lev, int *lat, int *lon, int *inx, float *in_val, float *out_val, int *rk)
{
	swdnn4DTensor tensor_in, tensor_out;
	int indx = (*inx);
	tensor_in.N = (*ens); //1
	tensor_in.C = (*lev); //97
	tensor_in.H = (*lat); //106
	tensor_in.W = (*lon); //196
	tensor_in.data = in_val;
	tensor_out.data = out_val;
	CNN_predict(in_val, tensor_in.N, tensor_in.C, tensor_in.H, tensor_in.W, out_val, indx);
	FILE *op;
	char out_path[11] = "ino_";
	out_path[4] = ((*rk)/10) + '0';
	out_path[5] = ((*rk)%10) + '0';
	strcpy(out_path + 6, ".txt");
	out_path[10] = 0;
	/*
	if (indx == 2) {
	op = fopen(out_path, "w");
	for (int ti = 0; ti < (*ens)*(*lat)*(*lon); ti++) {
		fprintf(op, "%.5f\n", in_val[ti]);
	}
	fclose(op);
	}
	*/
}

void ModuleCnnDestroy()
{
	free_data();
}


/*
int main(int argc, char **argv) {
  //输入为input_tensor，包括形状input_tensor.N,C,H,W,以及数据input_tensor.data
	//输出为output_tensor 包括形状output_tensor.N,C,H,W，C为1，以及数据output_tensor.data

	swdnn4DTensor input_tensor;
    input_tensor.N=2;
	input_tensor.C=97;
	input_tensor.H=190;
	input_tensor.W=370;
	float * input_data0 =(float *)malloc(2*2*97*190*370*sizeof(float));
	input_tensor.data=input_data0;
  input_data.filename="./100km_x_test.txt";
  input_data.read_len=2*97*190*370;
	input_data.data=input_data0;
  read_data(&input_data,1);
	
	//char *dir ="./model";
  char *dir[]={
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model",
		"./model"
	};
	init_data(dir);

    struct timeval t1, t2;
    double cost_time1;
    gettimeofday(&t1, NULL);

    test_predict(&input_tensor,&output_tensor[0],0);
	
    gettimeofday(&t2, NULL);
    cost_time1 = TIME(t1,t2);
    printf("cost_time=%lf\n", cost_time1);

	  float err=0;
		int size = 2*190*370;
		float * tmp0 = (float *)malloc(size*sizeof(float));
		swdnnReadData tmp;
		tmp.filename="./mid_res/out.txt";
		tmp.read_len=size;
		tmp.data=tmp0;
		read_data(&tmp,1);

	for(int dd=0; dd<tmp.read_len; dd++)
			err += fabs(tmp0[dd] - output_tensor[0].data[dd]);
		printf("\n\noutput err=%f\n\n", err); //gaoj



	printf("output_shape=%d, %d, %d, %d\n", output_tensor[0].N, output_tensor[0].C, output_tensor[0].H, output_tensor[0].W);
	free(tmp0);
	free(input_data0);
	free_data();
	return 0;
}
*/
