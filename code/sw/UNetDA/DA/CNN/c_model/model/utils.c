#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include<assert.h>
#include"utils.h"

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

float *buffer_ptr;//modified by gaoj 0407
int64_t buffer_size = 64364544+70836224+10;//modified by gaoj 0407


struct swdnnReadDataStruct
{
    char * filename;
    int read_len;
    float * data;
};
typedef struct swdnnReadDataStruct swdnnReadData;
typedef struct swdnnReadDataStruct * swdnnReadData_t;

swdnnReadData bias_data[SAMPLE_NUM][BIAS_LEN];
swdnnReadData weight_data[SAMPLE_NUM][WEIGHT_LEN];
swdnnReadData mean_data[SAMPLE_NUM][MEAN_LEN];
swdnnReadData var_data[SAMPLE_NUM][VAR_LEN];

float * Xdata;
float * Ydata;
float * Zdata;
float * Vdata;
swdnn4DTensor temp1, temp2, temp3, temp4;
swdnn4DTensor_t Temp1, Temp2, Temp3, Temp4;
float *encode0, *encode1, *encode2, *encode3;
swdnn4DTensor encode[4];
swdnn4DTensor_t Encode[4];

void read_data(swdnnReadData_t read_data,int item)
{
	int i,j,k;
	int num=0,index=0;
	int index1=0;
	FILE *fp[WEIGHT_LEN];
	for(i=0;i<item;i++)
	{
  		fp[i]=fopen(read_data[i].filename,"r");
		assert(fp[i]!=NULL);
	}
	for(i=0;i<item;i++)
	{
#if 1
        fread(read_data[i].data, sizeof(float), read_data[i].read_len, fp[i]);
#else
#endif
		fclose(fp[i]);
	}
#if 0
	for(i=0;i<item;i++)
		free(str[i]);
	free(str);
#endif
}

void free_data()
{
	int i,j;
	for(i=0;i<SAMPLE_NUM;i++)
	{
     for(j=0;j<MEAN_LEN;j++)
 		 {
 		 	free(mean_data[i][j].data);
 		 	free(var_data[i][j].data);
 		 }
     for(j=0;j<WEIGHT_LEN;j++)
 		 {
 		 	free(bias_data[i][j].data);
 		 	free(weight_data[i][j].data);
 		 }
	}
	free(Xdata);
	free(Ydata);
	free(Zdata);
	free(Vdata);
    free(encode0);
    free(encode1);
    free(encode2);
    free(encode3);
    free(buffer_ptr);//modified by gaoj 0407
}

void output(swdnnReadData_t read_data)
{
        FILE *fp;
	fp=fopen("./out.txt","w");
	assert(fp!=NULL);
	int i;
	for(i=0;i<read_data->read_len-1;i++)
		fprintf(fp,"%.18e ",read_data->data[i]);
	fprintf(fp,"%.18e\n",read_data->data[i]);
	fclose(fp);

}

void init_model(char * dir,int num,char **bias_f,char ** weight_f,char **mean_f,char **var_f,int *bias_l,int *weight_l,int*mean_l,int *var_l)
{	
        int i;
		char filename[TOTAL_LEN][128];
		//		char filename[24][128];
				int nf=0;
        for(i=0;i<WEIGHT_LEN;i++)
        {

					memset(filename[nf],0,128);
					strcat(filename[nf],dir);
					strcat(filename[nf],bias_f[i]);
        	bias_data[num][i].filename=filename[nf];
        	bias_data[num][i].read_len=bias_l[i];
					
					nf++;
					memset(filename[nf],0,128);
					strcat(filename[nf],dir);
					strcat(filename[nf],weight_f[i]);
        	weight_data[num][i].filename=filename[nf];
        	weight_data[num][i].read_len=weight_l[i];
					nf++;

        	bias_data[num][i].data=(float *)malloc(bias_data[num][i].read_len*sizeof(float));
             	assert(bias_data[num][i].data!=NULL);
        	weight_data[num][i].data=(float *)malloc(weight_data[num][i].read_len*sizeof(float));
             	assert(weight_data[num][i].data!=NULL);
        }
        for(i=0;i<MEAN_LEN;i++)
        {

					memset(filename[nf],0,128);
					strcat(filename[nf],dir);
					strcat(filename[nf],mean_f[i]);
        	mean_data[num][i].filename=filename[nf];
        	mean_data[num][i].read_len=mean_l[i];
					nf++;

					memset(filename[nf],0,128);
					strcat(filename[nf],dir);
					strcat(filename[nf],var_f[i]);
        	var_data[num][i].filename=filename[nf];
        	var_data[num][i].read_len=var_l[i];
					nf++;
        	mean_data[num][i].data=(float *)malloc(mean_data[num][i].read_len*sizeof(float));
             	assert(mean_data[num][i].data!=NULL);
        	var_data[num][i].data=(float *)malloc(var_data[num][i].read_len*sizeof(float));
             	assert(var_data[num][i].data!=NULL);
        }
        read_data(bias_data[num],BIAS_LEN);
        read_data(weight_data[num],WEIGHT_LEN);
        read_data(mean_data[num],MEAN_LEN);
        read_data(var_data[num],VAR_LEN);

}

void init_data(char ** dir, int N ,int C, int H, int W)
{	
        int64_t max_tensor_len = N * 32 * H * W;
		for(int i=0;i<SAMPLE_NUM;i++)
		{
				init_model(dir[i],i,bias_f,weight_f,mean_f,var_f,bias_l,weight_l,mean_l,var_l);
		}
		Xdata = (float *)malloc(max_tensor_len * sizeof(float));//用于缓存各个layer中数据，建议设置成可能用到的最大空间，Ydata、c1_data、c2_data也是一样的要求
		assert(Xdata!=NULL);
		Ydata = (float *)malloc(max_tensor_len * sizeof(float));
		assert(Ydata!=NULL);
		Zdata = (float *)malloc(max_tensor_len * sizeof(float));
		assert(Zdata!=NULL);
        Vdata = (float *)malloc(max_tensor_len * sizeof(float));
		assert(Vdata!=NULL);

        temp1.data = Xdata; 
        temp2.data = Ydata; 
        temp3.data = Zdata;
        temp4.data = Vdata;
        Temp1 = &temp1; Temp2 = &temp2; 
        Temp3 = &temp3; Temp4 = &temp4;       
        encode0=(float *)malloc(max_tensor_len/2 * sizeof(float));
        assert(encode0!=NULL);
        encode1=(float *)malloc(max_tensor_len/4 * sizeof(float));
        assert(encode1!=NULL);
        encode2=(float *)malloc(max_tensor_len/8 * sizeof(float));
        assert(encode2!=NULL);
        encode3=(float *)malloc(max_tensor_len/16 * sizeof(float));
        assert(encode3!=NULL);
        encode[0].data = encode0;
        Encode[0] = &encode[0];
        encode[1].data = encode1;
        Encode[1] = &encode[1];
         encode[2].data = encode2;
        Encode[2] = &encode[2];
         encode[3].data = encode3;
        Encode[3] = &encode[3];
     
        buffer_ptr = (float *)malloc(buffer_size * sizeof(float));
        assert(buffer_ptr!=NULL);//modified by gaoj 0407

}
