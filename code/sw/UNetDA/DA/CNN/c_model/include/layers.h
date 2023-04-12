#include "swdnn.h"



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

struct swdnnReadDataStruct
{
    char * filename;
    int read_len;
    float * data;
};
typedef struct swdnnReadDataStruct swdnnReadData;
typedef struct swdnnReadDataStruct * swdnnReadData_t;

struct ConvParaStruct
{
    int N;
    int Ci;
    int Hi;
    int Wi;
    int Co;
    int Ho;
    int Wo;
    int group;
    int Kh;
    int Kw;
};    
typedef struct ConvParaStruct ConvPara;
typedef struct ConvParaStruct *ConvPara_t;


//int64_t buffer_malloc_size = 233880192+257245184;
//float *buffer_ptr;
int Dilation=1;
int Group=1;
float alpha, beta;
double epsilon, ExponentialAverageFactor;
swdnnTensorFormat_t format;         
swdnnDataType_t dataType;
swdnnConvolutionMode_t mode;
swdnnConvolutionFwdAlgo_t algo;
swdnnNanPropagation_t maxpoolingNanOpt;
swdnnPoolingMode_t pooling_mode;
swdnnHandle_t handle = NULL;
swdnnTensorDescriptor_t xDesc = NULL;
swdnnTensorDescriptor_t yDesc = NULL;
swdnnTensorDescriptor_t bDesc = NULL;
swdnnFilterDescriptor_t wDesc = NULL;
swdnnConvolutionDescriptor_t convDesc = NULL;
swdnnBatchNormMode_t bnmode;
swdnnTensorDescriptor_t bnScaleBiasMeanVarDesc = NULL;
swdnnPoolingDescriptor_t poolingDesc = NULL;

void SWDNN_INIT();

void comp_outshape(int Hi, int Wi, int Khw, int Pad, int Stride, int *Ho, int *Wo){
    *Ho = (Hi + Pad*2-Dilation*(Khw-1)-1)/Stride + 1; 
    *Wo = (Wi + Pad*2-Dilation*(Khw-1)-1)/Stride + 1; 
}    

void get_conv_block_size(const ConvPara_t p, const float *ptr, int64_t ptr_malloc_size, int64_t *im2col_malloc_size, int64_t *gemm_malloc_size, float **gemm_malloc_ptr, float **im2col_malloc_ptr);

void conv2d(swdnn4DTensor_t Input, swdnn4DTensor_t Output, float* Weight, float* Bias, int Ci, int Co, int Khw, int Pad);

void BatchNorm2d(swdnn4DTensor_t Input, swdnn4DTensor_t Output, float *Weight, float *Bias, float *Mean, float *Var);

void ConCat(swdnn4DTensor_t Input1, swdnn4DTensor_t Input2, swdnn4DTensor_t Output);

void MaxPool2d(swdnn4DTensor_t Input, swdnn4DTensor_t Output, int Khw);

void Upsample(swdnn4DTensor_t Input, swdnn4DTensor_t Output);

void SiLU(swdnn4DTensor_t Input, swdnn4DTensor_t Output);

void Block_head(swdnn4DTensor_t Input, swdnn4DTensor_t Output, swdnnReadData_t Weight, swdnnReadData_t Bias, swdnnReadData_t Mean, swdnnReadData_t Var, int Ci, int Co);

void Block(swdnn4DTensor_t Input, swdnn4DTensor_t Output, swdnnReadData_t Weight, swdnnReadData_t Bias, swdnnReadData_t Mean, swdnnReadData_t Var, int Ci, int Co);

void ComplexUnet(swdnn4DTensor_t Input, swdnn4DTensor_t Output, swdnnReadData_t Weight, swdnnReadData_t Bias, swdnnReadData_t Mean, swdnnReadData_t Var);

void CNN_predict(float* input, int N, int C, int H, int W, float* output, int index);
