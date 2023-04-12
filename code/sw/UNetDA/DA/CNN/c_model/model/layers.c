#include <sys/time.h>                                                                                                                 
#include <time.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "swdnn.h"
#include "layers.h"
#include "utils.h"
extern swdnn4DTensor_t Temp1, Temp2, Temp3, Temp4;
extern swdnnReadData bias_data[SAMPLE_NUM][BIAS_LEN];
extern swdnnReadData weight_data[SAMPLE_NUM][WEIGHT_LEN];
extern swdnnReadData mean_data[SAMPLE_NUM][MEAN_LEN];
extern swdnnReadData var_data[SAMPLE_NUM][VAR_LEN];
extern swdnn4DTensor_t Encode[4];
extern int64_t buffer_size;
extern float *buffer_ptr;

struct timeval t1, t2;
double cost_time = 0;

void SWDNN_INIT(){
    format = SWDNN_TENSOR_NCHW;
    mode = SWDNN_CONVOLUTION;
    dataType = SWDNN_DATA_FLOAT;
    algo = SWDNN_CONVOLUTION_FWD_ALGO_IM2COL;
    maxpoolingNanOpt = SWDNN_PROPAGATE_NAN;
    pooling_mode = SWDNN_POOLING_MAX;
    swdnnCreate(&handle);
    swdnnCreateTensorDescriptor(&xDesc);
    swdnnCreateTensorDescriptor(&yDesc);
    swdnnCreateTensorDescriptor(&bDesc);
    swdnnCreateFilterDescriptor(&wDesc);
    swdnnCreateConvolutionDescriptor(&convDesc);
    swdnnSetConvolutionGroupCount(convDesc, Group);
    bnmode = SWDNN_BATCHNORM_SPATIAL;
    alpha = 1.0;
    beta = 0.0;
    epsilon=1e-5, ExponentialAverageFactor = 0.1;
    swdnnCreateTensorDescriptor(&bnScaleBiasMeanVarDesc);
    swdnnCreatePoolingDescriptor(&poolingDesc);
}
    

void get_conv_block_size(const ConvPara_t p, const float *ptr, int64_t ptr_malloc_size, int64_t *im2col_malloc_size, 
        int64_t *gemm_malloc_size, float **gemm_malloc_ptr, float **im2col_malloc_ptr)
{
    int64_t col_buffer_size = (p->Ci/p->group) * 1 * p->Kh * p->Kw * p->Ho * p->Wo; 
    int gemm_M = p->Co; 
    int gemm_N = p->Ho * p->Wo;
    int gemm_K = (p->Ci/p->group) * 1 * p->Kh * p->Kw; 
    int bM, bN, bK, kernel_type;
    get_blocks(gemm_M, gemm_N, gemm_K, &bM, &bN, &bK, &kernel_type);
    int Mend = ((gemm_M+bM-1)/bM)*bM;
    int Nend = ((gemm_N+bN-1)/bN)*bN;
    int Kend = ((gemm_K+bK-1)/bK)*bK;
    int64_t gemm_buffer_size = 0;
    if(Mend>gemm_M || Kend> gemm_K)gemm_buffer_size += Mend*Kend;
    if(Nend>gemm_N || Kend> gemm_K)gemm_buffer_size += Nend*Kend;
    if(Mend>gemm_M || Nend> gemm_N)gemm_buffer_size += Mend*Nend;
    *im2col_malloc_size = col_buffer_size;
    *gemm_malloc_size = gemm_buffer_size;
    assert(ptr_malloc_size>=(col_buffer_size+gemm_buffer_size));
    float* buffer_malloc_ptr = (float*)ptr;
    *im2col_malloc_ptr = buffer_malloc_ptr;
    *gemm_malloc_ptr = buffer_malloc_ptr + col_buffer_size;
}        

void conv2d(swdnn4DTensor_t Input, swdnn4DTensor_t Output, float* Weight, float *Bias, int Ci, int Co, int Khw, int Pad){

    //gettimeofday(&t1, NULL);
    int N, Hi, Wi;
    int Ho, Wo;
    int Stride = 1;
    N = Input->N;
    assert(Ci==(Input->C));
    Hi = Input->H;
    Wi = Input->W;
    comp_outshape(Hi, Wi, Khw, Pad, Stride, &Ho, &Wo);
    swdnnSetTensor4dDescriptor(xDesc, format, dataType, N, Ci, Hi, Wi);
    swdnnSetFilter4dDescriptor(wDesc, format, dataType, Co, Ci/Group, Khw, Khw);
    swdnnSetTensor4dDescriptor(bDesc, format, dataType, 1, Co, 1, 1);
    swdnnSetTensor4dDescriptor(yDesc, format, dataType, N, Co, Ho, Wo);
    swdnnSetConvolution2dDescriptor(convDesc, Pad, Pad, Stride, Stride, Dilation, Dilation, mode, dataType);
    int64_t col_buffer_size, gemm_buffer_size;
    float *gemm_malloc_ptr, *im2col_malloc_ptr;
    ConvPara p;
    p.N = N; p.Ci = Ci; p.Hi = Hi; p.Wi = Wi;
    p.Co = Co; p.Ho = Ho; p.Wo = Wo; p.group = Group;
    p.Kh = Khw; p.Kw = Khw;
    get_conv_block_size(&p, buffer_ptr, buffer_size, &col_buffer_size, &gemm_buffer_size, &gemm_malloc_ptr, &im2col_malloc_ptr);
    assert(gemm_malloc_ptr!=NULL);
    assert(im2col_malloc_ptr!=NULL);
    Output->N = N;
    Output->C = Co;
    Output->H = Ho;
    Output->W = Wo;
    swdnnConvolutionBiasForward_malloc(handle, &alpha, xDesc,Input->data, wDesc, Weight, bDesc, Bias, convDesc, algo, &beta, yDesc, Output->data, im2col_malloc_ptr, col_buffer_size, gemm_malloc_ptr, gemm_buffer_size);
    //master_conv_forward_f(Input->data, Weight, Output->data, N, Ci, Hi, Wi, Co, Ci, Khw, Khw, Group, Stride, Stride, Pad, Pad, Dilation, Dilation, Bias, 1);
    //gettimeofday(&t2, NULL);
    //cost_time  += TIME(t1,t2);
}    

void BatchNorm2d(swdnn4DTensor_t Input, swdnn4DTensor_t Output, float *Weight, float *Bias, float *Mean, float *Var){
    int N, C, H, W;
    N = Input->N;
    C = Input->C;
    H = Input->H;
    W = Input->W;

    swdnnSetTensor4dDescriptor(xDesc, format, dataType, N, C, H, W);
    swdnnSetTensor4dDescriptor(bnScaleBiasMeanVarDesc,format,dataType,1,C,1,1);
    Output->N = N;
    Output->C = C;
    Output->H = H;
    Output->W = W;

    swdnnBatchNormalizationForwardInference(handle, bnmode, &alpha, &beta, xDesc, Input->data, xDesc, Output->data, bnScaleBiasMeanVarDesc, Weight, Bias, Mean, Var,epsilon); 

}    

void ConCat(swdnn4DTensor_t Input1, swdnn4DTensor_t Input2, swdnn4DTensor_t Output){
    int N1,C1,H1,W1;
    int N2,C2,H2,W2;
    N1 = Input1->N; C1 = Input1->C; 
    H1 = Input1->H; W1 = Input1->W;
    N2 = Input2->N; C2 = Input2->C;
    H2 = Input2->H; W2 = Input2->W;
    assert(N1==N2 && H1==H2 && W1==W2);
    swdnnSetTensor4dDescriptor(xDesc, format, dataType, N1, C1, H1, W1);
    swdnnSetTensor4dDescriptor(yDesc, format, dataType, N2, C2, H2, W2);
    swdnnSetTensor4dDescriptor(wDesc, format, dataType, N1, C1+C2, H2, W2);
    Output->N = N1;
    Output->C = C1+C2;
    Output->H = H1;
    Output->W = W1;
    sw_cat_f(xDesc, Input1->data, yDesc, Input2->data, wDesc, Output->data, 1);
}    

void MaxPool2d(swdnn4DTensor_t Input, swdnn4DTensor_t Output, int Khw){
    int Pad = 0;
    int Stride = Khw;
    int N, C, H, W;
    int Ho,Wo;
    N = Input->N; C = Input->C;
    H = Input->H; W = Input->W;
    comp_outshape(H, W, Khw, Pad, Stride, &Ho, &Wo);
    Output->N = N; Output->C = C;
    Output->H = Ho; Output->W = Wo;

    swdnnSetPooling2dDescriptor(poolingDesc, pooling_mode, maxpoolingNanOpt, Khw, Khw, Pad, Pad, Stride, Stride);
    swdnnSetTensor4dDescriptor(xDesc, format, dataType, N, C, H, W);
    swdnnSetTensor4dDescriptor(yDesc, format, dataType, N, C, Ho, Wo);
    swdnnPoolingForward(handle, poolingDesc, &alpha, xDesc, Input->data, &beta, yDesc, Output->data);
}    

void Upsample(swdnn4DTensor_t Input, swdnn4DTensor_t Output){
    int N,C,H,W;
    N = Input->N; C = Input->C;
    H = Input->H; W = Input->W;
    Output->N = N; Output->C = C;
    Output->H = H*2; Output->W = W*2;
    swdnnSetTensor4dDescriptor(xDesc, format, dataType, N, C, H, W);
    swdnnSetTensor4dDescriptor(yDesc, format, dataType, N, C, H*2, W*2);
    float kk[2] = {2.0,2.0};
    sw_upsample_f(kk,xDesc,Input->data,yDesc,Output->data);
}    

void SiLU(swdnn4DTensor_t Input, swdnn4DTensor_t Output){
    int N,C,H,W;
    N = Input->N; C = Input->C;
    H = Input->H; W = Input->W;
    Output->N = N; Output->C = C;
    Output->H = H; Output->W = W;
    sw_silu_forward_f(Input->data, Output->data, N*C*H*W);
}    

void Block_head(swdnn4DTensor_t Input, swdnn4DTensor_t Output, swdnnReadData_t Weight, swdnnReadData_t Bias, swdnnReadData_t Mean, swdnnReadData_t Var, int Ci, int Co){
    conv2d(Input, Output, Weight[0].data, Bias[0].data, Ci, Co, 3, 1);
    BatchNorm2d(Output, Temp1, Weight[1].data, Bias[1].data, Mean[0].data, Var[0].data);
    SiLU(Temp1, Output);
    conv2d(Output, Temp1, Weight[2].data, Bias[2].data, Co, Co, 3, 1);//x1
    conv2d(Input, Temp2, Weight[3].data, Bias[3].data, Ci, Co, 3, 1);
    Output->N = Temp2->N;
    Output->C = Temp2->C;
    Output->H = Temp2->H;
    Output->W = Temp2->W;

    unsigned long len = Temp2->N * Temp2->C * Temp2->H * Temp2->W;
    sw_add_f(Temp1->data, Temp2->data, Output->data, len); 
} 



void Block(swdnn4DTensor_t Input, swdnn4DTensor_t Output, swdnnReadData_t Weight, swdnnReadData_t Bias, swdnnReadData_t Mean, swdnnReadData_t Var, int Ci, int Co){
     BatchNorm2d(Input, Temp2, Weight[0].data, Bias[0].data, Mean[0].data, Var[0].data); 
     SiLU(Temp2, Temp1);
     conv2d(Temp1, Temp2, Weight[1].data, Bias[1].data, Ci, Co, 3,1);
     BatchNorm2d(Temp2, Temp1, Weight[2].data, Bias[2].data, Mean[1].data, Var[1].data);
     SiLU(Temp1, Temp2);
     conv2d(Temp2, Temp1, Weight[3].data, Bias[3].data, Co, Co, 3,1);//x1
     conv2d(Input, Temp2, Weight[4].data, Bias[4].data, Ci, Co, 3,1);

     Output->N = Temp2->N;
     Output->C = Temp2->C;
     Output->H = Temp2->H;
     Output->W = Temp2->W;
     unsigned long len = Temp2->N * Temp2->C * Temp2->H * Temp2->W;     
     sw_add_f(Temp1->data, Temp2->data, Output->data, len);
}    

void ComplexUnet(swdnn4DTensor_t Input, swdnn4DTensor_t Output, swdnnReadData_t Weight, swdnnReadData_t Bias, swdnnReadData_t Mean, swdnnReadData_t Var){
    //Encode
    Block_head(Input, Encode[0], &Weight[0], &Bias[0], &Mean[0], &Var[0], 97,16);
    MaxPool2d(Encode[0], Temp3, 2);
    Block(Temp3, Encode[1], &Weight[4], &Bias[4], &Mean[1], &Var[1],16, 32);
    MaxPool2d(Encode[1], Temp3, 2);
    Block(Temp3, Encode[2], &Weight[9], &Bias[9], &Mean[3], &Var[3],32, 64);
    MaxPool2d(Encode[2], Temp3, 2);
    Block(Temp3, Encode[3], &Weight[14], &Bias[14], &Mean[5], &Var[5],64, 128);
    MaxPool2d(Encode[3], Temp3, 2);
    Block(Temp3, Temp4, &Weight[19], &Bias[19], &Mean[7], &Var[7],128, 256);
    //w25, B25, M9, V9

    //Decoder0
    Upsample(Temp4, Temp3);
    conv2d(Temp3, Temp4, Weight[24].data, Bias[24].data, 256, 128, 3,1);
    ConCat(Temp4, Encode[3], Temp3);

    Block(Temp3, Temp4, &Weight[25], &Bias[25], &Mean[9], &Var[9],256, 128);

    //Decoder1
    Upsample(Temp4, Temp3);
    conv2d(Temp3, Temp4, Weight[30].data, Bias[30].data, 128, 64, 3,1);
    ConCat(Temp4, Encode[2], Temp3);
    Block(Temp3, Temp4, &Weight[31], &Bias[31], &Mean[11], &Var[11], 128, 64);

    //Decoder2
    Upsample(Temp4, Temp3);
    conv2d(Temp3, Temp4, Weight[36].data, Bias[36].data, 64, 32, 3,1);
    ConCat(Temp4, Encode[1], Temp3);
    Block(Temp3, Temp4, &Weight[37], &Bias[37], &Mean[13], &Var[13], 64, 32);

    //Decoder3
    Upsample(Temp4, Temp3);
    conv2d(Temp3, Temp4, Weight[42].data, Bias[42].data, 32, 16, 3,1);
    ConCat(Temp4, Encode[0], Temp3);
    Block(Temp3, Temp4, &Weight[43], &Bias[43], &Mean[15], &Var[15], 32, 16);
    
    //Head
    conv2d(Temp4, Temp3, Weight[48].data, Bias[48].data, 16, 1, 1,0);
    int64_t len = Temp3->N * Temp3->C * Temp3->H * Temp3->W;
    sw_add_f(Temp3->data, Input->data, Output->data, len);
    //printf("UPSAMPLE time %lf \n", cost_time); 
}

void CNN_predict(float* input, int N, int C, int H, int W, float* output, int index){
    swdnn4DTensor input_tensor;
    input_tensor.N = N;
    input_tensor.C = C;
    input_tensor.H = H;
    input_tensor.W = W;
    input_tensor.data = input;
   
    swdnn4DTensor output_tensor;
    output_tensor.data = output;
    ComplexUnet(&input_tensor, &output_tensor, weight_data[index], bias_data[index], mean_data[index],var_data[index]);

}    
