1、结构
|-include/
|----swdnn.h     底层算子相关接口
|----utils.h     权重宏定义以及权重名及长度
|----layers.h    上层算子接口以及模型调用接口
|-libs/          依赖库
|----libswblas.a
|----libswdnn.a
|-model/
|----layers.c    上层算子接口以及模型调用接口
|----utils.c     初始化相关接口

2、结构体

2.1、swdnn4DTensor用于上层算子接口的输入输出
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

2.2、swdnnReadData用于读取文件数据
struct swdnnReadDataStruct
{
    char * filename;
    int read_len;
    float * data;
};
typedef struct swdnnReadDataStruct swdnnReadData;
typedef struct swdnnReadDataStruct * swdnnReadData_t;

3、接口介绍

SWDNN_INIT()
初始化SWDNN算子所需句柄等结构体

read_data(swdnnReadData_t read_data,int item)
读取read_data数组中的每个文件，item为数组长度

init_data(char ** dir, int N ,int C, int H, int W)
dir：97个模型权重的存放路径
NCHW：输入tensor（所有输入中最大的）的大小
功能：
（1）加载所有模型权重到bias_data[SAMPLE_NUM][BIAS_LEN]、weight_data[SAMPLE_NUM][WEIGHT_LEN]、mean_data[SAMPLE_NUM][MEAN_LEN]、var_data[SAMPLE_NUM][VAR_LEN]
（2）为中间结果encode0、encode1、encode2、encode3以及中间张量Temp1、Temp2、Temp3、Temp4以及矩阵乘所需缓存开辟空间

void CNN_predict(float* input, int N, int C, int H, int W, float* output, int index)
模型调用接口，NCHW为input的shape，index表示加载第几个模型的权重

free_data()
释放空间

4、使用方法
（1）根据需求修改include/utils.h的宏和权重名，其中SAMPLE_NUM表示模型数量，WEIGHT_LEN表示单个模型中的weight数量
（2）初始化
    CRTS_init();
    SWDNN_INIT();
（3）提供权重文件存放路径和input的最大NCHW，调用init_data(dir, N,C,H,W);加载文件并开辟内存空间
    char * dir[]={"./parameters_bin", "./parameters_bin"};
    init_data(dir, N,C,H,W);
（4）调用模型接口
    CNN_predict(input,N,C,H,W,output,index);
（5）释放空间
    free_data();

5、例子
执行sh run.sh

    







