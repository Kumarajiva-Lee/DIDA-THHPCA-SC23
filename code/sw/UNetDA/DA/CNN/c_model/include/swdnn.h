/*
 * Created by Huangzq at 2020/03/13
 *
 */
#include <crts.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define TIME(a,b) (1.0*((b).tv_sec-(a).tv_sec)+0.000001*((b).tv_usec-(a).tv_usec))

#if !defined(SWDNN_H_)
#define SWDNN_H_

#define SWDNN_MAJOR 1
#define SWDNN_MINOR 1
#define SWDNN_PATCHLEVEL 0

#define SWDNN_VERSION    (SWDNN_MAJOR * 1000 + SWDNN_MINOR * 100 + SWDNN_PATCHLEVEL)

#if defined (__cplusplus)
extern "C" {
#endif

struct swdnnHandleStruct
{
    int swdnn_num_mpe;
    int swdnn_num_spe;
};
typedef struct swdnnHandleStruct swdnnHandle;
typedef struct swdnnHandleStruct * swdnnHandle_t;

size_t  swdnnGetVersion	(void);	//To be Done, Marked by Huangzq at 10:25 am 2019/03/13

/*
 * SWDNN return codes
 */
enum swdnnStatusEnum
{
    SWDNN_STATUS_SUCCESS    			=   0,  //正常返回
    SWDNN_STATUS_ALLOC_FAILED   		=   1,  //申请空间异常  
    SWDNN_STATUS_BAD_PARAM  			=   2,  //参数异常
    SWDNN_STATUS_INTERNAL_ERROR 		=   3,  //函数内部错误
    SWDNN_STATUS_NOT_SUPPORTED   		=   4,	//功能尚未支持
    SWDNN_STATUS_RUNTIME_FP_OVERFLOW    =   5,  //浮点溢出
    SWDNN_STATUS_NOT_INITIALIZED    	=   6,  //swdnn未进行初始化
};
typedef enum swdnnStatusEnum swdnnStatus_t; 

/* human-readable error messages */

inline const char * swdnnGetErrorString	(swdnnStatus_t status){
	switch (status) {
		case SWDNN_STATUS_SUCCESS:
			return "SWDNN_STATUS_SUCCESS";
		case SWDNN_STATUS_ALLOC_FAILED:
			return "SWDNN_STATUS_ALLOC_FAILED";
		case SWDNN_STATUS_BAD_PARAM:
			return "SWDNN_STATUS_BAD_PARAM";
		case SWDNN_STATUS_INTERNAL_ERROR:
			return "SWDNN_STATUS_INTERNAL_ERROR";
		case SWDNN_STATUS_NOT_SUPPORTED:
			return "SWDNN_STATUS_NOT_SUPPORTED";
		case SWDNN_STATUS_RUNTIME_FP_OVERFLOW:
			return "SWDNN_STATUS_NOT_INITIALIZED";
		default:
			return "Unknown swdnn error number";
	}
}

swdnnStatus_t swdnnCreate	(swdnnHandle_t *handle); //To be Done, Marked by Huangzq at 10:25 am 2019/03
swdnnStatus_t swdnnDestroy	(swdnnHandle_t handle); //To be Done, Marked by Huangzq at 10:25 am 2019/03

/*
 *	SWDNN data type
 */
enum swdnnDataTypeEnum
{
    SWDNN_DATA_FLOAT    =   0,
    SWDNN_DATA_DOUBLE   =   1,
    SWDNN_DATA_HALF 	=   2,
    SWDNN_DATA_INT 	=   3,
    SWDNN_DATA_CHAR 	=   4,
    SWDNN_DATA_INT8x4	=   5,	//For TensorFormat NCHW_VECT_C only
};
typedef enum swdnnDataTypeEnum swdnnDataType_t;

/*
 *	SWDNN propagate Nan
 */
enum swdnnNanPropagationEnum
{
   SWDNN_NOT_PROGATE_NAN    =   0,  //Nan numbers are not propagated
   SWDNN_PROPAGATE_NAN  	=   1,  //Nan numbers are propagated
};
typedef enum swdnnNanPropagationEnum swdnnNanPropagation_t;

/*
 *	SWDNN Tensor Format
 */
enum swdnnTensorFormatEnum
{
    SWDNN_TENSOR_NCHW   =   0,	/* row major (wStride = 1, hStride = w) */
    SWDNN_TENSOR_NHWC   =   1,  /* feature maps interleaved ( cStride = 1 )*/
    SWDNN_TENSOR_NCHW_VECT_C    =   2,  /* each image point is vector of element of C : the length of the vector is carried by the data type*/
};
typedef enum swdnnTensorFormatEnum swdnnTensorFormat_t;
/**/
/*
 *	SWDNN Tensor Descriptor
 */
struct swdnnTensorDescriptorStruct
{
    swdnnTensorFormat_t format;
    swdnnDataType_t dataType;
	unsigned long len;
    //4d tensor
    int n;
    int c;
    int h;
    int w;
    int nStride;
    int cStride;
    int hStride;
    int wStride;
    //Nd tensor
    int nbDims;
    int * dims;
    int * strides;
};
typedef struct swdnnTensorDescriptorStruct swdnnTensorDescriptor;
typedef struct swdnnTensorDescriptorStruct * swdnnTensorDescriptor_t;

/* Maximum supported number of tensor dimensions */
#define SWDNN_DIM_MAX 8

/* Create an instance of a generic Tensor descriptor */
swdnnStatus_t swdnnCreateTensorDescriptor(
					swdnnTensorDescriptor_t            *tensorDesc );
swdnnStatus_t swdnnSetTensor4dDescriptor(
					swdnnTensorDescriptor_t			tensorDesc,
					swdnnTensorFormat_t			format,
					swdnnDataType_t				dataType,	/* image data type*/
					int 					n,	 	/* number of inputs (batch size) */
					int 					c,		/* number of input feature maps */
					int 					h,		/* height of input section */
					int					w); 		/* width of input section */

swdnnStatus_t swdnnSetTensor4dDescriptorEx(
					swdnnTensorDescriptor_t			tensorDesc,
					swdnnDataType_t				dataType, /* image data type */
					int					n,	  /* number of inputs (batcch size) */
	                		int                                 	c,        /* number of input feature maps */
                    			int                                 	h,        /* height of input section */
                    			int                                 	w,        /* width of input section */
                    			int                                 	nStride,
                    			int                                 	cStride,
                    			int                                 	hStride,
                    			int                                 	wStride );

swdnnStatus_t swdnnGetTensor4dDescriptor(
                    const swdnnTensorDescriptor_t       tensorDesc,
                    swdnnDataType_t                    *dataType, /* image data type */
					unsigned long					   *len,
                    int                                *n,        /* number of inputs (batch size) */
                    int                                *c,        /* number of input feature maps  */
                    int                                *h,        /* height of input section */
                    int                                *w,        /* width of input section */
                    int                                *nStride,
                    int                                *cStride,
                    int                                *hStride,
                    int                                *wStride );

swdnnStatus_t swdnnSetTensorNdDescriptor(
                    swdnnTensorDescriptor_t             tensorDesc,
                    swdnnDataType_t                     dataType,
                    int                                 nbDims,
                    const int *                         dimA,
                    const int *                         strideA );

/*
swdnnStatus_t swdnnSetTensorNdDescriptorEx(								//Not mentioned in the user mannual
                    swdnnTensorDescriptor_t             tensorDesc,
                    swdnnTensorFormat_t                 format,
                    swdnnDataType_t                     dataType,
                    int                                 nbDims,
                    const int *                         dimA );
*/

swdnnStatus_t swdnnGetTensorNdDescriptor(
                    const swdnnTensorDescriptor_t       tensorDesc,
                    int                                 nbDimsRequested,
                    swdnnDataType_t                    *dataType,
					unsigned long					   *len,
                    int                                *nbDims,
                    int                                *dimA,
                    int                                *strideA );

swdnnStatus_t swdnnGetTensorSizeInBytes(
                    const swdnnTensorDescriptor_t       tensorDesc,
                    size_t                              *size);

/* PixelOffset( n, c, h, w ) = n *input_stride + c * feature_stride + h * h_stride + w * w_stride

   1)Example of all images in row major order one batch of features after the other (with an optional padding on row)
   input_stride :  c x h x h_stride
   feature_stride : h x h_stride
   h_stride  :  >= w  ( h_stride = w if no padding)
   w_stride  : 1


   2)Example of all images in row major with features maps interleaved
   input_stride :  c x h x h_stride
   feature_stride : 1
   h_stride  :  w x c
   w_stride  : c

   3)Example of all images in column major order one batch of features after the other (with optional padding on column)
   input_stride :  c x w x w_stride
   feature_stride : w x w_stride
   h_stride  :  1
   w_stride  :  >= h

*/

/* Destroy an instance of Tensor4d descriptor */
swdnnStatus_t swdnnDestroyTensorDescriptor(
                                swdnnTensorDescriptor_t             tensorDesc );

swdnnStatus_t swdnnTransformTensor(
				swdnnHandle_t			handle,
				const void	       	       *alpha,
				const swdnnTensorDescriptor_t	xDesc,
				const void		       *x,
				const void		       *beta,
				const swdnnTensorDescriptor_t	yDesc,
				void			       *y);

/***************************Furture Task 1***********************/
/* Tensor layout conversion helper (y = alpha * x + beta * y) */
/*
swdnnStatus_t swdnnTransformTensor(
                                swdnnHandle_t                       handle,
                                const void                         *alpha,
                                const swdnnTensorDescriptor_t       xDesc,
                                const void                         *x,
                                const void                         *beta,
                                const swdnnTensorDescriptor_t       yDesc,
                                void                               *y );
*/
/***************************Furture Task 1***********************/

/***************************Furture Task 2***********************/
/* Tensor Bias addition : C = alpha * A + beta * C  */
/*
swdnnStatus_t swdnnAddTensor(
                                swdnnHandle_t                       handle,
                                const void                         *alpha,
                                const swdnnTensorDescriptor_t       aDesc,
                                const void                         *A,
                                const void                         *beta,
                                const swdnnTensorDescriptor_t       cDesc,
                                void                               *C );
*/
/***************************Furture Task 2***********************/

/***************************Furture Task 3***********************/
/*
* SWDNN OpTensor op type
*/
/*
typedef enum
{
    SWDNN_OP_TENSOR_ADD  = 0,
    SWDNN_OP_TENSOR_MUL  = 1,
    SWDNN_OP_TENSOR_MIN  = 2,
    SWDNN_OP_TENSOR_MAX  = 3,
    SWDNN_OP_TENSOR_SQRT = 4,
    SWDNN_OP_TENSOR_NOT  = 5,
} swdnnOpTensorOp_t;

swdnnStatus_t swdnnCreateOpTensorDescriptor(
                    swdnnOpTensorDescriptor_t          *opTensorDesc );

swdnnStatus_t swdnnSetOpTensorDescriptor(
                    swdnnOpTensorDescriptor_t           opTensorDesc,
                    swdnnOpTensorOp_t                   opTensorOp,
                    swdnnDataType_t                     opTensorCompType,
                    swdnnNanPropagation_t               opTensorNanOpt );

swdnnStatus_t swdnnGetOpTensorDescriptor(
                    const swdnnOpTensorDescriptor_t     opTensorDesc,
                    swdnnOpTensorOp_t                  *opTensorOp,
                    swdnnDataType_t                    *opTensorCompType,
                    swdnnNanPropagation_t              *opTensorNanOpt );

swdnnStatus_t swdnnDestroyOpTensorDescriptor(
                    swdnnOpTensorDescriptor_t           opTensorDesc );
*/

/* Tensor operation : C = op( alpha1 * A, alpha2 * B ) + beta * C */
/* B tensor is ignored for CUDNN_OP_TENSOR_SQRT, CUDNN_OP_TENSOR_NOT. */
/*
swdnnStatus_t swdnnOpTensor(
                    swdnnHandle_t                       handle,
                    const swdnnOpTensorDescriptor_t     opTensorDesc,
                    const void                         *alpha1,
                    const swdnnTensorDescriptor_t       aDesc,
                    const void                         *A,
                    const void                         *alpha2,
                    const swdnnTensorDescriptor_t       bDesc,
                    const void                         *B,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       cDesc,
                    void                               *C );
*/
/***************************Furture Task 3***********************/

/***************************Furture Task 4***********************/
/*
* SWDNN ReduceTensor op type
*/
/*
typedef enum
{
    SWDNN_REDUCE_TENSOR_ADD          = 0,
    SWDNN_REDUCE_TENSOR_MUL          = 1,
    SWDNN_REDUCE_TENSOR_MIN          = 2,
    SWDNN_REDUCE_TENSOR_MAX          = 3,
    SWDNN_REDUCE_TENSOR_AMAX         = 4,
    SWDNN_REDUCE_TENSOR_AVG          = 5,
    SWDNN_REDUCE_TENSOR_NORM1        = 6,
    SWDNN_REDUCE_TENSOR_NORM2        = 7,
    SWDNN_REDUCE_TENSOR_MUL_NO_ZEROS = 8,
} swdnnReduceTensorOp_t;
*/
/*
* SWDNN ReduceTensor indices type
*/
/*
typedef enum
{
    SWDNN_REDUCE_TENSOR_NO_INDICES        = 0,
    SWDNN_REDUCE_TENSOR_FLATTENED_INDICES = 1,
} swdnnReduceTensorIndices_t;
*/
/*
* SWDNN tensor indices type size (all unsigned)
* Currently not supported, default is 32 bit unsigned.
*/
/*
typedef enum
{
    SWDNN_32BIT_INDICES = 0,
    SWDNN_64BIT_INDICES = 1,
    SWDNN_16BIT_INDICES = 2,
    SWDNN_8BIT_INDICES  = 3,
} swdnnIndicesType_t;

swdnnStatus_t CUDNNWINAPI swdnnCreateReduceTensorDescriptor(
                                swdnnReduceTensorDescriptor_t          *reduceTensorDesc );

swdnnStatus_t CUDNNWINAPI swdnnSetReduceTensorDescriptor(
                                swdnnReduceTensorDescriptor_t           reduceTensorDesc,
                                swdnnReduceTensorOp_t                   reduceTensorOp,
                                swdnnDataType_t                     reduceTensorCompType,
                                swdnnNanPropagation_t               reduceTensorNanOpt,
                                swdnnReduceTensorIndices_t          reduceTensorIndices,
                                swdnnIndicesType_t                  reduceTensorIndicesType );

swdnnStatus_t CUDNNWINAPI swdnnGetReduceTensorDescriptor(
                                const swdnnReduceTensorDescriptor_t     reduceTensorDesc,
                                swdnnReduceTensorOp_t                  *reduceTensorOp,
                                swdnnDataType_t                    *reduceTensorCompType,
                                swdnnNanPropagation_t              *reduceTensorNanOpt,
                                swdnnReduceTensorIndices_t         *reduceTensorIndices,
                                swdnnIndicesType_t                 *reduceTensorIndicesType );

swdnnStatus_t CUDNNWINAPI swdnnDestroyReduceTensorDescriptor(
                                swdnnReduceTensorDescriptor_t           reduceTensorDesc );
*/
 /* Helper function to return the minimum size of the index space to be passed to the reduction given the input and output tensors */
/*
swdnnStatus_t CUDNNWINAPI swdnnGetReductionIndicesSize(
                                swdnnHandle_t                       handle,
                                const swdnnReduceTensorDescriptor_t reduceTensorDesc,
                                const swdnnTensorDescriptor_t       aDesc,
                                const swdnnTensorDescriptor_t       cDesc,
                                size_t                             *sizeInBytes );
*/
 /* Helper function to return the minimum size of the workspace to be passed to the reduction given the input and output tensors */
/*
swdnnStatus_t CUDNNWINAPI swdnnGetReductionWorkspaceSize(
                                swdnnHandle_t                       handle,
                                const swdnnReduceTensorDescriptor_t reduceTensorDesc,
                                const swdnnTensorDescriptor_t       aDesc,
                                const swdnnTensorDescriptor_t       cDesc,
                                size_t                             *sizeInBytes );
*/
/* Tensor operation : C = reduce op( alpha * A ) + beta * C */
/* The NaN propagation enum applies to only the min and max reduce ops; the other reduce ops propagate NaN as usual. */
/* The indices space is ignored for reduce ops other than min or max. */
/*
swdnnStatus_t CUDNNWINAPI swdnnReduceTensor(
                                swdnnHandle_t                       handle,
                                const swdnnReduceTensorDescriptor_t reduceTensorDesc,
                                void                               *indices,
                                size_t                              indicesSizeInBytes,
                                void                               *workspace,
                                size_t                              workspaceSizeInBytes,
                                const void                         *alpha,
                                const swdnnTensorDescriptor_t       aDesc,
                                const void                         *A,
                                const void                         *beta,
                                const swdnnTensorDescriptor_t       cDesc,
                                void                               *C );
*/
/***************************Furture Task 4***********************/

/* Set all values of a tensor to a given value : y[i] = value[0] */
swdnnStatus_t swdnnSetTensor(
                    swdnnHandle_t                       handle,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y,
                    const void                         *valuePtr );

/* Scale all values of a tensor by a given factor : y[i] = alpha * y[i] */
swdnnStatus_t swdnnScaleTensor(
                    swdnnHandle_t                       handle,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y,
                    const void                         *alpha );

/***************************Furture Task 2***********************/
/* Tensor Bias addition : C = alpha * A + beta * C  */
swdnnStatus_t swdnnAddTensor(
                                swdnnHandle_t                       handle,
                                const void                         *alpha,
                                const swdnnTensorDescriptor_t       aDesc,
                                const void                         *A,
                                const void                         *beta,
                                const swdnnTensorDescriptor_t       cDesc,
                                void                               *C );

/*
 * SWDNN OpTensor op type
 */
enum swdnnOpTensorOpEnum
{
	SWDNN_OP_TENSOR_ADD = 0,
	SWDNN_OP_TENSOR_MUL = 1,
	SWDNN_OP_TENSOR_MIN = 2,
	SWDNN_OP_TENSOR_MAX = 3,
	SWDNN_OP_TENSOR_SQRT = 4,
	SWDNN_OP_TENSOR_NOT = 5,
};
typedef enum swdnnOpTensorOpEnum swdnnOpTensorOp_t;

struct swdnnOpTensorDescriptorStruct
{
	swdnnOpTensorOp_t 	opTensorOp;
	swdnnDataType_t		opTensorCompType;
	swdnnNanPropagation_t	opTensorNanOpt;
};
typedef struct swdnnOpTensorDescriptorStruct swdnnOpTensorDescriptor;
typedef struct swdnnOpTensorDescriptorStruct * swdnnOpTensorDescriptor_t;

swdnnStatus_t swdnnCreateOpTensorDescriptor(
	swdnnOpTensorDescriptor_t      *opTensorDesc);

swdnnStatus_t swdnnSetOpTensorDescriptor(
	swdnnOpTensorDescriptor_t	opTensorDesc,
	swdnnOpTensorOp_t		opTensorOp,
	swdnnDataType_t			opTensorCompType,
	swdnnNanPropagation_t		opTensorNanOpt);

swdnnStatus_t swdnnGetOpTensorDescriptor(
	const swdnnOpTensorDescriptor_t opTensorDesc,
	swdnnOpTensorOp_t	       *opTensorOp,
	swdnnDataType_t		       *opTensorCompType,
	swdnnNanPropagation_t	       *opTensorNanOpt);

swdnnStatus_t swdnnDestroyOpTensorDescriptor(
	swdnnOpTensorDescriptor_t	opTensorDesc);

/* Tensor operation : C = op( alpha1 * A, alpha2 * B ) + beta * C */
/* B tensor is ignored for CUDNN_OP_TENSOR_SQRT, CUDNN_OP_TENSOR_NOT. */
swdnnStatus_t swdnnOpTensor(
	swdnnHandle_t			handle,
	const swdnnOpTensorDescriptor_t	opTensorDesc,
	const void		       *alpha1,
	const swdnnTensorDescriptor_t	aDesc,
	const void		       *A,
	const void		       *alpha2,
	const swdnnTensorDescriptor_t	bDesc,
	const void		       *B,
	const void		       *beta,
	const swdnnTensorDescriptor_t	cDesc,
	void			       *C);

/*
 *	SWDNN Filter Descriptor
 */
struct swdnnFilterDescriptorStruct
{
    swdnnTensorFormat_t format;
    swdnnDataType_t dataType;
    //4d filter
    int k;
    int c;
    int h;
    int w;
    //Nd filter
    int nbDims;
    int * filterDimA;
};
typedef struct swdnnFilterDescriptorStruct swdnnFilterDescriptor;
typedef struct swdnnFilterDescriptorStruct * swdnnFilterDescriptor_t;

/* Create an instance of FilterStruct */
swdnnStatus_t swdnnCreateFilterDescriptor(
                    swdnnFilterDescriptor_t            *filterDesc );


swdnnStatus_t swdnnSetFilter4dDescriptor(
                    swdnnFilterDescriptor_t             filterDesc,
                    swdnnDataType_t                     dataType, /* image data type */
                    swdnnTensorFormat_t                 format,
                    int                                 k,        /* number of output feature maps */
                    int                                 c,        /* number of input feature maps */
                    int                                 h,        /* height of each input filter */
                    int                                 w );      /* width of  each input filter */


swdnnStatus_t swdnnGetFilter4dDescriptor(
                    const swdnnFilterDescriptor_t       filterDesc,
                    swdnnDataType_t                    *dataType, /* image data type */
                    swdnnTensorFormat_t                *format,
                    int                                *k,        /* number of output feature maps */
                    int                                *c,        /* number of input feature maps */
                    int                                *h,        /* height of each input filter */
                    int                                *w );      /* width of  each input filter */


swdnnStatus_t swdnnSetFilterNdDescriptor(
                    swdnnFilterDescriptor_t             filterDesc,
                    swdnnDataType_t                     dataType, /* image data type */
                    swdnnTensorFormat_t                 format,
                    int                                 nbDims,
                    const int                          *filterDimA );

swdnnStatus_t swdnnGetFilterNdDescriptor(
                    const swdnnFilterDescriptor_t       filterDesc,
                    int                                 nbDimsRequested,
                    swdnnDataType_t                    *dataType, /* image data type */
                    swdnnTensorFormat_t                *format,
                    int                                *nbDims,
                    int                                *filterDimA );


swdnnStatus_t swdnnDestroyFilterDescriptor(
                    swdnnFilterDescriptor_t             filterDesc );


/*
 *	activation mode
 */
enum swdnnActivationModeEnum
{
    SWDNN_ACTIVATION_SIGMOID    	=   0,
    SWDNN_ACTIVATION_RELU   		=   1,
    SWDNN_ACTIVATION_TANH   		=   2,
    SWDNN_ACTIVATION_CLIPED_RELU    =   3,
    SWDNN_ACTIVATION_ELU    		=   4,
    SWDNN_ACTIVATION_GELU		=   5,
    SWDNN_ACTIVATION_SILU   		=   6,
};
typedef enum swdnnActivationModeEnum swdnnActivationMode_t;

/*
 *	SWDNN Activation Descriptor
 */
struct swdnnActivationDescriptorStruct
{
    swdnnActivationMode_t mode;
    swdnnNanPropagation_t    reluNanOpt;
    double coef;
};
typedef struct swdnnActivationDescriptorStruct swdnnActivationDescriptor;
typedef struct swdnnActivationDescriptorStruct * swdnnActivationDescriptor_t;

/* Activation functions: All of the form "output = alpha * Op(inputs) + beta * output" */
swdnnStatus_t swdnnCreateActivationDescriptor(
                    swdnnActivationDescriptor_t        *activationDesc);

swdnnStatus_t swdnnSetActivationDescriptor(
                    swdnnActivationDescriptor_t         activationDesc,
                    swdnnActivationMode_t               mode,
                    swdnnNanPropagation_t               reluNanOpt,
                    double                              coef ); /* ceiling for clipped RELU, alpha for ELU */

swdnnStatus_t swdnnGetActivationDescriptor(
                    const swdnnActivationDescriptor_t   activationDesc,
                    swdnnActivationMode_t              *mode,
                    swdnnNanPropagation_t              *reluNanOpt,
                    double*                             coef ); /* ceiling for clipped RELU, alpha for ELU */

swdnnStatus_t swdnnDestroyActivationDescriptor(
                    swdnnActivationDescriptor_t activationDesc);

/* Function to perform forward activation  */
swdnnStatus_t swdnnActivationForward(
                    swdnnHandle_t                       handle,
                    swdnnActivationDescriptor_t         activationDesc,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y );

/* Function to perform backward activation  */
swdnnStatus_t swdnnActivationBackward(
                    swdnnHandle_t                       handle,
                    swdnnActivationDescriptor_t         activationDesc,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       yDesc,
                    const void                         *y,
                    const swdnnTensorDescriptor_t       dyDesc,
                    const void                         *dy,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       dxDesc,
                    void                               *dx );
/*
 *	pooling mode
 */
enum swdnnPoolingModeEnum
{
    SWDNN_POOLING_MAX   =   0,
    SWDNN_POOLING_AVG   =   1,
};
typedef enum swdnnPoolingModeEnum swdnnPoolingMode_t;

/*
 *	pooling descriptor
 */
struct swdnnPoolingDescriptorStruct
{
    int nbDimsRequested;
    swdnnPoolingMode_t mode;
    swdnnNanPropagation_t maxpoolingNanOpt;
    //params for 2D pooling
    int windowHeight;
    int windowWidth;
    int verticalPadding;
    int horizontalPadding;
    int verticalStride;
    int horizontalStride;
    //params for Nd pooling
    int nbDims;
    int * windowDimA;
    int * paddingA;
    int * strideA;
};
typedef struct swdnnPoolingDescriptorStruct swdnnPoolingDescriptor;
typedef struct swdnnPoolingDescriptorStruct * swdnnPoolingDescriptor_t;

/* Create an instance of pooling descriptor */
swdnnStatus_t swdnnCreatePoolingDescriptor(
                    swdnnPoolingDescriptor_t           *poolingDesc );

swdnnStatus_t swdnnSetPooling2dDescriptor(
                    swdnnPoolingDescriptor_t            poolingDesc,
                    swdnnPoolingMode_t                  mode,
                    swdnnNanPropagation_t               maxpoolingNanOpt,
                    int                                 windowHeight,
                    int                                 windowWidth,
                    int                                 verticalPadding,
                    int                                 horizontalPadding,
                    int                                 verticalStride,
                    int                                 horizontalStride );

swdnnStatus_t swdnnGetPooling2dDescriptor(
                    const swdnnPoolingDescriptor_t      poolingDesc,
                    swdnnPoolingMode_t                 *mode,
                    swdnnNanPropagation_t              *maxpoolingNanOpt,
                    int                                *windowHeight,
                    int                                *windowWidth,
                    int                                *verticalPadding,
                    int                                *horizontalPadding,
                    int                                *verticalStride,
                    int                                *horizontalStride );

swdnnStatus_t swdnnSetPoolingNdDescriptor(
                    swdnnPoolingDescriptor_t            poolingDesc,
                    const swdnnPoolingMode_t            mode,
                    const swdnnNanPropagation_t         maxpoolingNanOpt,
                    int                                 nbDims,
                    const int                          *windowDimA,
                    const int                          *paddingA,
                    const int                          *strideA );

swdnnStatus_t swdnnGetPoolingNdDescriptor(
                    const swdnnPoolingDescriptor_t      poolingDesc,
                    int                                 nbDimsRequested,
                    swdnnPoolingMode_t                 *mode,
                    swdnnNanPropagation_t              *maxpoolingNanOpt,
                    int                                *nbDims,
                    int                                *windowDimA,
                    int                                *paddingA,
                    int                                *strideA );

swdnnStatus_t swdnnGetPoolingNdForwardOutputDim(
                    const swdnnPoolingDescriptor_t      poolingDesc,
                    const swdnnTensorDescriptor_t       inputTensorDesc,
                    int                                 nbDims,
                    int                                *outputTensorDimA );

swdnnStatus_t swdnnGetPooling2dForwardOutputDim(
                    const swdnnPoolingDescriptor_t      poolingDesc,
                    const swdnnTensorDescriptor_t       inputTensorDesc,
                    int                                *n,
                    int                                *c,
                    int                                *h,
                    int                                *w );

/* Destroy an instance of pooling descriptor */
swdnnStatus_t swdnnDestroyPoolingDescriptor(
                    swdnnPoolingDescriptor_t            poolingDesc );

/* Pooling functions: All of the form "output = alpha * Op(inputs) + beta * output" */

/* Function to perform forward pooling */
swdnnStatus_t swdnnPoolingForward(
                    swdnnHandle_t                       handle,
                    const swdnnPoolingDescriptor_t      poolingDesc,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y );

/* Function to perform backward pooling */
swdnnStatus_t swdnnPoolingBackward(
                    swdnnHandle_t                       handle,
                    const swdnnPoolingDescriptor_t      poolingDesc,
                    const void                          *alpha,
                    const swdnnTensorDescriptor_t       yDesc,
                    const void                         *y,
                    const swdnnTensorDescriptor_t       dyDesc,
                    const void                         *dy,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       dxDesc,
                    void                               *dx );


/*
 *	convolution mode
 */
enum swdnnConvolutionModeEnum
{
    SWDNN_CONVOLUTION   =   0,
    SWDNN_CROSS_CORRELATION =   1,
};
typedef enum swdnnConvolutionModeEnum swdnnConvolutionMode_t;

/*
 *	convolution forward algo
 */
enum swdnnConvolutionFwdAlgoEnum
{
    SWDNN_CONVOLUTION_FWD_ALGO_IM2COL    =   0,
};
typedef enum swdnnConvolutionFwdAlgoEnum swdnnConvolutionFwdAlgo_t;

/*
 *	convolution backward filter algo
 */
enum swdnnConvolutionBwdFilterAlgoEnum
{
    SWDNN_CONVOLUTION_BWD_FILTER_ALGO_IM2COL    =   0,
};
typedef enum swdnnConvolutionBwdFilterAlgoEnum swdnnConvolutionBwdFilterAlgo_t;

/*
 *	convolution backward data algo
 */
enum swdnnConvolutionBwdDataAlgoEnum
{
    SWDNN_CONVOLUTION_BWD_DATA_ALGO_IM2COL  =   0,
};
typedef enum swdnnConvolutionBwdDataAlgoEnum swdnnConvolutionBwdDataAlgo_t;

/*
 *	SWDNN Convolution descriptor
 */
struct swdnnConvolutionDescriptorStruct
{
    swdnnConvolutionMode_t mode;
    swdnnDataType_t computeType;
	int groupCount;			//Option for group convolution
    //2D params
    int pad_h;      		//zero-padding height
    int pad_w;     			//zero-padding width
    int u;           		//vertical filter stride
    int v;           		//horizontal filter stride
    int dilation_h;			//filter height dilation
    int dilation_w;			//filter width dilation
    //Nd params
    int arrayLength;    	//Dimension of the convolution
    int * padA;          	//zero-padding size for each dimension
    int * filterStrideA; 	//filter stride for each dimension
    int * dilationA;     	//dilation factor for each dimension
};
typedef struct swdnnConvolutionDescriptorStruct swdnnConvolutionDescriptor;
typedef struct swdnnConvolutionDescriptorStruct * swdnnConvolutionDescriptor_t;

/* Create an instance of convolution descriptor */
swdnnStatus_t swdnnCreateConvolutionDescriptor(
                    swdnnConvolutionDescriptor_t       *convDesc );

/* set group convolution param */
swdnnStatus_t swdnnSetConvolutionGroupCount(
					swdnnConvolutionDescriptor_t	   convDesc,
					int									groupCount);

/* get group convolution param */
swdnnStatus_t swdnnGetConvolutionGroupCount(
					swdnnConvolutionDescriptor_t	   convDesc,
					int								   *groupCount);

swdnnStatus_t swdnnSetConvolution2dDescriptor( swdnnConvolutionDescriptor_t convDesc,
                                                 int pad_h,    /* zero-padding height */
                                                 int pad_w,    /* zero-padding width */
                                                 int u,   /* vertical filter stride */
                                                 int v,   /* horizontal filter stride */
                                                 int dilation_h, /* filter dilation in the vertical dimension */
                                                 int dilation_w, /* filter dilation in the horizontal dimension */
                                                 swdnnConvolutionMode_t mode,
                                                 swdnnDataType_t computeType
                                               );

swdnnStatus_t swdnnGetConvolution2dDescriptor(  const swdnnConvolutionDescriptor_t convDesc,
                                                int* pad_h,    /* zero-padding height */
                                                int* pad_w,    /* zero-padding width */
                                                int* u,        /* vertical filter stride */
                                                int* v,        /* horizontal filter stride */
                                                int* dilation_h, /* filter dilation in the vertical dimension */
                                                int* dilation_w, /* filter dilation in the horizontal dimension */
                                                swdnnConvolutionMode_t* mode,
                                                swdnnDataType_t *computeType
                                             );

/* Helper function to return the dimensions of the output tensor given a convolution descriptor */
swdnnStatus_t swdnnGetConvolution2dForwardOutputDim(
                    const swdnnConvolutionDescriptor_t  convDesc,
                    const swdnnTensorDescriptor_t       inputTensorDesc,
                    const swdnnFilterDescriptor_t       filterDesc,
                    int                                *n,
                    int                                *c,
                    int                                *h,
                    int                                *w );

swdnnStatus_t swdnnSetConvolutionNdDescriptor(
                    swdnnConvolutionDescriptor_t        convDesc,
                    int                                 arrayLength,             /* nbDims-2 size */
                    const int                          *padA,
                    const int                          *filterStrideA,
                    const int                          *dilationA,
                    swdnnConvolutionMode_t              mode,
                    swdnnDataType_t                     computeType );  /* convolution data type */

swdnnStatus_t swdnnGetConvolutionNdDescriptor(
                    const swdnnConvolutionDescriptor_t  convDesc,
                    int                                 arrayLengthRequested,
                    int                                *arrayLength,
                    int                                *padA,
                    int                                *strideA,
                    int                                *dilationA,
                    swdnnConvolutionMode_t             *mode,
                    swdnnDataType_t                    *computeType );   /* convolution data type */

/* Helper function to return the dimensions of the output tensor given a convolution descriptor */
swdnnStatus_t swdnnGetConvolutionNdForwardOutputDim(
                    const swdnnConvolutionDescriptor_t  convDesc,
                    const swdnnTensorDescriptor_t       inputTensorDesc,
                    const swdnnFilterDescriptor_t       filterDesc,
                    int                                 nbDims,
                    int                                *tensorOuputDimA );

/* Destroy an instance of convolution descriptor */
swdnnStatus_t swdnnDestroyConvolutionDescriptor(
                    swdnnConvolutionDescriptor_t        convDesc );

/* Convolution functions: All of the form "output = alpha * Op(inputs) + beta * output" */

/* Function to perform the forward pass for batch convolution */
swdnnStatus_t swdnnConvolutionForward(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const swdnnFilterDescriptor_t       wDesc,
                    const void                         *w,
                    const swdnnConvolutionDescriptor_t  convDesc,
                    swdnnConvolutionFwdAlgo_t           algo,
                    //void                               *workSpace,
                    //size_t                              workSpaceSizeInBytes,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y );

swdnnStatus_t swdnnConvolutionForward_malloc(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const swdnnFilterDescriptor_t       wDesc,
                    const void                         *w,
                    const swdnnConvolutionDescriptor_t  convDesc,
                    swdnnConvolutionFwdAlgo_t           algo,
                    //void                               *workSpace,
                    //size_t                              workSpaceSizeInBytes,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y, 
                    void                               *im2col_malloc_ptr,
                    const int64_t                      im2col_malloc_size,
                    void                               *gemm_malloc_ptr,
                    const int64_t                      gemm_malloc_size );


/* Fused conv/bias operation : y = alpha * (conv(x) + bias) + beta * y*/
swdnnStatus_t swdnnConvolutionBiasForward(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const swdnnFilterDescriptor_t       wDesc,
                    const void                         *w,
                    const swdnnTensorDescriptor_t       biasDesc,
                    const void                         *bias,

                    const swdnnConvolutionDescriptor_t  convDesc,
                    swdnnConvolutionFwdAlgo_t           algo,
                    //void                               *workSpace,
                    //size_t                              workSpaceSizeInBytes,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y );

swdnnStatus_t swdnnConvolutionBiasForward_malloc(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const swdnnFilterDescriptor_t       wDesc,
                    const void                         *w,
                    const swdnnTensorDescriptor_t       biasDesc,
                    const void                         *bias,

                    const swdnnConvolutionDescriptor_t  convDesc,
                    swdnnConvolutionFwdAlgo_t           algo,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y,
                    void                               *im2col_malloc_ptr,
                    const int64_t                      im2col_malloc_size,
                    void                               *gemm_malloc_ptr,
                    const int64_t                      gemm_malloc_size);

/* Function to compute the bias gradient for batch convolution */
swdnnStatus_t swdnnConvolutionBackwardBias(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       dyDesc,
                    const void                         *dy,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       dbDesc,
                    void                               *db );

/* Function to compute the filter gradient for batch convolution */
swdnnStatus_t swdnnConvolutionBackwardFilter(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const swdnnTensorDescriptor_t       dyDesc,
                    const void                         *dy,
                    const swdnnConvolutionDescriptor_t  convDesc,
                    swdnnConvolutionBwdFilterAlgo_t     algo,
                    //void                               *workSpace,
                    //size_t                              workSpaceSizeInBytes,
                    const void                         *beta,
                    const swdnnFilterDescriptor_t       dwDesc,
                    void                               *dw );

swdnnStatus_t swdnnConvolutionBackwardFilter_malloc(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const swdnnTensorDescriptor_t       dyDesc,
                    const void                         *dy,
                    const swdnnConvolutionDescriptor_t  convDesc,
                    swdnnConvolutionBwdFilterAlgo_t     algo,
                    //void                               *workSpace,
                    //size_t                              workSpaceSizeInBytes,
                    const void                         *beta,
                    const swdnnFilterDescriptor_t       dwDesc,
                    void                               *dw ,
                    void                               *im2col_malloc_ptr,
                    const int64_t                      im2col_malloc_size,
                    void                               *gemm_malloc_ptr,
                    const int64_t                      gemm_malloc_size );




/* Function to compute the data gradient for batch convolution */
swdnnStatus_t swdnnConvolutionBackwardData(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnFilterDescriptor_t       wDesc,
                    const void                         *w,
                    const swdnnTensorDescriptor_t       dyDesc,
                    const void                         *dy,
                    const swdnnConvolutionDescriptor_t  convDesc,
                    swdnnConvolutionBwdDataAlgo_t       algo,
                    //void                               *workSpace,
                    //size_t                              workSpaceSizeInBytes,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       dxDesc,
                    void                               *dx );

swdnnStatus_t swdnnConvolutionBackwardData_malloc(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnFilterDescriptor_t       wDesc,
                    const void                         *w,
                    const swdnnTensorDescriptor_t       dyDesc,
                    const void                         *dy,
                    const swdnnConvolutionDescriptor_t  convDesc,
                    swdnnConvolutionBwdDataAlgo_t       algo,
                    //void                               *workSpace,
                    //size_t                              workSpaceSizeInBytes,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       dxDesc,
                    void                               *dx ,
                    void                               *col2im_malloc_ptr,
                    const int64_t                      col2im_malloc_size,
                    void                               *gemm_malloc_ptr,
                    const int64_t                      gemm_malloc_size );


swdnnStatus_t swdnnIm2Col(
                    swdnnHandle_t                       handle,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const swdnnFilterDescriptor_t       wDesc,
                    const swdnnConvolutionDescriptor_t  convDesc,
                    void                               *colBuffer );

/*
 *	SWDNN Deconvolution descriptor
 */
struct swdnnDeconvolutionDescriptorStruct
{
    swdnnConvolutionMode_t mode;
    swdnnDataType_t computeType;
	int groupCount;			//Option for group convolution
    //2D params
    int pad_h;      		//zero-padding height
    int pad_w;     			//zero-padding width
    int u;           		//vertical filter stride
    int v;           		//horizontal filter stride
    int output_pad_h;      		//zero-padding output height
    int output_pad_w;     			//zero-padding output width
    int dilation_h;			//filter height dilation
    int dilation_w;			//filter width dilation
    //Nd params
    int arrayLength;    	//Dimension of the convolution
    int * padA;          	//zero-padding size for each dimension
    int * filterStrideA; 	//filter stride for each dimension
    int * dilationA;     	//dilation factor for each dimension
};
typedef struct swdnnDeconvolutionDescriptorStruct swdnnDeconvolutionDescriptor;
typedef struct swdnnDeconvolutionDescriptorStruct * swdnnDeconvolutionDescriptor_t;

/* Create an instance of deconvolution descriptor */
swdnnStatus_t swdnnCreateDeconvolutionDescriptor(
                    swdnnDeconvolutionDescriptor_t       *deconvDesc );

/* set group deconvolution param */
swdnnStatus_t swdnnSetDeconvolutionGroupCount(
					swdnnDeconvolutionDescriptor_t	   deconvDesc,
					int									groupCount);

/* get group deconvolution param */
swdnnStatus_t swdnnGetDeconvolutionGroupCount(
					swdnnDeconvolutionDescriptor_t	   deconvDesc,
					int								   *groupCount);

swdnnStatus_t swdnnSetDeconvolution2dDescriptor( swdnnDeconvolutionDescriptor_t deconvDesc,
                                                 int pad_h,    /* zero-padding height */
                                                 int pad_w,    /* zero-padding width */
                                                 int u,   /* vertical filter stride */
                                                 int v,   /* horizontal filter stride */
                                            	 int output_pad_h,    /*zero-padding output height*/
                                            	 int output_pad_w,    /*zero-padding output width*/
                                                 int dilation_h, /* filter dilation in the vertical dimension */
                                                 int dilation_w, /* filter dilation in the horizontal dimension */
                                                 swdnnConvolutionMode_t mode,
                                                 swdnnDataType_t computeType
                                               );

swdnnStatus_t swdnnGetDeconvolution2dDescriptor(  const swdnnDeconvolutionDescriptor_t deconvDesc,
                                                int* pad_h,    /* zero-padding height */
                                                int* pad_w,    /* zero-padding width */
                                                int* u,        /* vertical filter stride */
                                                int* v,        /* horizontal filter stride */
                                            	int* output_pad_h,    /*zero-padding output height*/
                                            	int* output_pad_w,    /*zero-padding output width*/
                                                int* dilation_h, /* filter dilation in the vertical dimension */
                                                int* dilation_w, /* filter dilation in the horizontal dimension */
                                                swdnnConvolutionMode_t* mode,
                                                swdnnDataType_t *computeType
                                             );

/* Helper function to return the dimensions of the output tensor given a deconvolution descriptor */
swdnnStatus_t swdnnGetDeconvolution2dForwardOutputDim(
                    const swdnnDeconvolutionDescriptor_t  deconvDesc,
                    const swdnnTensorDescriptor_t       inputTensorDesc,
                    const swdnnFilterDescriptor_t       filterDesc,
                    int                                *n,
                    int                                *c,
                    int                                *h,
                    int                                *w );
/* Function to perform the forward pass for batch deconvolution */
swdnnStatus_t swdnnDeconvolutionForward(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const swdnnFilterDescriptor_t       wDesc,
                    const void                         *w,
                    const swdnnDeconvolutionDescriptor_t  deconvDesc,
                    swdnnConvolutionFwdAlgo_t           algo,
                    //void                               *workSpace,
                    //size_t                              workSpaceSizeInBytes,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y );

swdnnStatus_t swdnnDeconvolutionForward_malloc(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const swdnnFilterDescriptor_t       wDesc,
                    const void                         *w,
                    const swdnnDeconvolutionDescriptor_t  deconvDesc,
                    swdnnConvolutionFwdAlgo_t           algo,
                    //void                               *workSpace,
                    //size_t                              workSpaceSizeInBytes,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y,
                    void                               *col2im_malloc_ptr,
                    const int64_t                      col2im_malloc_size,
                    void                               *gemm_malloc_ptr,
                    const int64_t                      gemm_malloc_size);


/* Destroy an instance of deconvolution descriptor */
swdnnStatus_t swdnnDestroyDeconvolutionDescriptor(
                    swdnnDeconvolutionDescriptor_t        deconvDesc );

/* Fused conv/bias operation : y = alpha * (conv(x) + bias) + beta * y*/
swdnnStatus_t swdnnDeconvolutionBiasForward(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const swdnnFilterDescriptor_t       wDesc,
                    const void                         *w,
                    const swdnnTensorDescriptor_t       biasDesc,
                    const void                         *bias,

                    const swdnnDeconvolutionDescriptor_t  deconvDesc,
                    swdnnConvolutionFwdAlgo_t           algo,
                    //void                               *workSpace,
                    //size_t                              workSpaceSizeInBytes,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y );

swdnnStatus_t swdnnDeconvolutionBiasForward_malloc(
                    swdnnHandle_t                       handle,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const swdnnFilterDescriptor_t       wDesc,
                    const void                         *w,
                    const swdnnTensorDescriptor_t       biasDesc,
                    const void                         *bias,

                    const swdnnDeconvolutionDescriptor_t  deconvDesc,
                    swdnnConvolutionFwdAlgo_t           algo,
                    //void                               *workSpace,
                    //size_t                              workSpaceSizeInBytes,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y,
                    void                               *col2im_malloc_ptr,
                    const int64_t                      col2im_malloc_size,
                    void                               *gemm_malloc_ptr,
                    const int64_t                      gemm_malloc_size);

/*
 *	SWDNN LRN descriptor
 */
struct swdnnLRNDescriptorStruct
{
    int lrnN;
    float lrnAlpha;
    float lrnBeta;
    float lrnK;
};
typedef struct swdnnLRNDescriptorStruct swdnnLRNDescriptor;
typedef struct swdnnLRNDescriptorStruct * swdnnLRNDescriptor_t;

/* 
 * Create an instance of LRN (Local Response Normalization) descriptor
 * Uses lrnN=5, lrnAlpha=1e-4, lrnBeta=0.75, lrnK=2.0 as defaults from Krizhevsky'12 ImageNet paper
 */
swdnnStatus_t swdnnCreateLRNDescriptor(
		    swdnnLRNDescriptor_t               *normDesc);

#define SWDNN_LRN_MIN_N     1       /* minimum allowed lrnN */
#define SWDNN_LRN_MAX_N     16      /* maximum allowed lrnN */
#define SWDNN_LRN_MIN_K     1e-5    /* minimum allowed lrnK */
#define SWDNN_LRN_MIN_BETA  0.01    /* minimum allowed lrnBeta */

/* LRN layer mode */
enum swdnnLRNModeEnum
{
    SWDNN_LRN_CROSS_CHANNEL_DIM1 = 1001, /* Normalize across tensor's dimA[1] dimension */
};
typedef enum swdnnLRNModeEnum swdnnLRNMode_t;

/*
 * Uses a window [center-lookBehind, center+lookAhead], where
 * lookBehind = (lrnN-1)/2 , lookAhead = lrnN-lookBehind-1.
 */
swdnnStatus_t swdnnSetLRNDescriptor(
		    swdnnLRNDescriptor_t                normDesc,
		    int					lrnN,
		    float				lrnAlpha,
		    float				lrnBeta,
		    float				lrnK);

/*
 * Retrieve the settings currently stored in an LRN layer descriptor
 * Any of the provided pointers can be NULL (no corresponding value will be returned)
 */
swdnnStatus_t swdnnGetLRNDescriptor(
		    swdnnLRNDescriptor_t		normDesc,
		    int				       *lrnN,
		    float			       *lrnAlpha,
		    float			       *lrnBeta,
		    float			       *lrnK);

/* Destroy an instance of LRN descriptor */
swdnnStatus_t swdnnDestroyLRNDescriptor(
		    swdnnLRNDescriptor_t lrnDesc);

/* LRN functions: output = alpha * normalize(x) + beta * old_y */

/* LRN cross-channel forward computation. */
swdnnStatus_t swdnnLRNCrossChannelForward(
		    swdnnHandle_t                       handle, 
		    swdnnLRNDescriptor_t                normDesc, 
		    swdnnLRNMode_t                      lrnMode, 
		    const void                         *alpha, 
		    const swdnnTensorDescriptor_t       xDesc, 
		    const void                         *x, 
		    void			       *scale, 
		    const void                         *beta, 
		    const swdnnTensorDescriptor_t       yDesc, 
		    void                               *y);

/* LRN cross-channel backward computation. */
swdnnStatus_t swdnnLRNCrossChannelBackward(
		    swdnnHandle_t                       handle,
                    swdnnLRNDescriptor_t                normDesc,
                    swdnnLRNMode_t                      lrnMode,
                    const void*                         alpha,
                    const swdnnTensorDescriptor_t       yDesc,
                    const void                         *y,
                    const swdnnTensorDescriptor_t       dyDesc,
                    const void                         *dy,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
		    const void                         *scale,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       dxDesc,
                    void                               *dx);

/*
 *	softmax algorithm
 */
enum swdnnSoftmaxModeEnum
{
    SWDNN_SOFTMAX_MODE_CHANNEL  =   0, //across C
    SWDNN_SOFTMAX_MODE_INSTANCE =   1, //across C,H,W
};
typedef enum swdnnSoftmaxModeEnum swdnnSoftmaxMode_t;

enum swdnnSoftmaxAlgorithmEnum
{
    SWDNN_SOFTMAX_FAST  =   0,  //straightforward softmax
    SWDNN_SOFTMAX_ACCURATE  =   1,  //scale to avoid potential floating point overflow
    SWDNN_SOFTMAX_LOG   =   2,  //Log softmax
};
typedef enum swdnnSoftmaxAlgorithmEnum swdnnSoftmaxAlgorithm_t;

/* Softmax functions: All of the form "output = alpha * Op(inputs) + beta * output" */

/* Function to perform forward softmax */
swdnnStatus_t swdnnSoftmaxForward(
                    swdnnHandle_t                       handle,
                    swdnnSoftmaxAlgorithm_t             algo,
                    swdnnSoftmaxMode_t                  mode,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y );

/* Function to perform backward softmax */
swdnnStatus_t swdnnSoftmaxBackward(
                    swdnnHandle_t                       handle,
                    swdnnSoftmaxAlgorithm_t             algo,
                    swdnnSoftmaxMode_t                  mode,
                    const void                         *alpha,
                    const swdnnTensorDescriptor_t       yDesc,
                    const void                         *y,
                    const swdnnTensorDescriptor_t       dyDesc,
                    const void                         *dy,
                    const void                         *beta,
                    const swdnnTensorDescriptor_t       dxDesc,
                    void                               *dx );

/*
 *	BatchNorm algorithm
 */
enum swdnnBatchNormModeEnum
{
	/* bnScale, 
 	 * bnBias tensor dims are 1xCxHxWx..
 	 * (one value per CHW...-slice, 
 	 * normalized over N slice) */
    SWDNN_BATCHNORM_PRE_ACTIVATION  =   0,
	/* bnScale, 
 	 * bnBias tensor dims are 1xCx1x1 
 	 * (one value per C-dim,
 	 * normalized over Nx1xHxW subtensors) */
    SWDNN_BATCHNORM_SPATIAL =   1,
};
typedef enum swdnnBatchNormModeEnum swdnnBatchNormMode_t;

#define SWDNN_BN_MIN_EPSILON 1e-5 /* Minimum epsilon allowed to be used in the Batch Normalization formula */

/*
 * * Performs Batch Normalization during Inference: 
 * * y[i] = bnScale[k]*(x[i]-estimatedMean[k])/sqrt(epsilon+estimatedVariance[k]) + bnBias[k]
 * * with bnScale, bnBias, runningMean, runningInvVariance tensors indexed
 * * according to spatial or per-activation mode. Refer to swdnnBatchNormalizationForwardTraining
 * * above for notes on function arguments.
 * */
swdnnStatus_t swdnnBatchNormalizationForwardInference(
                    swdnnHandle_t                       handle,
                    swdnnBatchNormMode_t                mode,
                    const void                         *alpha, /* alpha[0] = result blend factor */
                    const void                         *beta,  /* beta[0] = dest layer blend factor */
                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,     /* NxCxHxW */
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y,     /* NxCxHxW */
                    const swdnnTensorDescriptor_t       bnScaleBiasMeanVarDesc,
                    const void                         *bnScale,
                    const void                         *bnBias,
                    const void                         *estimatedMean,
                    const void                         *estimatedVariance,
                    double                              epsilon );

/* Computes y = BN(x). Also accumulates moving averages of mean and inverse variances */
swdnnStatus_t swdnnBatchNormalizationForwardTraining(
                    swdnnHandle_t                       handle,
                    swdnnBatchNormMode_t                mode,

                    const void                         *alpha, /* alpha[0] = result blend factor */
                    const void                         *beta,  /* beta[0] = dest layer blend factor */

                    const swdnnTensorDescriptor_t       xDesc,
                    const void                         *x,     /* NxCxHxW */
                    const swdnnTensorDescriptor_t       yDesc,
                    void                               *y,     /* NxCxHxW */

                    /* Shared desc for the next 6 tensors in the argument list.
                       Data type to be set as follows:
                       type = (typeOf(x) == double) ? double : float
                       Dimensions for this descriptor depend on normalization mode
                       - Spatial Normalization : tensors are expected to have dims 1xCx1x1
                        (normalization is performed across NxHxW)
                       - Per-Activation Normalization : tensors are expected to have dims of 1xCxHxW 
                        (normalization is performed across N) */
                    const swdnnTensorDescriptor_t       bnScaleBiasMeanVarDesc,

                    /* 'Gamma' and 'Beta' respectively in Ioffe and Szegedy's paper's notation */
                    const void                         *bnScale,
                    const void                         *bnBias,

                    /* MUST use factor=1 in the very first call of a complete training cycle.
                       Use a factor=1/(1+n) at N-th call to the function to get
                       Cumulative Moving Average (CMA) behavior
                       CMA[n] = (x[1]+...+x[n])/n
                       Since CMA[n+1] = (n*CMA[n]+x[n+1])/(n+1) =
                       ((n+1)*CMA[n]-CMA[n])/(n+1) + x[n+1]/(n+1) =
                       CMA[n]*(1-1/(n+1)) + x[n+1]*1/(n+1) */
                    double                              exponentialAverageFactor,

                    /* Used in Training phase only. 
                       runningMean = newMean*factor + runningMean*(1-factor) */
                    void                               *resultRunningMean,
                    /* Output in training mode, input in inference. Is the moving average
                       of  variance[x] (factor is applied in the same way as for runningMean) */
                    void                               *resultRunningVariance,

                    /* Has to be >= CUDNN_BN_MIN_EPSILON. Should be the same in forward and backward functions. */
                    double                              epsilon,

                    /* Optionally save intermediate results from the forward pass here
                       - can be reused to speed up backward pass. NULL if unused */
                    void                               *resultSaveMean,
                    void                               *resultSaveInvVariance );


/* Performs backward pass of Batch Normalization layer. Returns x gradient,
* bnScale gradient and bnBias gradient */
swdnnStatus_t swdnnBatchNormalizationBackward(
			  	swdnnHandle_t                       handle,
			  	swdnnBatchNormMode_t                mode,
			  	const void                         *alphaDataDiff,
			  	const void                         *betaDataDiff,
			  	const void                         *alphaParamDiff,
			  	const void                         *betaParamDiff,
			  	const swdnnTensorDescriptor_t       xDesc, /* same desc for x, dx, dy */
			  	const void                         *x,
			  	const swdnnTensorDescriptor_t       dyDesc,
			  	const void                         *dy,
			  	const swdnnTensorDescriptor_t       dxDesc,
			  	void                               *dx,
			  	/* Shared tensor desc for the 4 tensors below */
			  	const swdnnTensorDescriptor_t       dBnScaleBiasDesc,
			  	const void                         *bnScale, /* bnBias doesn't affect backpropagation */
			  	/* scale and bias diff are not backpropagated below this layer */
			  	void                               *dBnScaleResult,
			  	void                               *dBnBiasResult,
			  	/* Same epsilon as forward pass */
			  	double                              epsilon,

			  	/* Optionally cached intermediate results from
			  	   forward pass */
			  	const void                         *savedMean,
			  	const void                         *savedInvVariance );

/*
* Derives a tensor descriptor from layer data descriptor for BatchNormalization 
* scale, invVariance, bnBias, bnScale tensors. Use this tensor desc for 
* bnScaleBiasMeanVarDesc and bnScaleBiasDiffDesc in Batch Normalization forward and backward functions.
*/
swdnnStatus_t swdnnDeriveBNTensorDescriptor(
                    swdnnTensorDescriptor_t             derivedBnDesc,
                    const swdnnTensorDescriptor_t       xDesc,
                    swdnnBatchNormMode_t                mode );

/*
 * Fully connected layer has five APIs
 * 1.swdnnFullyConnectedForward
 * 2.swdnnFullyConnectedBiasForward
 * 3.swdnnFullyConnectedBackwardBias
 * 4.swdnnFullyConnectedBackwardFilter
 * 5.swdnnFullyConnectedBackwardData
 */
swdnnStatus_t swdnnFullyConnectedForward(
	swdnnHandle_t handle,
	const void *alpha,
	const swdnnTensorDescriptor_t xDesc,
	const void *x,
	const swdnnFilterDescriptor_t wDesc,
	const void *w,
	const void *beta,
	const swdnnTensorDescriptor_t yDesc,
	void *y);

swdnnStatus_t swdnnFullyConnectedBiasForward(
	swdnnHandle_t handle,
	const void *alpha,
	const swdnnTensorDescriptor_t xDesc,
	const void *x,
	const swdnnFilterDescriptor_t wDesc,
	const void *w,
	const swdnnTensorDescriptor_t bDesc,
	const void *bias,
	const void *beta,
	const swdnnTensorDescriptor_t yDesc,
	void *y);

swdnnStatus_t swdnnFullyConnectedBackwardBias(
	swdnnHandle_t handle,
	const void *alpha,
	const swdnnTensorDescriptor_t dyDesc,
	const void *dy,
	const void *beta,
	const swdnnTensorDescriptor_t dbDesc,
	void *db);

swdnnStatus_t swdnnFullyConnectedBackwardFilter(
	swdnnHandle_t handle,
	const void *alpha,
	const swdnnTensorDescriptor_t xDesc,
	const void *x,
	const swdnnTensorDescriptor_t dyDesc,
	const void *dy,
	const void *beta,
	const swdnnFilterDescriptor_t dwDesc,
	void *dw);

swdnnStatus_t swdnnFullyConnectedBackwardData(
	swdnnHandle_t handle,
	const void *alpha,
	const swdnnFilterDescriptor_t wDesc,
	const void *w,
	const swdnnTensorDescriptor_t dyDesc,
	const void *dy,
	const void *beta,
	const swdnnTensorDescriptor_t dxDesc,
	void *dx);









swdnnStatus_t sw_cat_f(
                const swdnnTensorDescriptor_t       x1Desc,
                void                         *x1,
                const swdnnTensorDescriptor_t       x2Desc,
                void                         *x2,
                const swdnnTensorDescriptor_t       yDesc,
                void                         *y,
		unsigned int dim );

swdnnStatus_t sw_upsample_f(
        const float *k,
        const swdnnTensorDescriptor_t inputDesc,
        const void *input,
        const swdnnTensorDescriptor_t outputDesc,
        void *output);
//void swdnn_init()
//void swdnn_halt();

#if defined (__cplusplus)
}
#endif

#endif	/* SWDNN_H_ */
