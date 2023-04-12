import time
import numpy as np
import multiprocessing
import time
import mpi4py.MPI as MPI
import os

import joblib
import torch
import torch.nn as nn
import torch.utils.data as Data

import torch.nn.functional as F
import torchvision.transforms as T
import copy

import config as cf
from pathlib import Path
class Model(object):
    """
    <<interface>> Model，接口类
    """

    def fit(self):
        """
        模型训练
        """
        pass

    def predict(self):
        """
        模型预测
        """
        pass

    def save(self, path):
        """
        模型保存
        """
        pass

    def load(self, path):
        """
        模型加载
        """
        pass

def get_tar_lev(model = 'svr',div = 1,model_save_path = '.'):
    layer_num = cf.get_layer_num()
    tar_l = cf.get_tar_l()
    ord_ = str(cf.get_ord_())

    l = []
    for layer_rank in range(layer_num):
        if layer_rank == 0:
            tar = tar_l[0]
            lev = 0
        else:
            tar = tar_l[int((layer_rank - 1) / 32 + 1) ]
            lev = (layer_rank - 1) % 32

        path = get_path(tar,ord_,lev,div,model,model_save_path)
        l.append(path)

    return l


def get_path(tar,model = '',lev = 32,div = 1,bias = "",model_save_path = '/share1/liminyan/model'):
    if tar == 'phs':lev = 0
    if model == '0':path = model_save_path + '/'+ model +bias+tar+'_'+str(div) +'_'+str(lev)+'_0'
    elif model == '1' :path = model_save_path + '/'+ model +bias+tar+'_'+str(div) +'_'+str(lev)+'_1'
    else:path = model_save_path + '/'+ model +bias+tar+'_'+str(div) +'_'+str(lev)+''
    return path

class ConvDeconvNeuralNetwork(Model):

    """
    用于集合后处理的卷积反卷积网络
    Attributes:
        kernel_model: torch框架下的自建模型
        learning_rate: float 学习率
        weight_decay: L2范数正则项在损失函数中的权重
        optimizer: Adam优化器
        batch_size: int 批样本数量
        epochs: int 所有样本上训练的次数
        patiences: int 早停策略下的容忍度 n/次epochs
    """

    def __init__(self, model_save_path, arch=None, learning_rate = 0.005, weight_decay=0.005,batch_size = 32, epochs = 30, patiences = 100):
        self.kernel_model = arch
        self.learning_rate = learning_rate
        self.weight_decay = weight_decay
        self.optimizer = torch.optim.Adam(self.kernel_model.parameters(), lr=self.learning_rate, weight_decay=self.weight_decay)
        self.batch_size = batch_size
        self.model_save_path = model_save_path
        self.epochs = epochs
        self.patiences = patiences
        self.tar = ''
        self.lev = 0
        self.config = None
        self.div = 1


    def unnormalized(self,img,teg = 'Y'):

        if teg == 'X':
            img = img * self.std_X + self.mu_X
        elif teg == 'Y':
            img = img * self.std_Y + self.mu_Y
        return img

    def predict(self, data):
        """
        模型预测
        :param DataProcessor: 数据处理器实例，主要使用其中的 x_test
        :return: 返回预测值或实例本身
        """
        # 将模型切换为预测状态，将测试集数据转换为 torch 数据进行预测，预测结果再转换为 numpy.ndarray 数据
        self.kernel_model.eval()
        process_data = (data - self.mu_X)/self.std_X
        x_test = torch.from_numpy(process_data).type(torch.FloatTensor)
        y_pred = self.kernel_model(x_test)
        return self.unnormalized(y_pred.detach().numpy())

    def save(self, path):
        """
        模型保存
        :param path: 模型保存的路径，保存的文件为 .pt 格式，如 0302_PRAVG_CNN.pt
        :return: 返回实例本身
        """
        torch.save({
            'model_state_dict': self.kernel_model.state_dict(),
            # 'optimizer_state_dict': self.optimizer.state_dict(),
            'mu_X': self.mu_X,
            'std_X': self.std_X,
            'mu_Y': self.mu_Y,
            'std_Y': self.std_Y
        }, path)



        return self

    def get_item_mu(self,data,ibegin,iend,jbegin,jend,shape = 3,halo = 10):

        ibegin += halo -1
        iend += halo 
        ibegin -= halo
        iend += halo

        jbegin += halo -1
        jend += halo 
        jbegin -= halo
        jend += halo

        # print('org',data.shape)
        # print('log',ibegin,iend,jbegin,jend)

        real = data[:,5:-5,5:-5]
        left = real[:,:,-halo:]
        right = real[:,:,0:halo]

        real = np.append(left,real,axis = 2)
        real = np.append(real,right,axis = 2)
        up = np.repeat(real[:,[0]], repeats=halo, axis=1) 
        down = np.repeat(rnameeal[:,[-1]], repeats=halo, axis=1)

        # print('left',left.shape)
        # print('right',right.shape)
        # print('up',up.shape)
        # print('down',down.shape)
        real = np.append(up,real,axis=1)
        real = np.append(real,down,axis=1)
        # print('real',real.shape)
        mid = real[:,ibegin:iend,jbegin:jend]

        # print('mid',mid.shape)
        return mid
        # mid = mid.reshape(shape[0],1,shape[1],shape[2])

    def load(self, path,ibegin,iend,jbegin,jend):
        """
        模型加载
        :param path: 加载模型的路径，加载的文件为 .pt 格式，如 0302_PRAVG_CNN.pt
        :return: 返回实例本身
        """
        # print(self.model_save_path, flush=True)
        checkpoint = torch.load(path, map_location=torch.device('cpu'))
        self.kernel_model.load_state_dict(checkpoint['model_state_dict'])
        # print(path, self.kernel_model.name)
        # self.mu_X = np.load(self.model_save_path+'/mu_X_192x384.npy')
        # self.std_X = np.load(self.model_save_path+'/std_X_192x384.npy')
        # self.mu_X = np.load(self.model_save_path+'/mu_X_var_192x384.npy')
        # self.std_X = np.load(self.model_save_path+'/std_X_var_192x384.npy')
        self.mu_X    = np.load(self.model_save_path+'/mu5_X.npy')
        self.std_X   = np.load(self.model_save_path+'/std5_X.npy')
        # if self.kernel_model.name == "phs":
        #     self.mu_Y    = self.mu_X[0:1]
        #     self.std_Y   = self.std_X[0:1]
        # elif self.kernel_model.name == "pt":
        #     self.mu_Y    = self.mu_X[1:33]
        #     self.std_Y   = self.std_X[1:33]
        # elif self.kernel_model.name == "u_Agrid":
        #     self.mu_Y    = self.mu_X[33:65]
        #     self.std_Y   = self.std_X[33:65]
        # elif self.kernel_model.name == "v_Agrid":
        #     self.mu_Y    = self.mu_X[65:97]
        #     self.std_Y   = self.std_X[65:97]
        # else:
        #     pass
        # self.mu_X1   = np.load(self.model_save_path+'/mu3_X.npy')
        # self.std_X1  = np.load(self.model_save_path+'/std3_X.npy')
        self.mu_Y    = self.mu_X[self.kernel_model.name:self.kernel_model.name+1]
        self.std_Y   = self.std_X[self.kernel_model.name:self.kernel_model.name+1]
        local = False
        if local:
            self.mu_X   = self.mu_X[:,:100,:190]
            self.mu_Y   = self.mu_Y[:100,:190]
            self.std_X  = self.std_X[:,:100,:190]
            self.std_Y  = self.std_Y[:100,:190]

        self.kernel_model.eval()
        return self


    @staticmethod
    def weigth_init(m):
        """
        初始化模型参数
        :param m: torch中神经网络层
        """
        if isinstance(m, torch.nn.Conv2d):
            m.weight.data.normal_(0.1)
            m.bias.data.zero_()

class CDNNArch(torch.nn.Module):
    """
    torch架构下的卷积反卷积神经网络架构
    """
    def __init__(self, name, ):
        super(CDNNArch, self).__init__()
        self.name  = name 
        if name == "pt" or name == "u_Agrid" or name == "v_Agrid":
            num_class = 32
        elif name == "phs":
            num_class = 1
        else:
            num_class = 97

        self.gzs = None
        self.actv = torch.nn.Tanh()

        self.input = torch.nn.Sequential(
            torch.nn.BatchNorm2d(97)
            )

        self.conv1 = torch.nn.Sequential(
            torch.nn.Conv2d(97, 8, 3),
            torch.nn.BatchNorm2d(8),
            self.actv
            )
        
        self.conv2 = torch.nn.Sequential(
            torch.nn.Conv2d(8, 16, 3),
            self.actv
            )
        self.conv3 = torch.nn.Sequential(
            torch.nn.Conv2d(16, 32, 3),
            self.actv
            )

        self.conv3_merge = torch.nn.Sequential(
            torch.nn.Conv2d(32, 16, 3,padding=1),
            self.actv
            )

        self.conv2_merge = torch.nn.Sequential(
            torch.nn.Conv2d(16, 8, 3,padding=1),
            self.actv
            )

        self.deconv_3 = torch.nn.Sequential(
            torch.nn.ConvTranspose2d(32, 16, 3),
            self.actv
            )

        self.deconv_2 = torch.nn.Sequential(
            torch.nn.ConvTranspose2d(16, 8, 3),
            self.actv
            )

        self.deconv_1 = torch.nn.Sequential(
            torch.nn.ConvTranspose2d(8, num_class, 3),
            )

    def save_mid_res(self,res,name):
        res = res.detach().numpy()
        shape = res.shape
        all_size = 1
        for x in shape:
          all_size *= x
        res=res.reshape([1,all_size])
        print('name:',name,res.shape)
        np.savetxt('mid_res/'+name+'.txt',res)


    def forward(self, x_input):
        x = self.input(x_input)#10
        c1 = self.conv1(x)#9------------------------|c1
        c2 = self.conv2(c1)#8--------------|c2     |
        c3 = self.conv3(c2)#7              |       |
        
        dco3 = self.deconv_3(c3)#8------------|dco3   |

        concat3 = torch.cat([dco3,c2],dim=1)#8|       |

        merge3 = self.conv3_merge(concat3)#8---------<|merge3
        dco2 = self.deconv_2(merge3)#9----------------|dco2
        concat2 = torch.cat([dco2,c1],dim=1)#9        |
        merge2 = self.conv2_merge(concat2)#9---------<|merge2
        dco1 = self.deconv_1(merge2)#10
        out = dco1

        if self.name == 'pt':
            out  = out + x_input[:,1:33]
        elif self.name == 'u_Agrid':
            out  = out + x_input[:,33:65]
        elif self.name == 'v_Agrid':
            out  = out + x_input[:,65:97]
        elif self.name == "phs":
            out  = out + x_input[:,0:1]
        else:
            out  = out + x_input[:,:97]

        return out 

        
class MulityCNN(object):
    """docstring for MulityCNN"""
    def __init__(self, div_range,comm):
        super(MulityCNN, self).__init__()
        self.step = 0
        self.div_range = div_range
        self.comm = comm
        self.model = {}
        self.data_sz = None
        self.halo_lat = 6
        self.halo_lon = 12

    def get_arg_from_rank(self,key,begin_div = 2):

        if key == 0:
            tar = 'phs'
            lev = 0
        elif key <= 32:
            tar = 'pt'
            lev = key - 1 
        elif key <= 64:
            tar = 'u_Agrid'
            lev = key - 1 - 32
        else:
            tar = 'v_Agrid'
            lev = key - 1 - 64

        return tar,lev

    def set_input_m(self,tar,model = '',lev = 32,div = 1,bias = "",model_save_path = '/share1/liminyan/model'):
        # comm = MPI.COMM_WORLD
        comm_rank = self.comm.Get_rank()
        comm_size = self.comm.Get_size()
        num = comm_rank
        num = 0

        last = '.npy'
        if bias == 'cnn':
            last = '.pt'
        if tar == 'phs':
            lev = 0
        if model == '0':
            if model_save_path == "/home/xuewei/ddn/xiaoyi/NN_gpu/trainedModels/ResUNet":
                # path = model_save_path + '/'+ model +bias+tar+'_'+str(16) +'_'+str(lev)+'_[0]'+'_'+str(num)+last
                path = model_save_path +'_' + tar + '_layer_300/saveModel'
            else:
                path = model_save_path + '/'+ model +bias+tar+'_'+str(div) +'_'+str(lev)+'_[0]'+'_'+str(num)+last
        else:
            path = model_save_path + '/'+ model +bias+tar+'_'+str(div) +'_'+str(lev)+''+last

        # print(path)
        return path

    def load(self,path):
        ord_ = 0
        s = time.time()
        num = self.comm.Get_rank()
        # print(path)
        # print(self.data_sz)
        for x in range(1,self.div_range+1):

            div = x

            div_eatch = int(32 / self.div_range )

            div = x * div_eatch
            # print("div", div, self.div_range, flush=True)
            tar,lev =  self.get_arg_from_rank(num,begin_div = 2)
            # model_path = self.set_input_m(tar,
            #     model = str(ord_),lev = lev,div = div * 16,
            #     model_save_path = path,
            #     bias = 'cnn')
            model_path = path + '/savedModel' + str(num)
            # svr = ConvDeconvNeuralNetwork(ResUnetArch(tar_layer=num), epochs = 50)
            # svr = ConvDeconvNeuralNetwork(model_save_path=path, arch=CDNNArch(name=tar), epochs = 50)
            svr = ConvDeconvNeuralNetwork(model_save_path=path, arch=ComplexUnet(name=num), epochs = 50)
            svr.tar = tar
            svr.lev = lev

            ibegin = self.data_sz[-1-5]
            iend = self.data_sz[-1-4]
            jbegin = self.data_sz[-1-3]
            jend = self.data_sz[-1-2]

            svr.load(model_path,ibegin,iend,jbegin,jend)
            key = str(tar) +'_' + str(num)
            self.model[key] = svr
            # print(num, key)

            
        e = time.time()
        if num == 0:
            print('load time',e-s)

    def convolution2d(self,image, kernel):
        m, n = kernel.shape
        if (m == n):
            y = image.shape[-2]
            x = image.shape[-1]
            y = y - m + 1
            x = x - m + 1
            new_image = np.zeros((self.data_sz[-1-1],y,x))
            for i in range(y):
                for j in range(x):
                    new_image[:, i, j] = np.sum(np.sum(image[:, i:i+m, j:j+m]*kernel, axis=-1), axis=-1)
        return new_image

    def denoise(self,data):

        noisy = data
        sigma_est = np.mean(estimate_sigma(noisy))
        patch_kw = dict(patch_size=5,      # 5x5 patches
                        patch_distance=6,  # 13x13 search area
                        )
        denoise_fast = denoise_nl_means(noisy, h=0.8 * sigma_est, fast_mode=True,**patch_kw)
        return denoise_fast


    def predict(self,data):
        num = self.comm.Get_rank()
        size = self.comm.Get_size()
        tar,lev =  self.get_arg_from_rank(num,begin_div = 2)
        ibegin = self.data_sz[-1-5]
        iend = self.data_sz[-1-4]
        jbegin = self.data_sz[-1-3]
        jend = self.data_sz[-1-2]
        
        result = np.empty([self.div_range,self.data_sz[-1-1],iend - ibegin+1, jend - jbegin+1])

        # AAA
        # kernel2 = np.ones([2,2]) / 4.0
        # kernel3 = np.ones([3,3]) / 9.0
        # kernel5 = np.ones([5,5]) / 25.0
        # kernel7 = np.ones([7,7]) / 49.0
        # smooth_kernel = np.array([  [1 ,  10,  1],
        #                             [10, 100, 10],
        #                             [1 ,  10,  1]   ]) / 144
        for x in range(1,self.div_range+1):
            div = x 
            div_eatch = int(32 / self.div_range )
            div = x*div_eatch
            key = str(tar) +'_' + str(num)
            mid_res =  self.model[key].predict(data)
            print(mid_res.shape)
            if tar == 'phs':
                # print(num, tar, lev, flush=True)
                # print(mid_res[:, 0, self.halo:-self.halo,self.halo:-self.halo].shape, flush=True)
                
                result[x - 1]= mid_res[:, 0, self.halo_lat:-self.halo_lat,self.halo_lon:-self.halo_lon]
                # result[x - 1]= self.convolution2d(image = mid_res[:, 0], kernel=smooth_kernel)[:, self.halo - 1:-self.halo + 1,self.halo - 1:-self.halo + 1]
                print(num, tar, lev, flush=True)
            else:
                # print(num, tar, lev, flush=True)
                # print(mid_res[:, lev, self.halo:-self.halo,self.halo:-self.halo].shape, flush=True)
                result[x - 1]= mid_res[:, 0, self.halo_lat:-self.halo_lat,self.halo_lon:-self.halo_lon]
                # if self.step <= 3:
                #     result[x - 1]= mid_res[:, lev, self.halo:-self.halo,self.halo:-self.halo]
                # else:
                #     result[x - 1]= self.convolution2d(image = mid_res[:, lev], kernel=smooth_kernel)[:, self.halo - 1:-self.halo + 1,self.halo - 1:-self.halo + 1]
                # result[x - 1]= self.convolution2d(image = mid_res[:, lev], kernel=smooth_kernel)[:, self.halo - 1:-self.halo + 1,self.halo - 1:-self.halo + 1]
                print(num, tar, lev, flush=True)
                # result[x - 1]= mid_res

            # mean_std = np.mean(self.model[key].std_X)
            # noisy = mid_res[0,halo:-halo,halo:-halo]
            # sigma = [mean_std*10, mean_std*3]

            #  下面都是去噪的算法
            # result[x - 1][0] = sp.ndimage.filters.gaussian_filter(mid_res[0], [1,1], mode='reflect')[halo:-halo,halo:-halo]

            # result[x - 1]  = self.denoise(noisy)

            # result[x - 1][0] = self.denoise(mid_res[0,halo : -halo,halo : -halo])

            # result[x -1][0] = self.convolution2d(mid_res[0,halo :-halo+1,halo :-halo+1],kernel2)

            # result[x - 1][0] = self.convolution2d(mid_res[0,halo-1:-halo+1,halo - 1:-halo+1],kernel3)

            # result[x - 1][0] = self.convolution2d(mid_res[0,halo-2:-halo+2,halo - 2:-halo+2],kernel5)
            
            # result[x - 1][0] = self.convolution2d(mid_res[0,halo-3:-halo+3,halo - 3:-halo+3],kernel7)
            # None
        # if comm_rank == 0:
        #     self.step += 1

        return result

class BBlock(nn.Module):
    def __init__(self, in_ch, out_ch):
        super().__init__()
        self.conv1 = nn.Conv2d(in_ch, out_ch, 3, padding=1) ##
        self.conv2 = nn.Conv2d(out_ch, out_ch, 3, padding=1) ##
        self.relu  = nn.SiLU()
        self.bn1   = nn.BatchNorm2d(in_ch)
        self.bn2   = nn.BatchNorm2d(out_ch)

        self.conv3 = nn.Conv2d(in_ch, out_ch, 3, padding=1) ##
        
    def forward(self, x, head = False):
        enhance = True
        if enhance:
            if not head:
                x1 = self.relu(self.bn1(x))
            else:
                x1 = x
            x2 = self.conv2(self.relu(self.bn2(self.conv1(x1))))
            return x2 + self.conv3(x)
        else:
            return self.relu(self.conv2(self.relu(self.conv1(x)))) + self.conv3(x)

class Encoder(nn.Module):
    def __init__(self, chs):
        super().__init__()
        self.enc_blocks = nn.ModuleList([BBlock(chs[i], chs[i+1]) for i in range(len(chs)-1)])
        self.pool       = nn.MaxPool2d((2, 2))

    def forward(self, x):
        ftrs = []
        for i, block in enumerate(self.enc_blocks):
            if i == 0:
                x = block(x, True)
            else:
                x = block(x)
            ftrs.append(x)
            x = self.pool(x)
        return ftrs

class Encoder0(nn.Module):
    def __init__(self, chs):
        super().__init__()
        self.enc_blocks = nn.ModuleList([BBlock(chs[i], chs[i+1]) for i in range(len(chs)-1)])

    def forward(self, x):
        ftrs = []
        for i, block in enumerate(self.enc_blocks):
            if i == 0:
                x = block(x, True)
            else:
                x = block(x)
            ftrs.append(x)
        return ftrs     

# class Decoder(nn.Module):
#     def __init__(self, chs):
#         super().__init__()
#         self.chs        = chs
#         self.upconvs    = nn.ModuleList([nn.ConvTranspose2d(chs[i], chs[i+1], (2, 2), (2, 2)) for i in range(len(chs)-1)])
#         self.dec_blocks = nn.ModuleList([BBlock(chs[i], chs[i+1]) for i in range(len(chs)-1)])

#     def forward(self, x, encoder_features):
#         decoder_features = [x]
#         for i in range(len(self.chs)-1):
#             x        = self.upconvs[i](x)
#             enc_ftrs = encoder_features[i]
#             x        = torch.cat([x, enc_ftrs], dim=1)
#             x        = self.dec_blocks[i](x)
#             decoder_features.append(x)
#         return decoder_features

class Decoder(nn.Module):
    def __init__(self, chs):
        super().__init__()
        self.chs        = chs
        self.upconvs    = nn.ModuleList([nn.ConvTranspose2d(chs[i], chs[i+1], (2, 2), (2, 2)) for i in range(len(chs)-1)])
        self.upsample   = True
        if self.upsample:
            nn_list = []
            for i in range(len(chs)-1):
                nn_list.append(nn.Upsample(scale_factor=(2, 2)))
                nn_list.append(nn.Conv2d(chs[i], chs[i+1], 3, padding=1))
            self.upconvs    = nn.ModuleList(nn_list)
        else:
            self.upconvs    = nn.ModuleList([nn.ConvTranspose2d(chs[i], chs[i+1], (2, 2), (2, 2)) for i in range(len(chs)-1)])
        self.dec_blocks = nn.ModuleList([BBlock(chs[i], chs[i+1]) for i in range(len(chs)-1)])

    def forward(self, x, encoder_features):
        decoder_features = [x]
        for i in range(len(self.chs)-1):
            if self.upsample:
                x        = self.upconvs[2*i](x)
                x        = self.upconvs[2*i+1](x)
            else:
                x        = self.upconvs[i](x)
            enc_ftrs = encoder_features[i]
            x        = torch.cat([x, enc_ftrs], dim=1)
            x        = self.dec_blocks[i](x)
            decoder_features.append(x)
        return decoder_features

class Decoder0(nn.Module):
    def __init__(self, chs):
        super().__init__()
        self.chs        = chs
        self.upconvs    = nn.ModuleList([nn.Conv2d(chs[i], chs[i+1], 3, padding=1) for i in range(len(chs)-1)])
        self.dec_blocks = nn.ModuleList([BBlock(chs[i], chs[i+1]) for i in range(len(chs)-1)])

    def forward(self, x, encoder_features):
        decoder_features = [x]
        for i in range(len(self.chs)-1):
            x        = self.upconvs[i](x)
            enc_ftrs = encoder_features[i]
            x        = torch.cat([x, enc_ftrs], dim=1)
            x        = self.dec_blocks[i](x)
            decoder_features.append(x)
        return decoder_features

class ComplexUnet(nn.Module):
    def __init__(self, name=None, gzs=None, enc_chs=(97, 8, 16, 32), dec_chs=(32, 16, 8), num_class=1):
        super().__init__()
        addition = 3
        if addition == 1:
            enc_chs = (97, 16, 32, 64)
            dec_chs = (64, 32, 16)
        if addition == 2:
            enc_chs = (97, 16, 32, 64, 128)
            dec_chs = (128, 64, 32, 16)
        if addition == 3:
            enc_chs = (97, 16, 32, 64, 128, 256)
            dec_chs = (256, 128, 64, 32, 16)
        if addition == 4:
            enc_chs = (97, 16, 32, 64, 128, 256, 512)
            dec_chs = (512, 256, 128, 64, 32, 16)
        # if addition == 5:
        #     enc_chs = (97, 64, 128, 256, 512, 1024)
        #     dec_chs = (1024, 512, 256, 128, 64)
        self.gzs         = gzs
        self.encoder     = Encoder(enc_chs)
        self.decoder     = Decoder(dec_chs)
        # if name == "pt" or name == "u_Agrid" or name == "v_Agrid":
        #     num_class = 32
        # elif name == "phs":
        #     num_class = 1
        # else:
        #     num_class = 97
        self.head        = nn.Conv2d(dec_chs[-1], num_class, 1)
        self.device      = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.transform1  = T.Resize((192, 384))
        self.transform2  = T.Resize((190, 370))
        self.name        = name  
        
    def forward(self, x_input):
        transform = False
        if transform:
            x_input  = self.transform1(x_input)

        enc_ftrs = self.encoder(x_input)
        out      = self.decoder(enc_ftrs[::-1][0], enc_ftrs[::-1][1:])
        out      = self.head(out[-1])
        out      = out + x_input[:,self.name:self.name+1]
        # if self.name == 'pt':
        #     out  = out + x_input[:,1:33]
        # elif self.name == 'u_Agrid':
        #     out  = out + x_input[:,33:65]
        # elif self.name == 'v_Agrid':
        #     out  = out + x_input[:,65:97]
        # elif self.name == "phs":
        #     out  = out + x_input[:,0:1]
        # else:
        #     out  = out + x_input[:,:97]

        if transform:
            out      = self.transform2(out)
        return out

class ComplexUnet0(nn.Module):
    def __init__(self, name=None, gzs=None, enc_chs=(97, 8, 16, 32), dec_chs=(32, 16, 8), num_class=97):
        super().__init__()
        addition = 1
        if addition == 1:
            enc_chs = (97, 16, 32, 64)
            dec_chs = (64, 32, 16)
        self.gzs         = gzs
        if not self.gzs == None:
            enc_chs = (98, 16, 32, 64)
        self.encoder     = Encoder0(enc_chs)
        self.decoder     = Decoder0(dec_chs)
        if name == "pt" or name == "u_Agrid" or name == "v_Agrid":
            num_class = 32
        elif name == "phs":
            num_class = 1
        else:
            num_class = 97
        self.head        = nn.Conv2d(dec_chs[-1], num_class, 1)
        self.device      = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.name        = name  
        
    def forward(self, x_input):
        enc_ftrs = self.encoder(x_input)
        out      = self.decoder(enc_ftrs[::-1][0], enc_ftrs[::-1][1:])
        out      = self.head(out[-1])
        if self.name == 'pt':
            out  = out + x_input[:,1:33]
        elif self.name == 'u_Agrid':
            out  = out + x_input[:,33:65]
        elif self.name == 'v_Agrid':
            out  = out + x_input[:,65:97]
        elif self.name == "phs":
            out  = out + x_input[:,0:1]
        else:
            out  = out + x_input[:,:97]

        return out

class ResUnetArch(torch.nn.Module):
    """
    torch架构下的卷积反卷积神经网络架构 1x180x360 -> 1x180x360
    """
    def __init__(self, tar_layer):
        super(ResUnetArch, self).__init__()

        self.input = torch.nn.Sequential(
            torch.nn.BatchNorm2d(97)
            )

        self.conv1 = torch.nn.Sequential(
            torch.nn.Conv2d(97, 8, 3),
            torch.nn.BatchNorm2d(8),
            # torch.nn.Tanh()
            torch.nn.Tanh()
            # torch.nn.ReLU()
            )
        
        self.conv2 = torch.nn.Sequential(
            torch.nn.Conv2d(8, 16, 3),
            # torch.nn.BatchNorm2d(16),
            # torch.nn.Tanh()
            torch.nn.Tanh()
            # torch.nn.ReLU()
            )
        self.conv3 = torch.nn.Sequential(
            torch.nn.Conv2d(16, 32, 3),
            # torch.nn.BatchNorm2d(32),
            # torch.nn.Tanh()
            torch.nn.Tanh()
            # torch.nn.ReLU()
            )

        self.conv3_merge = torch.nn.Sequential(
            torch.nn.Conv2d(32, 16, 3,padding=1),
            # torch.nn.BatchNorm2d(16),
            torch.nn.Tanh()
            )

        self.conv2_merge = torch.nn.Sequential(
            torch.nn.Conv2d(16, 8, 3,padding=1),
            # torch.nn.BatchNorm2d(8),
            # torch.nn.Tanh()
            torch.nn.Tanh()
            # torch.nn.ReLU()
            )

        self.deconv_3 = torch.nn.Sequential(
            torch.nn.ConvTranspose2d(32, 16, 3),
            # torch.nn.BatchNorm2d(16),
            # torch.nn.Tanh()
            torch.nn.Tanh()
            )

        self.deconv_2 = torch.nn.Sequential(
            torch.nn.ConvTranspose2d(16, 8, 3),
            # torch.nn.BatchNorm2d(8),
            # torch.nn.Tanh()
            torch.nn.Tanh()
            )

        self.deconv_1 = torch.nn.Sequential(
            torch.nn.ConvTranspose2d(8, 1, 3),
            )
        self.tar_layer = tar_layer

    def save_mid_res(self,res,name):
        res = res.detach().numpy()
        shape = res.shape
        all_size = 1
        for x in shape:
          all_size *= x
        res=res.reshape([1,all_size])
        print('name:',name,res.shape)
        np.savetxt('mid_res/'+name+'.txt',res)


    def forward(self, x_input):
        x = self.input(x_input)#10
        # x  = x_input.unsqueeze(1)
        c1 = self.conv1(x)#9------------------------|c1
        c2 = self.conv2(c1)#8--------------|c2     |
        c3 = self.conv3(c2)#7              |       |
        
        dco3 = self.deconv_3(c3)#8------------|dco3   |
        # self.save_mid_res(dco3,'dco3')

        concat3 = torch.cat([dco3,c2],dim=1)#8|       |
        # self.save_mid_res(concat3,'concat3')

        merge3 = self.conv3_merge(concat3)#8---------<|merge3
        # self.save_mid_res(merge3,'merge3')

        dco2 = self.deconv_2(merge3)#9----------------|dco2
        # self.save_mid_res(dco2,'dco2')

        concat2 = torch.cat([dco2,c1],dim=1)#9        |
        # self.save_mid_res(concat2,'concat2')

        merge2 = self.conv2_merge(concat2)#9---------<|merge2
        # self.save_mid_res(merge2,'merge2')

        dco1 = self.deconv_1(merge2)#10
        # self.save_mid_res(dco1,'dco1')

        # self.save_mid_res(out,'out')
        return dco1.squeeze(1) + x_input[:,self.tar_layer]