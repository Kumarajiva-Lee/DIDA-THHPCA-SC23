import mpi4py.MPI as MPI
import numpy as np
import sys
import time
import PostProcessor
import Model
import config as cf
import sys
import copy
import scipy as sp
import netCDF4 as nc
import scipy.ndimage
from sys import argv
saved_model_path_intg = {}

saved_model_path_intg['phs'] = '/ddnstor/xuewei/xiaoyi/NN_cpu/trainedModels/ResUNet3_0_14_200'
saved_model_path_intg['pt']  = '/ddnstor/xuewei/xiaoyi/NN_cpu/trainedModels/ResUNet3_0_14_200'
saved_model_path_intg['u']   = '/ddnstor/xuewei/xiaoyi/NN_cpu/trainedModels/ResUNet3_0_14_200'
saved_model_path_intg['v']   = '/ddnstor/xuewei/xiaoyi/NN_cpu/trainedModels/ResUNet3_0_14_200'
cf.init()
inter_num  = cf.get_inter_num()
halo_lat = 6
halo_lon = 12
x_num = 2
y_num = 2

start_rank = int(argv[1])
gourp_root_rank = start_rank
comm = MPI.COMM_WORLD
comm_rank = comm.Get_rank()
comm_size = comm.Get_size()
bias_rank = int(argv[1])
group_num = cf.get_group_num()
layer_num = cf.get_layer_num()
model_num = cf.get_model_num()
all_size = cf.get_all_size()
group_rank = (comm_rank - bias_rank) % group_num
group_root = group_rank + bias_rank
group_bloc = int((comm_rank - bias_rank) / group_num)
data_sz = np.empty(10,dtype=np.int32)*0
newGroup = comm.group.Excl([x for x in range(bias_rank)])
new_comm = comm.Create_group(newGroup)
new_comm = new_comm.Split(group_rank)
new_comm_rank = new_comm.Get_rank()
new_comm_size = new_comm.Get_size()
train_sz = int(data_sz[1] * data_sz[2] * data_sz[3]*data_sz[8] / new_comm_size)
train_data = np.empty(train_sz)
proxy = PostProcessor.PostProcessor()
proxy_intgrate = PostProcessor.PostProcessor()
ensemble = np.empty(0)
data_list=[]

def run(iternum = 0, last_result = None):
	new_comm.barrier()
	if comm_rank == group_root:
		print(iternum, flush=True)
		comm.Recv([data_sz, 10, MPI.INT], bias_rank - 1, 0)
		print(data_sz, flush=True)
	new_comm.Bcast([data_sz, 10, MPI.INT],root = int(group_rank/group_num))
	new_comm.barrier()
	inter_num = data_sz[9]
	total_num = data_sz[0]
	base_num = data_sz[8]
	global ensemble
	
	if iternum == 0:
		# initialization
		svr1 = Model.MulityCNN(div_range = 1, comm = new_comm)
		svr1.data_sz = copy.deepcopy(data_sz)
		svr1.data_sz[0] = 1
		svr1.halo_lat = halo_lat
		svr1.halo_lon = halo_lon
		proxy_intgrate.model = svr1
		real_rank = int((comm_rank - bias_rank) / group_num)
		if real_rank == 0:
			print(real_rank, 'phs')
			proxy_intgrate.load(saved_model_path_intg['phs'])
		elif real_rank <= 32:
			proxy_intgrate.load(saved_model_path_intg['pt'])
		elif real_rank <= 64:
			proxy_intgrate.load(saved_model_path_intg['u'])
		elif real_rank <= 96:
			proxy_intgrate.load(saved_model_path_intg['v'])
	if iternum == 0:
		# receive data
		if comm_rank == group_root:
			recv_sz = data_sz[1] * data_sz[2] * data_sz[3] * data_sz[8]
			recv_data = np.empty(recv_sz)
			comm.Recv([recv_data, recv_sz, MPI.DOUBLE], bias_rank - 1, 0)
			print('DNN: recv data',recv_data.shape,flush = True)
			print('DNN: recv data',np.sum(recv_data),flush = True)
			recv_data = recv_data.reshape(data_sz[3],data_sz[2],data_sz[1],data_sz[8])
			
			recv_data = recv_data.transpose(3,2,1,0)
			# np.save('/home/xuewei/ddn/jiaqilong/dida-v4/test/exp_gamma1_0/member0_step0.npy', recv_data[0])
			# now: (ens, z, y, x)
			# 无halo版本 发回给fortran
			# np.save('/home/xuewei/ddn/jiaqilong/dida-v4/test/exp_debug/' + str(iternum) + '_receive.npy', recv_data)
			tmp_recv = recv_data
		else:
			recv_data = np.empty([data_sz[8],data_sz[1],data_sz[2]+halo_lat*2,data_sz[3]+halo_lon*2])
		
		# new_comm.barrier()
		# new_comm.Bcast(recv_data,0)
		# new_comm.barrier()

	# ready to predict
	if iternum == 0:
		if comm_rank == group_root:
			recv_sz =  data_sz[1] * data_sz[2] * data_sz[3] * data_sz[8]
			# first round cnn do nothing
			result_data = tmp_recv
		else:
			result_data = None

		new_comm.barrier()
		if comm_rank == group_root:
			result_data = result_data.transpose(3,2,1,0)
			total_size = recv_sz
			result_data = result_data.reshape(total_size)

			comm.Send([result_data, total_size, MPI.DOUBLE], bias_rank - 1, 0)
			
		del result_data
		del recv_data
	else:
		if comm_rank == group_root:
			recv_sz =  data_sz[1] * data_sz[2] * data_sz[3] * data_sz[8]
			result_data = np.empty((data_sz[8] * data_sz[2] * data_sz[3] * new_comm.Get_size()))
			# np.save('/home/xuewei/ddn/jiaqilong/dida-v4/test/exp_debug/' + str(iternum) + '_ensemble_brfore_cnn.npy', ensemble)
		else:
			result_data = None
		new_comm.barrier()
		if comm_rank == group_root:
			# np.save('/home/xuewei/ddn/jiaqilong/dida-v4/test/exp_gamma1_0/member0_step' + str(iternum) + '_input.npy', ensemble[0])
			print('begin predict', flush=True)
		proxy_intgrate.data = ensemble
		result = proxy_intgrate.predict()
		print(result.shape, flush=True)
		result = result.reshape(data_sz[8] *  data_sz[2] * data_sz[3]) 
		
		new_comm.barrier()
		new_comm.Gather(result, result_data, root=0)
		new_comm.barrier()
		if comm_rank == group_root:
			result_data = result_data.reshape(new_comm.Get_size(),data_sz[8],data_sz[2],data_sz[3])
			result_data = result_data.transpose(1,0,2,3)
			# np.save('/home/xuewei/ddn/jiaqilong/dida-v4/test/exp_gamma1_0/member0_step' + str(iternum) + '.npy',result_data[0])
			result_data = result_data.transpose(3,2,1,0)
			total_size = recv_sz
			# np.save('/home/xuewei/ddn/jiaqilong/dida-v4/test/exp_debug/' + str(iternum) + '_result.npy', result_data)
			result_data = result_data.reshape(total_size)
			
			comm.Send([result_data, total_size, MPI.DOUBLE], bias_rank - 1, 0)
			
		del result_data

	new_comm.barrier()

	# receive ensemble data
	if comm_rank == group_root:
		recv_sz = data_sz[1] * data_sz[2] * data_sz[3] * data_sz[8] 
		recv_data = np.empty(recv_sz)
		comm.Recv([recv_data, recv_sz, MPI.DOUBLE], bias_rank - 1, 0)
		
		print('DNN: recv ensemble data for next round',recv_data.shape,flush = True)
		recv_data = recv_data.reshape(data_sz[3],data_sz[2],data_sz[1],data_sz[8])
		recv_data = recv_data.transpose(3,2,1,0)
		# tmp_recv = recv_data
		# np.save('/home/xuewei/ddn/jiaqilong/dida-v4/test/exp_gamma1_0/' + str(iternum) + '_ensemble_before_halo.npy', recv_data)
		data_sz_lr = recv_data.shape[0]*halo_lon*recv_data.shape[1]*recv_data.shape[2]

		item_send_left =  recv_data[:,:,:,: halo_lon]
		item_send_right = recv_data[:,:,:,-halo_lon:]
		item_recv_right = np.zeros(data_sz_lr)
		item_recv_left = np.zeros(data_sz_lr)
		item_send_left = item_send_left.reshape(data_sz_lr)
		item_send_right = item_send_right.reshape(data_sz_lr)

		item_recv_right = item_send_left
		item_recv_left = item_send_right

		item_recv_right = item_recv_right.reshape(recv_data.shape[0],recv_data.shape[1],recv_data.shape[2],halo_lon)
		item_recv_left = item_recv_left.reshape(recv_data.shape[0],recv_data.shape[1],recv_data.shape[2],halo_lon)
		recv_data = np.append(item_recv_left,recv_data,axis =3)
		recv_data = np.append(recv_data,item_recv_right,axis =3)

		item_recv_up = np.repeat(recv_data[:,:,[0]], repeats=halo_lat, axis=2)
		item_recv_down = np.repeat(recv_data[:,:,[-1]], repeats=halo_lat, axis=2)

		recv_data = np.append(item_recv_up,recv_data,axis =2)
		recv_data = np.append(recv_data,item_recv_down,axis =2)
		# np.save('/home/xuewei/ddn/jiaqilong/dida-v4/test/exp_gamma1_0/' + str(iternum) + '_ensemble_after_halo.npy', recv_data)
	else:
		recv_data = np.empty([data_sz[8] , data_sz[1], data_sz[2] + halo_lat * 2, data_sz[3] + halo_lon * 2])
	new_comm.barrier()
	new_comm.Bcast(recv_data, 0)
	new_comm.barrier()
	ensemble = recv_data
	
	del recv_data
	return inter_num


inter_num = run(0)
for x in range(1,inter_num):
	run(x)
