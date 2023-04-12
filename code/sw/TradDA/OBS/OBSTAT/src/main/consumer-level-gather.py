from rediscluster import RedisCluster
#import redis
import json
import numpy as np
from datetime import datetime,timedelta
import cal_indicator as indicator
import testplotmulti
import os
import time
import configparser
from mpi4py import MPI
import sys


comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()


pic_path=sys.argv[1]
#-----------------------------------

cf = configparser.ConfigParser()
cf.read("../namelist/initial_for_obstat.input")

#-------------------------------set redis-------------------------------------


redisaddress = eval(cf.get("obstat_ini", "redis_address")).split(',')
redisaddress = [x.split(':') for x in redisaddress]
redisaddress = np.array(redisaddress)

startup_nodes = []
address_id = 0
for x in redisaddress:
   host = redisaddress[address_id,0]
   port = redisaddress[address_id,1]
   redis_dictionary = {"host":host, "port":6379}
   startup_nodes.append(redis_dictionary)
   address_id = address_id+1

#print(startup_nodes)

#startup_nodes = [                           #集群节点及端口
    #{"host":"192.168.1.1", "port":6379},
    #{"host":"192.168.1.2", "port":6379},
    #{"host":"192.168.1.3", "port":6379}
#]

#print(startup_nodes)

conn = RedisCluster(startup_nodes=startup_nodes, decode_responses=True)

##################start_change #########start_change #############start_change ###########start_change#########
casename = eval(cf.get("obstat_ini", "case_name")) #'case_yz'
#print(casename)

#------------------------------set time-------------------------------------

redis_da_start_time = conn.hget(casename,'da_start_time')
da_start0,da_start1,da_start2,da_start3,da_start4 = redis_da_start_time.split(',',4)
da_start_time = datetime(int(da_start0),int(da_start1),int(da_start2),int(da_start3),int(da_start4))

delta_seconds = int(conn.hget(casename,'da_in_seconds')) #5*60

start_time_get = eval(cf.get("obstat_ini", "obsstat_start_time"))
start0,start1,start2,start3,start4 = start_time_get.split(',',4)
start_time = datetime(int(start0),int(start1),int(start2),int(start3),int(start4))

end_time_get = eval(cf.get("obstat_ini", "obsstat_end_time"))
end0,end1,end2,end3,end4 = end_time_get.split(',',4)
end_time = datetime(int(end0),int(end1),int(end2),int(end3),int(end4))
run_end_time = end_time.strftime("%Y%m%d%H%M")

Numtime_to_start = int((start_time-da_start_time).total_seconds() // delta_seconds )

#print(Numtime_to_start)

if (start_time-da_start_time).total_seconds() % delta_seconds == 0:
    run_start_time = start_time
else:
    run_start_time = da_start_time
    for i in range(0,Numtime_to_start+1):
        run_start_time = run_start_time + timedelta(seconds = delta_seconds)

Numtime = int(((end_time-run_start_time).total_seconds()) // delta_seconds + 1) 


#print('start time:',run_start_time)
#print('numtime:',Numtime)

#------------------------------set space------------------------------------

NumYGrid = int(conn.hget(casename,'num_lat')) #180
NumXGrid = int(conn.hget(casename,'num_lon')) #360
NumZGrid = int(conn.hget(casename,'mpas_num_lev')) #26
delta_lev_id = int(conn.hget(casename,'obs_stat_interval_vertical')) #4



vari_all = eval(cf.get("obstat_ini", "obsstat_var_name")).split(',')

prob_vari_surf = ['ps']

vari_surf = [x for x in vari_all if x in prob_vari_surf]
vari_norm = [x for x in vari_all if x not in prob_vari_surf]

#print(vari_norm)

#print(NumZGrid)
#print(-delta_lev_id)

vari_norm_lev_id_ave = [x for x in np.arange(NumZGrid,0,-delta_lev_id)]
#print(vari_norm_lev_id_ave)
#vari_norm_lev_id_add = [1] 
#隔几层画隔几层写,可追加指定层
vari_norm_lev_id = vari_norm_lev_id_ave#+vari_norm_lev_id_add
vari_surf_lev_id = [0]
#vari_norm_lev_id = [1,32,40,59]
#vari_surf_lev_id = [60]


#-------------------------------set hash-key name---------------------------

res_in_key = str(NumXGrid)+'x'+str(NumYGrid)
#res_in_key = str(360)+'x'+str(180)
deg = eval(conn.hget(casename,'atm_mpas_sceneid')) 
test_case = eval(conn.hget(casename,'test_case'))

#print(deg)
cc_case = deg.split('-',2)[2]

#print('cc_case',cc_case)

#print(deg)

hashkey_lat = 'lat:'+res_in_key #'lat:1deg-201902'
hashkey_lon = 'lon:'+res_in_key

#print(hashkey_lat)

hashkey_tr = 'realfield:'+deg+':'+res_in_key #'realfield:1deg-201902'
hashkey_bg = 'axbfield:'+casename+':'+deg+':'+res_in_key
hashkey_amb = 'ambfield:'+casename+':'+deg+':'+res_in_key

#print(hashkey_tr)
print(hashkey_bg)


##################end_change #########end_change #############end_change ###########end_change################
#------------------------------prepare---------------------------------------


Numlevel = len(vari_norm_lev_id)
Numproc = Numlevel*(len(vari_norm))+len(vari_surf)
rank_gather = 0

#print(Numproc)

vari = vari_norm + vari_surf

vari_lev_group_id = []
for x in vari_norm:
   vari_lev_group_id.append(vari_norm_lev_id)
for x in vari_surf:
   vari_lev_group_id.append(vari_surf_lev_id)
vari_lev_id = np.array(vari_lev_group_id)


#----------------------------------sspadd-------------------------------------
nl_sec = Numproc // size
nl_res = Numproc % size
if(rank < nl_res) :
    nl_start = rank * (nl_sec + 1)
    nl_end = nl_start + nl_sec + 1
else:
    nl_start = rank * nl_sec + nl_res
    nl_end = nl_start + nl_sec

#print("myrank:", rank, nl_start, nl_end)
########################################取经纬度信息################################



lat_key_get = []
for lat_in in range(1,NumYGrid+1):
        lat_key_str = str(lat_in)+':'+'lat'
        lat_key_get.append(lat_key_str)

lon_key_get = []
for lon_in in range(1,NumXGrid+1):
        lon_key_str = str(lon_in)+':'+'lon'
        lon_key_get.append(lon_key_str)

lat_get = conn.hmget(hashkey_lat, lat_key_get)
lon_get = conn.hmget(hashkey_lon, lon_key_get)

lat_get = [float(x) for x in lat_get]
lon_get = [float(x) for x in lon_get]
 
lat_theta_get = [[np.cos(x*np.pi/180)] for x in lat_get]

lat = np.array(lat_get)
lon = np.array(lon_get)

lat_theta = np.array(lat_theta_get)
#print(lat_theta)

id_interval = len(lat)//6
id_lat = np.arange(0,len(lat)+1,id_interval)


#########################################取背景场，真实场，分析场，诊断绘图##########################################

va = []
va_theta = []
runtime = []
curr_time_from_start = run_start_time 
curr_time = curr_time_from_start.strftime("%Y%m%d%H%M")

for time_in in range(0,Numtime):
    key_str = []
    key_get = []
    runtime.append(time_in)

    for proc_in in range(nl_start, nl_end):

        proc_for_norm = Numlevel*(len(vari_norm))
        
        if nl_start < proc_for_norm:
           level_in = nl_start % Numlevel
           vari_in = nl_start // Numlevel
        else:
           level_in = 0
           vari_in = len(vari_norm)-1 + (nl_end - proc_for_norm)


        level_in_key_list = vari_lev_id[vari_in]
        level_in_key = level_in_key_list[level_in]

        #print(level_in_key)

        variable_in_key = vari[vari_in]

        key_str = []
        key_get = []
        time_in_key = curr_time

        t0=time.time()
        #print(variable_in_key)


        for lat_in in range(1,NumYGrid+1):
        #for lat_in in range(1,5+1):
            for lon_in in range(1,NumXGrid+1): 
            #for lon_in in range(1,5+1): 

                lat_in_key = lat_in
                lon_in_key = lon_in 
                key_str = str(lon_in_key)+':'+str(lat_in_key)+':'+str(level_in_key)+':'+str(variable_in_key)
                key_get.append(key_str)
                #print(key_get[2]) 
                #print(hashkey_tr+':'+str(time_in_key))

        t1=time.time()
        print("拼接：",t1-t0)


        #print(key_get) 
        #print(hashkey_tr+':'+str(time_in_key))
        tr_get = conn.hmget(hashkey_tr+':'+str(time_in_key), key_get)
        bg_get = conn.hmget(hashkey_bg+':'+str(time_in_key), key_get)        
        amb_get = conn.hmget(hashkey_amb+':'+str(time_in_key), key_get)

        t2=time.time()
        print("hmget：",t2-t1)

        #print(amb_get)

        tr_get = [float(x) for x in tr_get]
        bg_get = [float(x) for x in bg_get]
        amb_get = [float(x) for x in amb_get]

        tr_array = np.array(tr_get)
        bg_array = np.array(bg_get)
        amb_array = np.array(amb_get)

        tr = tr_array.reshape(NumYGrid,NumXGrid)
        bg = bg_array.reshape(NumYGrid,NumXGrid)
        amb = amb_array.reshape(NumYGrid,NumXGrid)
        an = bg + amb

        tr_theta = tr * lat_theta
        bg_theta = bg * lat_theta
        amb_theta = amb * lat_theta
        an_theta = bg_theta + amb_theta

        #rmse_6090S = indicator.cal_rmse(an[id_lat[0]:id_lat[1],:], tr[id_lat[0]:id_lat[1],:])
        #rmse_3060S = indicator.cal_rmse(an[id_lat[1]:id_lat[2],:], tr[id_lat[1]:id_lat[2],:])
        #rmse_0030S = indicator.cal_rmse(an[id_lat[2]:id_lat[3],:], tr[id_lat[2]:id_lat[3],:])
        #rmse_0030N = indicator.cal_rmse(an[id_lat[3]:id_lat[4],:], tr[id_lat[3]:id_lat[4],:])
        #rmse_3060N = indicator.cal_rmse(an[id_lat[4]:id_lat[5],:], tr[id_lat[4]:id_lat[5],:])
        #rmse_6090N = indicator.cal_rmse(an[id_lat[5]:id_lat[6],:], tr[id_lat[5]:id_lat[6],:])
        #rmse_level = indicator.cal_rmse(an[:,:], tr[:,:])

        rmse_theta_6090S = indicator.cal_rmse(an_theta[id_lat[0]:id_lat[1],:], tr_theta[id_lat[0]:id_lat[1],:])
        rmse_theta_3060S = indicator.cal_rmse(an_theta[id_lat[1]:id_lat[2],:], tr_theta[id_lat[1]:id_lat[2],:])
        rmse_theta_0030S = indicator.cal_rmse(an_theta[id_lat[2]:id_lat[3],:], tr_theta[id_lat[2]:id_lat[3],:])
        rmse_theta_0030N = indicator.cal_rmse(an_theta[id_lat[3]:id_lat[4],:], tr_theta[id_lat[3]:id_lat[4],:])
        rmse_theta_3060N = indicator.cal_rmse(an_theta[id_lat[4]:id_lat[5],:], tr_theta[id_lat[4]:id_lat[5],:])
        rmse_theta_6090N = indicator.cal_rmse(an_theta[id_lat[5]:id_lat[6],:], tr_theta[id_lat[5]:id_lat[6],:])
        rmse_theta_level = indicator.cal_rmse(an_theta[:,:], tr_theta[:,:])


        t000=time.time()

        #tr_gather = np.array(comm.allgather(tr))
        #bg_gather = np.array(comm.allgather(bg))
        #an_gather = np.array(comm.allgather(an))

        tr_theta_gather = np.array(comm.allgather(tr_theta))
        bg_theta_gather = np.array(comm.allgather(bg_theta))
        an_theta_gather = np.array(comm.allgather(an_theta))

        t111=time.time()

        print('gather',t111-t000)

        interval_i = len(level_in_key_list)
        istart = vari_in*Numlevel
        iend = istart+interval_i

        #print('istart',istart)
        #print('iend',iend)
        #print('len_gather',len(an_gather))

        #rmse_global = indicator.cal_rmse(an_gather[istart:iend], tr_gather[istart:iend])
        rmse_global_theta = indicator.cal_rmse(an_theta_gather[istart:iend], tr_theta_gather[istart:iend])

        #va.append([rmse_6090N, rmse_3060N, rmse_0030N, rmse_0030S, rmse_3060S, rmse_6090S, rmse_level, rmse_global])

        va_theta.append([rmse_theta_6090N, rmse_theta_3060N, rmse_theta_0030N, rmse_theta_0030S, rmse_theta_3060S, rmse_theta_6090S, rmse_theta_level, rmse_global_theta])


        pic_param = [runtime, curr_time, level_in_key, level_in, variable_in_key, pic_path, cc_case]

        t222=time.time()

        #testplotmulti.plotlev(bg, an, tr, va, va_theta, pic_param)
        testplotmulti.plotlev(bg, an, tr, va_theta, pic_param)

        t3=time.time()
        print('plot',t3-t222)
        print('draw1time',t3-t0)


    curr_time_from_start = curr_time_from_start + timedelta(seconds = delta_seconds)
    curr_time = curr_time_from_start.strftime("%Y%m%d%H%M")


