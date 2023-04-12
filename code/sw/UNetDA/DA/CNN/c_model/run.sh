set -x
sh ./mk
bsub -b -I -q q_sw_expr -cgsp 64 -n 1 -share_size 13000 -host_stack 4096 -mpecg 1 -cache_size 0 -o aaa.log ./predict
#export STASK_MALLOC_CROSS=1
#bsub -I -akernel -b -q q_sw_expr -N 1 -cgsp 64 -ro_size 256 -share_size 200 -cross_size 84000 -priv_size 2 -mpecg 6 -cache_size 0 ./predict
