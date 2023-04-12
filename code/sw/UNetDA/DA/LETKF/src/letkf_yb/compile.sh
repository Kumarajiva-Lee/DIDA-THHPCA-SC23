mpicc -O3 -g -mieee -mhost -o yb_master.o -c letkf_yb_master.c
mpicc -O3 -g -mieee -mslave -msimd -mftz -o yb_slave.o -c letkf_yb_slave.c
ar rc libletkf_yb.a yb_master.o yb_slave.o
