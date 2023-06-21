FC=mpif90
MPAL_INCLUDE=../build/
MPAL_LIB=../build/

XMATH_DIR=/home/export/online1/mdt00/shisuan/swthhpca/DIDA-SHARE/3rdParty/xMath/

LIB_DIR=../lib/

# test1: test1.f90
# 	$(FC) -I$(MPAL_INCLUDE) -L$(MPAL_INCLUDE) -L$(LIB_DIR) $^ -o $@ -lmpal -lcadnaF -lrpe

test3: test3.f90
	$(FC) -mieee -g -O3 -mftz -msimd -I$(MPAL_INCLUDE) -L$(MPAL_INCLUDE) -L$(LIB_DIR) -L$(XMATH_DIR) $^ -o $@ -lmpal -lrpe  -lswblas -lrma -lm_slave

run: test3
	bsub -b -I -q q_sw_expr -n 1 -cgsp 64 -share_size 10000 -host_stack 256 ./test3

.PHONY: clean

clean:
	rm -f test1 test2 test3
