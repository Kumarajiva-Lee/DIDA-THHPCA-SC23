#!/bin/bash
#SBATCH -J new
#SBATCH -o dida.out
#SBATCH -p cnCPU
#SBATCH --exclusive
#SBATCH -N 25
#SBATCH --ntasks-per-socket=10

procs=496
#namelist=${2}

ulimit -s unlimited
ulimit -c unlimited

sh ../setconf2redis.sh namelists.input
mpirun -n $procs ${PWD}/dida.exe namelists.input
