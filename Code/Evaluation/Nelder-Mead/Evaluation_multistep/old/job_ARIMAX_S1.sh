#!/bin/bash -l
#BSUB -env none
### -- Name of the job ---
#BSUB -J My_Application
### -- estimated wall clock time (execution time): hh:mm:ss --
#BSUB -W 48:00
### -- number of processors/cores/nodes --
#BSUB -M 2GB
#BSUB -n 1
#BSUB -o OandE/Output_%J.out
#BSUB -e OandE/Error_%J.err

/appl/R/bin/R-3.6.3-mkl CMD BATCH --vanilla "/zhome/6e/9/133731/Desktop/Thesis/Thesis/Code/Evaluation/Evaluation_multistep/evaluation_ARIMAX_S1.r" "/zhome/6e/9/133731/Desktop/Thesis/Thesis/Code/Evaluation/Evaluation_multistep/evaluation_ARIMAX_S1.rout"
