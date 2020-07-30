#!/bin/bash -l
#BSUB -env none
### -- Name of the job ---
#BSUB -J My_Application
### -- estimated wall clock time (execution time): hh:mm:ss --
#BSUB -W 12:00
### -- number of processors/cores/nodes --
#BSUB -M 2GB
#BSUB -n 1
#BSUB -o OandE/Output_%J.out
#BSUB -e OandE/Error_%J.err
#LSB_JOB_REPORT_MAIL=N
/appl/R/bin/R-3.6.3-mkl CMD BATCH --vanilla  "-162" "/zhome/6e/9/133731/Desktop/Thesis/Thesis/Code/Evaluation/DDS/Evaluation_multistep/ARIMA/S1/evaluation_ARIMA_S1.r" "/zhome/6e/9/133731/Desktop/Thesis/Thesis/Code/Evaluation/DDS/Evaluation_multistep/ARIMA/S1/OandE/job162.Rout"
