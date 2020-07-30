import os, shutil
import time

rscriptname='evaluation_ARIMA_S1.r'
job_in='job_regression.sh'
job_submit='job_submit.sh'

scriptpath=os.path.dirname(os.path.realpath(__file__))
workpath=os.path.abspath(os.path.join(scriptpath, os.pardir))
script=os.path.join(scriptpath,rscriptname)

for i in range(162): #162
#get working directories and option combinations
    exepath= '/appl/R/bin/R-3.6.3-mkl CMD BATCH --vanilla ' + ' "-' + str(i+1) + '" "' + script + '" "' + os.path.join(scriptpath, 'OandE', "job" + str(i+1) + ".Rout" ) + '"'
    #modify job file
    job_out=os.path.join(scriptpath, job_submit)
    shutil.copyfile(os.path.join(scriptpath, job_in),job_out)
    f=open(job_out,'r')
    text=f.read()
    f.close()
    text=text.replace('INSERT_EXEPATH_HERE',exepath)
    f=open(job_out,'w')
    f.write(text)
    f.close()
    time.sleep(0.25)

    #submit
    os.system("bsub -q hpc < " + '"' + job_out + '"')
    #os.system("qsub " + '"' + jobpath + '"')
