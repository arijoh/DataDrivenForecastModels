
xreflect=function(x,xmin,xmax){
  select=x<xmin
  x[select]=xmin[select]+(xmin[select]-x[select])
  x[select][x[select]>xmax[select]]=xmin[select]
  select=x>xmax
  x[select]=xmax[select]-(x[select]-xmax[select])
  x[select][x[select]<xmin[select]]=xmax[select]
  return(x)
}

ddsoptim=function(f,start,ndds=2500,rdds=0.2,xmax,xmin,...){
  #DDS search algorithm to find not the optimal but a good set of parameters
  #Tolson, B. A., & Shoemaker, C. A. (2007). Dynamically dimensioned search algorithm for computationally efficient watershed model calibration. Water Resources Research, 43(1), W01413. https://doi.org/10.1029/2005WR004723
  #f=function to optimize
  #start=starting parameters
  #ndds=no. of objective function evaluations to perform
  #rdds=perturbation parameter - DO NOT TOUCH UNLESS YOU KNOW WHAT YOU'RE DOING
  #xmax, xmin=vectors of max and min values for each parameter (define reasonable ranges, the algorithm searches these intervals)
  x=start
  xbest=x
  fbest=1e10
  trace=vector()
  add_args=list(...) #xmax, xmin
  #get additional arguments
  namesf=names(formals(f)[2:length(formals(f))])[names(formals(f)[2:length(formals(f))]) %in% names(add_args)]
  formals(f)[namesf]=add_args[namesf]
  for (counts in c(1:ndds)){
    formals(f)[1]=list(x)
    fval=f()
    #print(c(counts,fval,x))
    trace=rbind(trace,c(counts,fval,x))
    if (is.na(fval)) fval=1e50
    if (fval<fbest){
      fbest=fval
      xbest=x
    }
    pincl=1-log(counts)/log(ndds)
    npert=0
    #RANDOMLY PERTURBATE PARAMATERS WITH PROBABILITY PINCL, KEEP TRACK OF NO. OF PERTURBATIONS
    x=xbest
    rno=runif(length(x))
    select=rno<pincl
    rvar=rnorm(sum(select))
    sig=rdds*(xmax[select]-xmin[select])
    x[select]=xbest[select]+sig*rvar
    x=xreflect(x,xmin,xmax)
    npert=sum(select)
    #ALWAYS MODIFY AT LEAST ONE PARAMETER, SELECT RANDOMLY IF NONE HAS BEEN MODIFIED
    if (npert==0){
      rno=abs(runif(1))
      I=as.integer(rno*length(x))
      if (I>length(x)) I=length(x)
      if (I<1) I=1
      rvar=rnorm(1)
      sig=rdds*(xmax[I]-xmin[I])
      x[I]=xbest[I]+sig*rvar
      x=xreflect(x,xmin,xmax)
    }
  }
  #popt=c(fbest,xbest)
  return(list(value = fbest, par = xbest, trace = trace))
}
