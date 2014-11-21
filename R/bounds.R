boundPar<-function(t., fmin, fmax)
   {
   x=asin((2*t.-fmin)/(fmax-fmin)-1)/1.570795

   return(x)
   }

unboundPar<-function(x, fmin, fmax)
   {
   t=(fmin+(fmax-fmin)*(sin(x*1.570795)+1))/2

   return(t)
   }

penalty<-function(x)
   {
   fpen<-0
   
   if (x < 0.00001)
     fpen<-(x-0.00001)*(x-0.00001)
   else if (x > 0.9999)
     fpen<-(x-0.9999)*(x-0.9999)
   else if (x < -1)
     fpen<-1000*(x+1)*(x+1)
   else if (x > 1)
     fpen<-1000*(x-1)*(x-1)

  return(fpen)
  }

fr <- function(x) {   ## Rosenbrock Banana function
    x1 <- x[1]
    x2 <- x[2]
    100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

# rec.<-rec(sr.p4)
# ssb.<-ssb(sr.p4)

logl.ar1<-function(rho,sigma2,obs,hat)
  {
  n       <-length(obs)
  s2      <-sum((obs[,-1] - rho*obs[,-n] - hat[,-1] + rho*hat[,-n])^2)
  s1      <-(1-rho^2)*(obs[,1]-hat[,1])^2 + s2
  sigma2.a<-(1-rho^2)*sigma2
  res     <-(log(1/(2*pi))-n*log(sigma2.a)+log(1-rho^2)-s1/(2*sigma2.a))/2

  return(res)
  }

logl<-function(sigma2,obs,hat)
  {
  n  <-length(obs)
  s1 <-sum((obs-hat)^2)
  res<-((log(1/(2*pi))-n*log(sigma2)-s1/(2*sigma2))/2)

  return(res)
  }
  
fn <- function(x,rec,ssb) {
    x1    <-x[1]
    x2    <-x[2]
    sigma2<-x[3]
    res   <--logl.ar1(0,sigma2,rec,ssb*x1/(x2+ssb))

    return(res)
    }

# optim(c(400,30,.3), fn,rec=rec.,ssb=ssb.)


bh<-function(a,b,ssb,rec){
    a.<-unboundPar(a, lower.[1], upper.[1])
    b.<-unboundPar(b, lower.[2], upper.[2])
#print(c(a,b))
#		print(c(a.,b.))

		rHat<-a.*ssb/(b.+ssb)
		
#		rec. <-c(rec)
#    rHat.<-c(rHat)
#    ssb. <-c(ssb)

#    print(rec.)
#    print(ssb.)
#    print(rHat.)
#    plot(ssb.,rec.,xlim=c(0,max(ssb.)),ylim=c(0,max(rec.)))
#    lines(sort(ssb.),rHat.[order(ssb.)])

    return(rHat)
    }
    
# bevholt {{{
bh.bounded <- function()
  {
  ## log likelihood, assuming normal log.
  logl <- function(a, b, sigma2, rec, ssb)
    {
    res <-#sum(dnorm(log(rec), log(bh(a,b,ssb,rec)), sqrt(sigma2), TRUE), na.rm=TRUE) +
          sum((log(rec)-log(bh(a,b,ssb,rec)))^2)
          #var(log(rec)-log(bh(a,b,ssb,rec)), na.rm=TRUE) +
          #+penalty(a)
          #+penalty(b)
          
#    print(c(a,b,penalty(a),penalty(b)))

    return(res)
    }
    
  ## initial parameter values
  initial <- structure(function(rec, ssb)
		  {
      a      <-boundPar((lower.[1]+upper.[1])/2, lower.[1], upper.[1])
			b      <-boundPar((lower.[2]+upper.[2])/2, lower.[2], upper.[2])
			
			sigma2 <- var(log(rec)-log(mean(rec)), y= NULL, na.rm = TRUE)

#			print(c(a,b,sigma2))

			return(list(a=a, b=b, sigma2=sigma2))
		  },

  ## bounds
  lower=c(0, 0.0001, 0.0001),
	upper=rep(1, 3))

  ## model to be fitted
  model  <- rec~bh(a,b,ssb,rec)

	return(list(logl=logl, model=model, initial=initial))
} # }}}

# 
# library(FLCore)
# data(ple4)
# sr.p4<-as.FLSR(ple4)
# sr.p4<-transform(sr.p4,ssb=ssb/1000,rec=rec/1000)
# model(sr.p4)<-bevholt()
# sr.p4<-mle(sr.p4,start=list(a=400,b=30,sigma2=.2))
# 
# #l1=mean(rec(sr.p4))
# #l2=quantile(rec(sr.p4)/ssb(sr.p4))
# 
# lower.<-c(0.2*c(params(sr.p4)[1,1]),0.2*c(params(sr.p4)[1,2]))
# upper.<-c(  5*c(params(sr.p4)[1,1]),  5*c(params(sr.p4)[1,2]))
# 
# model(sr.p4)<-bh.bounded()
# sr.p4<-mle(sr.p4)
# plot(sr.p4)
# 
# range         <-seq(80,120,5)/100
# param.grid    <-expand.grid(s=seq(0.75,1.0,0.025),v=range*params(ple4.sr)[1,"v"],ll=NA)
# 
# ## profile and plot liklihood around best guess
# for (i in 1:length(param.grid[,1]))
#    param.grid[i,"ll"]<-logLik(mle(ple4.sr,fixed=(list(s=param.grid[i,"s"],v=param.grid[i,"v"],spr0=6.12))))
# 
# image(  interp(param.grid[,"s"], param.grid[,"v"], param.grid[,"ll"]))
# contour(interp(param.grid[,"s"], param.grid[,"v"], param.grid[,"ll"]),add=T)
# 
