proj<-function(om,eql,srD,
                 ftar =0.75,
                 fCV  =0.3,
                 start=50,end=103,rcvPeriod=5){
  
  ## Recovery
  rcv =seq(c(fbar(om)[,ac(start)]),c(FLBRP:::refpts(eql)["msy","harvest"])*ftar,length.out=rcvPeriod+1)
  rcv =FLQuant(rcv,dimnames=dimnames(fbar(om)[,ac(start+0:rcvPeriod)]))
  om  =FLash:::fwd(om,f=rcv,sr=eql)
  
  ## F in longterm
  om  =fwdWindow(om,eql,end=end)
  lgt =FLQuant(c(rcv[,rcvPeriod+1]),dimnames=list(year=(start+rcvPeriod+1):(end)))
  om  =FLash:::fwd(om,f=lgt, sr=eql)
  
  harvest(om)=rlnorm(dim(srD)[6],log(harvest(om)),fCV)
  units(harvest(om))="f"
  apex=c(ages(catch.sel(eql))[catch.sel(eql)==c(fapex(catch.sel(eql)))])
  range(om)[c("minfbar","maxfbar")]=apex
  res=FLash::fwd(om,f=fbar(om)[,-1],sr=eql,sr.residuals=srD)
  
  res}
