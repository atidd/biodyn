#' priorFn
#' @description A utility function to help set up the \code{prior} slot in \code{biodyn}.
#'            
#' @param ... any one of  \code{r,k,p,b0,msy,bmsy,fmsy}
#' 
#' #' @return \code{list} with om, ...
#'  
#' @export

#' @rdname priorFn
#' 
#' @seealso \code{\link{biodyn}}
#' 
#' @examples
#' \dontrun{
#'    priorFn(r=c(weight=1,0.3,0.03)
#'    }
priorFn=function(...){
  
  args=list(...)
  
  res=biodyn:::biodyn()@priors
  nms=dimnames(res)$params[dimnames(res)$params %in% names(args)]
  nms=nms[nms %in% dimnames(res)$params]
  
  for (i in nms){
    if ('a'      %in% names(args[[i]])) res[i,'a'][]     =unlist(c(args[[i]]['a']))
    if ('b'      %in% names(args[[i]])) res[i,'b'][]     =unlist(c(args[[i]]['b']))
    if ('weight' %in% names(args[[i]])) res[i,'weight'][]=unlist(c(args[[i]]['weight']))}
  
  res}  

#' priors
#' 
#' @description Creates priors for \code{biodyn} using \code{FLBRP} 
# #' and an optional \code{FLStock} 
#' which corresponds to Operating Model.
#'            
#' #' @return \code{FLPar} which corresponds to \code{priors} slot in 
#' \code{boidyn}
#'  
#' @export

#' @rdname priors
#' 
#' @seealso \code{\link{biodyn}}
#' 
#' @examples
#' \dontrun{
#'    prior(bd)=eql
#'    }
setGeneric('setPrior<-',   
           function(object,value) standardGeneric('setPrior<-'))

setMethod('setPrior<-', signature(object='biodyn',value='FLBRP'),
          function(object,value) {

            res=priorCalc(eql=value)
            
            priors(object)
            object})
  
priorCalc=function(eql){
  refpts=FLBRP:::refpts
  
  msy =c(refpts(eql)["msy","yield"])
  bmsy=c(refpts(eql)["msy","biomass"])
  #r_  =try(calcR(eql))
  #if ("try-error"%in% is(r_)) r_=NA
  
  k   =refpts(eql)["virgin","biomass"]
  b0  =1#c(stock(stk)[,1]/k)
  
  f=c(refpts(eql)["crash","harvest"])
  if (is.na(f)) f=c(refpts(eql)["msy","harvest"])*3
  r=try(log(lambda(leslie(eql,f)[drop=TRUE])))
  if ("try-error"%in% is(r)) r=NA
  
  p=optimise(function(p,bmsy,k) 
    (bmsy-k*(1/(1+p))^(1/p))^2, c(0.001,5), bmsy=bmsy,k=k)$minimum
  
  c(msy=msy,bmsy=bmsy,fmsy=msy/bmsy,r=r,k=k,shape=bmsy/k,p=p,b0=b0)}

  
