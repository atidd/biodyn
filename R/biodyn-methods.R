# %+% is in ggplot2, but export as a function, so our method steps on it
# 
# "+.gg" <- function(e1, e2) {
#   # Get the name of what was passed in as e2, and pass along so that it
#   # can be displayed in error messages
#   e2name <- deparse(substitute(e2))
#   
#   if      (is.theme(e1))  add_theme(e1, e2, e2name)
#   else if (is.ggplot(e1)) add_ggplot(e1, e2, e2name)
# }
# 
# 
# #' @rdname gg-add
# #' @export
# "%+%" <- `+.gg`

setMethod('harvest', signature(object='biodyn'),
             function(object,when=.5,...) {
             
             yrs1=  dimnames(stock(object))$year
             yrs2=c(dimnames(stock(object))$year[-1],as.numeric(max(dimnames(stock(object))$year))+1)
             
             #res <- catch(object)/(stock(object)[,yrs1]*(1-when)+
             #                        stock(object)[,yrs2]*when)
             
             yrs=dimnames(catch(object))$year[dimnames(catch(object))$year %in% dimnames(catch(object))$year]
             res <- catch(object)[,yrs]/stock(object)[,yrs]
             units(res) <- 'hr'
             return(res)
           })

setMethod('stock', signature(object='biodyn'),
          function(object,when=0) {
            
            when=max(min(when,1),0)
            if (when<=0) return(object@stock)
            
            yrs =  dimnames(stock(object))$year
            yrs1=  rev(rev(yrs)[-1])
            yrs2=  yrs[-1]
             
            (1-when)*stock(object)[,yrs1]+when*stock(object)[,yrs2]})

#' computePrd
#'
#' Calculates the surplus production for a biomass dynamic model given a level of stock biomass
#' 
#' @param object an object of class \code{biodyn} 
#' @param biomass stock biomaas, may be a \code{numerix},  \code{FLQuant} or missing. In the latte case the stock slot will be used.
#' @param ... other arguments
#'
#' @return an \code{FLPar} object
#' 
#' @seealso \code{\link{plotPrd}}
#' 
#' @export
#' @rdname sp
#'
#' @aliases computePrd,biodyn,FLQuant-method  computePrd,biodyn,missing-method  computePrd,biodyn,numeric-method
#' @examples
#' \dontrun{ computePrd(bd,seq(0,params(bd)['k'])) }
#'  
setGeneric('computePrd',function(object,biomass,...) standardGeneric('computePrd'))
setMethod( 'computePrd', signature(object='biodyn',   biomass='missing'),     function(object,biomass=stock(object))  prdFn(model(object),params(object),biomass))
setMethod( 'computePrd', signature(object='biodyn',   biomass='numeric'),     function(object,biomass)                prdFn(model(object),params(object),biomass))
setMethod( 'computePrd', signature(object='biodyn',   biomass='FLQuant'),     function(object,biomass)                prdFn(model(object),params(object),biomass))


# calcLogLik

calcSigma <- function(obs,hat=rep(1,length(obs)),error='log'){
  yrs=dimnames(obs)$year
  yrs=yrs[yrs %in% dimnames(hat)$year]
  hat=hat[,yrs]
  obs=obs[,yrs]
  
  if (error=='log'){
    hat=log(hat)
    obs=log(obs)}
  
  SS =sum((obs-hat)^2,na.rm=T)
  
  return((SS/length(hat))^.5)}

loglFn<-function(obs,se,hat=rep(0,length(obs))){
  flag=!is.na(obs) & !is.na(hat)
  obs =obs[flag]
  hat =hat[flag]
  
  SS<-sum((obs-hat)^2)
  
  n   <-length(obs)
  res <-(log(1/(2*pi))-n*log(se)-SS/(2*se^2))/2
  
  return(res)}

calcLogLik<-function(obs,hat=rep(0,length(obs)),error='log',type=1){
  
  yrs=dimnames(obs)$year
  yrs=yrs[yrs %in% dimnames(hat)$year]
  hat=hat[,yrs]
  obs=obs[,yrs]
  
  if (error=='log'){
    hat=log(hat)
    obs=log(obs)}
  
  se<-calcSigma(obs,hat)
  
  if (type==1) return(loglFn(se,obs,hat)) else
    if (type==2) return(-sum(dnorm(obs, hat, se, log=(error=='log')))) else
      if (type==3) return(sum((obs-hat)^2))}
