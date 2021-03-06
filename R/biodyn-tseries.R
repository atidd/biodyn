#' tseries
#' @description A utility method to create a data.frame with time series of Performance Measures i.e.
#' stock biomass, SSB, recruitment, F, harvest rate
#'            
#' @param object \code{biodyn} 
#' @param refpts \code{FLBRP}
#' @param ... other arguments
#' 
#' @return a \code{data.frame} 
#'  
#' @aliases tseries,FLStock,missing  
#'  
#' @rdname tseries
#'  
#' @examples
#'  \dontrun{tseries(ple4,FLBRP(ple4))}
#'    
setGeneric('tseries',function(object,refpts,...)    standardGeneric('tseries'))

tseriesFn1=function(object){
  
  res=FLQuants(stock     =stock(object)%/%bmsy(object),
               catch     =catch(object)%/%msy( object),
               harvest   =harvest(object)%/%fmsy(object))
  
  model.frame(res,drop=T)}

tseriesFn2=function(object,brp,proxy='msy'){
  
  res=FLQuants(stock  =stock(object)%/%FLBRP::refpts(brp)[proxy,   'biomass'],
               ssb    =ssb(  object)%/%FLBRP::refpts(brp)[proxy,   'ssb'],
               rec    =rec(  object)%/%FLBRP::refpts(brp)[proxy,   'rec'],
               stockV =stock(object)%/%FLBRP::refpts(brp)["virgin",'biomass'],
               ssbV   =ssb(  object)%/%FLBRP::refpts(brp)["virgin",'ssb'],
               recV   =rec(  object)%/%FLBRP::refpts(brp)["virgin",'rec'],
               catch  =catch(object)%/%FLBRP::refpts(brp)[proxy,   'yield'],
               
               ageS   =apply(stock.n(object)*mat(object)*ages(stock.n(object)),c(2,6),sum)%/%
                              apply(stock.n(object)*mat(object),c(2,6),sum),
               ageB   =apply(stock.n(object)*ages(stock.n(object)),c(2,6),sum)%/%
                              apply(stock.n(object),c(2,6),sum),
               ageC   =apply(catch.n(object)*ages(catch.n(object)),c(2,6),sum)%/%
                              apply(catch.n(object),c(2,6),sum),
               
               wtS   =apply(stock.wt(object)*stock.n(object)*mat(object),c(2,6),sum)%/%
                              apply(stock.n(object)*mat(object),c(2,6),sum),
               wtB   =apply(stock.wt(object)*stock.n(object),c(2,6),sum)%/%
                              apply(stock.n(object),c(2,6),sum),
               wtC   =apply(catch.wt(object)*catch.n(object),c(2,6),sum)%/%
                              apply(catch.n(object),c(2,6),sum),
               
               fbar   =fbar( object)%/%FLBRP::refpts(brp)[proxy,   'harvest'],
               harvest=(catch(object)/stock(object))%/%(FLBRP::refpts(brp)[proxy,'yield']/FLBRP::refpts(brp)[proxy,'biomass']))
  
  model.frame(res,drop=T)}
# 
# setMethod('tseries', signature(object='FLStock',refpts='FLBRP'), 
#   function(object,refpts,proxy='msy',...) {
# 
#   res=tseriesFn2(object,refpts,proxy)
#     
#   return(res)})
setMethod('tseries', signature(object='FLStock',refpts='missing'), 
          function(object,refpts,...) {
            
            res=tseriesFn1(object)
            
            return(res)})

setMethod('tseries', signature(object='FLStock',refpts='FLBRP'), 
          function(object,refpts,...) {
            
            res=tseriesFn2(object,refpts)
            
            return(res)})
