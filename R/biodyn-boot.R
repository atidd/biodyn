#' boot, Bootstraps biodyn.
#'
#' @description 
#' Bootstraps the aspic model
#'
#' @param object; a \code{biodyn} object 
#' @seealso \code{\link{biodyn},\link{boot},\link{jk}}
#' 
#' @aliases boot-method boot,biodyn-method
#' 
#' @export
#' 
#' @rdname boot
#' @examples
#' \dontrun{
#'     data(asp)
#'     asp=boot(asp)}
setGeneric('boot',  function(object,...)  standardGeneric('boot'))

setMethod('boot', signature(object='biodyn'),
          function(object,run=TRUE)
            bootFn(object=object, n=500, run=run))

bootFn<-function(object=object, n=500, run=TRUE){
   dgs=object@diags
   
   fn<-function(x){   
     hat=as.FLQuant(transform(dgs,data=hat)[,     c("data","year")])
     rsd=as.FLQuant(transform(dgs,data=residual)[,c("data","year")])
   
     smp=FLQuant(sample(c(rsd),n*dim(rsd)[2],replace=TRUE),
             dimnames=list(year=dimnames(rsd)$year,iter=seq(n)))
   
     hat*exp(smp)}
   
   FLQuants(dlply(dgs,.(.id),fn))}
   

