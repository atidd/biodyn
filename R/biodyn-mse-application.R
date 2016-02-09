utils::globalVariables(c('fwdWindow','interval', 'nits',
                         'pMeasure',
                         'dbWriteTable'))

#' runMSE
#' @description Runs a full MSE using an \code{FLStock} object as the Operating Model and \code{biodyn} as the Mangement Procedure
#'           
#' @aliases runMSE
#' 
#' @param om an \code{FLStock} object 
#' @param eql an \code{FLBRP} object that holds the biological parameters for use in the projections
#' @param mp an \code{biodyn} object that holds the options for the biomass dynamic assessment model
#' @param range a \code{vector} the starting and end years for the projections, and the interval for running the MP
#' @param srDev  a \code{FLQuant} with recruitment deviates
#' @param uDev an \code{FLQuant} or \code{FLQuants} with CPUE residuals
#' @param ftar a \code{numeric} with target F in HCR
#' @param fmin a \code{numeric} with minimum F in HCR
#' @param blim a \code{numeric} with biomass limit for HCR
#' @param btrig a \code{numeric} with biomass trigger (i.e. when to reduce F) in HCR 
#' @param what a \code{character} that specifies what is to be used for the reference point in the HCR, recycled as required
#' @param mult a \code{logical} that specifies whether quantity in HCR options is a multiplier or probability, recycled as required
#'
#' @return  a list of \code{data.frame}s with performance measures from OM and summaries from MP, if \code{con!=NULL} will
#' also write to a MYSQL database
#'  
#' @export
#' @rdname runMSE
#' 
#' @seealso \code{\link{biodyn}}
#' 
#' @examples
#' \dontrun{
#' sim()
#'    }
runMSE=function(om,eql,mp,
                srDev,uDev,
                ftar=0.75,fmin=0.01,blim=0.8,btrig=0.4,
                what="msy",
                mult=TRUE,
                range=c(range(om)["maxyear"],
                        range(om)["maxyear"]+30,interval=3)){
  
  names(range)=c("min","max","interval")
  
  ## OM projections
  om  =fwdWindow(om,end=range["max"],eql)
  nits=dims(srDev)$iter
  
  ## F in longterm
  lgt=FLQuant(c(FLBRP::refpts(eql)['msy','harvest']*hcr['ftar']),
                         dimnames=list(year=range["min"]:range["max"],iter=seq(nits)))
  
  ## Add stochastcity
  om =fwd(om,f=lgt, sr=eql,sr.residuals=srDev)
  
  ## save projection for comparison
  prj=om
  
  res=mseBiodyn(om,eql,
                srDev=srDev,
                uDev =uDev,
                control=control(mp),
                priors =mp@priors,
                start   =range[1],
                end     =range[2],
                interval=range[3],
                ftar =hcr['ftar'],
                fmin =hcr['ftar'],
                blim =hcr['blim'],
                btrig=hcr['btrig'],
                what =what,
                mult =mult)
    
  res}  
