guessK=function(r,catch,p=1,ratio=0.5)
  1/ratio*mean(catch)*p/(r*(1-(ratio)^p))

#' biodyn constructor
#' 
#' @name biodyn

#' @description Creates an object of the biodyn class representing a biomass dynamic stock assessment model.
#' 
#' @param ...  named parameter being passed to slots
#' 
#' @return biodyn object
#' 
#' @aliases biodyn-method biodyn,ANY-method  biodyn,ANY,ANY-method biodyn,FLBRP,FLStock-method
#' 
#' @export
#' @examples 
#' \dontrun{
#' bd=biodyn(params=FLPar(r=0.6,k=50000,p=1,b0=1))
#' }
setGeneric('biodyn',   function(object,y,...)  standardGeneric('biodyn'))
setMethod('biodyn', signature(),
    function(model="pellat",
             catch=NA,
             r=0.5,p=1,k=guessK(r=r,catch=catch,p=p),b0=0.75,
             min=.1,max=10,...){
            
      model=tolower(model)
            
      args = list(...)

      res=new("biodyn")

      # Load given slots
      for(i in names(args))
        slot(res, i) = args[[i]]
     
      if ("FLQuant"%in%is(catch))
        res@catch=catch
    
      if (!("params" %in% names(args))){
        res@params=rbind(FLPar(r =FLPar(r),
                               k =FLPar(k),
                               p =FLPar(p),
                               b0=FLPar(b0)))  
        }
        
      res@control=propagate(res@control,dims(res@params)$iter)
      nms=dimnames(res@control)$param[dimnames(res@control)$param %in% dimnames(res@params)$param]
      res@control[nms,  'val']=res@params[nms,]
      res@control[nms,  'min']=res@params[nms,]*min
      res@control[nms,  'max']=res@params[nms,]*max
      res@control[c("b0","p"),'phase']=-1
             
      range(res)=unlist(dims(catch(res))[c("minyear","maxyear")])

      if (!("stock"%in%names(args)))
        res@stock=FLQuant(NA,dimnames=list(year= range(res)["minyear"]:(range(res)["maxyear"]+1)))

      res=fwd(res,catch=res@catch)

      return(res)})

setMethod('biodyn', signature(object='FLBRP',y='FLStock'),
          function(object,y,model="pellat",min=.1,max=10,msy=NULL,r=NULL,...){
        
        #if (!(is(y)%in%"FLBRP")) return(NULL)
          
        res=FLBRP2biodyn(object)
        res=fwd(res,catch=catch(y)[,-1])
        
        res})
            
            
###########################################
setGeneric('biodyn.',   function(model,params,...)  standardGeneric('biodyn.'))
setMethod('biodyn.', signature(model='factor',params='FLPar'),
          function(model,params,min=0.1,max=10,catch=NULL,stock=NULL,msy=NULL,...){
            model=tolower(model)
            if (is.null(msy) & !is.null(catch)) 
              msy=mean(catch,na.rm=TRUE)
            
            args = list(...)
            
            dimnames(params)$params=tolower(dimnames(params)$params)
            
            if (!('b0' %in%  dimnames(params)$params)) 
              params=rbind(params,propagate(FLPar('b0'=1),dims(params)$iter))
            
            if (model=='pellat' & !('p' %in%  dimnames(params)$params)) 
              params=rbind(params,propagate(FLPar('p'=1),dims(params)$iter))
            
            if (!('k' %in%  dimnames(params)$params))
              if (!is.null(msy) & model=='pellat') 
                params=rbind(params,'k'=FLPar(K(msy,params)))
            else  
              params=rbind(params,'k'=FLPar(k=as.numeric(NA)))
            
            if (model=='pellat')
              params=params[c('r','k','p','b0'),]
            res        =biodyn()
            
            if (!('factor' %in% is(model)))
              model=factor(model)
            res@model  =model
            res@params =params 
            
            if (!is.null(stock))
              res@stock[]=params(res)['k']*params(res)['b0']
            
            if (!is.null(catch)){
              res@catch=catch
              
              res=fwd(res,catch=catch)
            }
            else  if (!is.null(stock)) {
              res@stock=stock
              
              res@catch=window(stock,end=dims(stock)$maxyear-1)
              res@catch[]=NA}
            
            res@control=propagate(res@control,dims(params)$iter)
            nms=dimnames(res@control)$param[dimnames(res@control)$param %in% dimnames(res@params)$param]
            res@control[nms,  'val']=res@params[nms,]
            res@control[nms,  'min']=res@params[nms,]*min
            res@control[nms,  'max']=res@params[nms,]*max
            
            if (!('b0' %in% nms))
              res@control['b0',c('min','max','val')]=c(0.75,1,1)
            
            # Load given slots
            for(i in names(args))
              slot(res, i) = args[[i]]
            
            return(res)})

setMethod('biodyn.', signature(model='character',params='FLPar'),
          function(model,params,min=0.1,max=10,catch=NULL,stock=NULL,...) 
            biodyn(model=factor(model,levels=models),params,min=min,max=max,catch=catch,stock=stock,...))

setMethod('biodyn.', signature(model='factor',params='missing'),
          function(model,params,min=min,max=max,catch=NULL,stock=NULL,...){
            
            args = list(...)
            
            res        =biodyn()
            res@model  =model
            
            nms=c(biodyn:::modelParamsmodelParams(model),'b0')
            par=rep(NA,length(nms))
            names(par)=nms
            
            res@params =FLPar(par) 
            res@params['b0']=1
            
            if (!is.null(stock))
              res@stock[]=params(res)['k']*params(res)['b0']
            
            if (!is.null(catch))
              res@catch=catch
            else  if (!is.null(stock)) {
              res@catch=window(res@stock,end=dims(res@catch)$maxyear-1)
              res@catch[]=NA}
            
            # Load given slots
            for(i in names(args))
              slot(res, i) = args[[i]]
            
            return(res)})

setMethod('biodyn.', signature(model='character',params='missing'),
          function(model=model,min=0.1,max=10.0,catch=NULL,index=NULL,stock=NULL,...) 
            biodyn(model=factor(model,levels=models),min=min,max=max,catch=catch,stock=stock,...))

setMethod('biodyn.', signature(model='missing',params='missing'),
          function(model,params,min=0.1,max=10.0,msy=NULL,...) {
            args = list(...)
         
            res=new('biodyn')
            
            # Load given slots
            for(i in names(args))
              slot(res, i) = args[[i]]
            
            range(res)=unlist(dims(catch(res))[c("minyear","maxyear")])
            
            return(res)})


#' is.biodyn
#'
#' @description Checks class type and returns TRUE if object is of type biodyn
#' @param x biodyn class
#' 
#' @return TRUE or FALSE
#' 
#' @export
#' @examples
#' \dontrun{
#'  is.biodyn(biodyn()) 
#'  }
is.biodyn = function(x)
  return(inherits(x, 'biodyn'))
