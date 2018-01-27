
getHandlerValue<-reactive({ 
  handler<-getHandler()
  if(is.null(handler)){ #NULL is default
    return(NULL)
  }
  name<-getTibName()
  columnName<-getTibColumnName()
  
  hv<-request$inputHandler[[name]][[columnName]]
  
  if( !is.null(hv) && hv==handler){
    return(handler)
  } else {
    return(NULL)
  }
})

getHandler<-reactive({
  colName<-getTibColumnName()
  columnValues<-getTib()[[colName]]
  if(!is.null(colName)){
    cat("serverSelection.R:: getHandler: colName=",colName,"\n")
    if(is.null(columnValues)){
      cat("columnValues is NULL")
      print(getTib())
    }
    print(columnValues)
  }
  else{
    cat("colName is NULL\n")
  }
  
  if(!is.null(columnValues)){
    if(is.character(columnValues) && isColorString(columnValues)){
      cat('column is colourable\n\n')
      return('colourable')
    }
  }
  NULL
})

setHandlerValue<-function(hValue){ # hValue==NULL iff is 'default'
  handler<-getHandler()
  # if(handler=='default') bail
  if(is.null(handler)) {
    return(NULL)
  }
  if(!is.null(hValue) && hValue!=handler){
    return(NULL)
  }
  if(is.null(request$inputHandler)){
    request$inputHandle<-list()
  }
  name<-getTibName()
  colName<-getTibColumnName()
  # if tibName not in request$inputHandler list, add it with vector as arg
  if(is.null(request$inputHandler[[name]])){ 
    request$inputHandler[[name]]<- list()
  }
  request$inputHandler[[name]][[colName]]<-hValue
}


# #-- hidden output
# output$handler<-reactive({
#   rtv<-getHandler()
#   if(is.null(rtv)){
#     rtv<-'default'
#   }
#   rtv
# })
# 
# outputOptions(output, "handler", suspendWhenHidden=FALSE)
# 
# output$handlerValue<-reactive({
#   rtv<-getHandlerValue()
#   if(is.null(rtv)){
#     rtv<-'default'
#   }
#   rtv
# })
# outputOptions(output, "handlerValue", suspendWhenHidden=FALSE)
