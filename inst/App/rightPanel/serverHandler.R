

handler<-reactiveValues(
  choices=tibble(name='x',column='y',type='character',minVal=NA, maxVal=NA,step=1, selectedWidget=NA)[0,]
)

type2WidgetChoices<-function(colType){
  choices<-list(
         point=c('radio','picker'),
         character=c('radio','picker'), #'switch', 'toggle'),
         character.list= c('radio','picker'), #, "multiInput", 'picker', 'checkbox'), #range
         character.list.2= c('radio','picker'), #, "multiInput", 'picker', 'checkbox'), #range
         character.list.vec= c('radio','picker'), #, "multiInput", 'picker', 'checkbox'), #range
         integer=c('radio','picker','slider',  "numeric"), #,'knob'
         numeric=c('radio','picker','slider',  "numeric"), #,'knob'
         numeric.list=c('radio','picker','slider',  "numeric"), #,'knob'
         numeric.list.2=c('radio','picker','slider',  "numeric"), #,'knob'
         numeric.list.vec=c('radio','picker','slider',  "numeric"), #,'knob'
         colourable=c('radio','picker', 'colourable') , #'spectrum', 'colorSelectorInput' ),
         other=c('radio','picker'),
         other.list=c('radio','picker')
  )[[colType]]
  if(is.null(choices)){
    choices<-c('radio','picker')
  }
  choices  
}




# TODO: populate handler with rows as needed
# TODO: rewrite to update just minVal or maxVal or step or selectedWidget
updateWidgetChoicesRow<-function(#tibName, colName, colType, 
                                 minVal=NA, maxVal=NA, step=1, selectedWidget='radio'){ # use current tib and col
  #if( missing(tibName)|| missing(colName)){ stop("missing tibName or colName")}
  tibName<-getTibName()
  colName<-getTibColumnName()
  colType<-getColumnType()
  
  rowNo<-which(handler$choices$name==tibName & handler$choices$column==colName) # & handler$choices$type==colType)
  if(length(rowNo)>0){ #not much changes, just replace selected (assuming selected in colVal)
    nn<-names(match.call()[-1])
    for(n in nn){
      handler$choices[[n]][rowNo]<-get(n)
    }
  } else { #remove the row
    
    widgets<-type2WidgetChoices(colType)
    
    tmp<-handler$choices[!(handler$choices$name==tibName & handler$choices$column==colName),]
    handler$choices<-add_row(tmp, name=tibName, column=colName,  minVal=minVal, maxVal=maxVal, step=step, selectedWidget=selectedWidget)
  }
} 



getWidgetChoices<-reactive({
  colType<-getColumnType()
  widgetChoices<-type2WidgetChoices(colType)
})

getWidget<-reactive({
  #cat('entering getWidget\n')
  widgets<-getWidgetChoices()
  widget<-widgets[1]
  colName<-getTibColumnName()
  columnValues<-getTib()[[colName]]
  #colType<-extractColType(columnValues)
  row<-filter(handler$choices, name==getTibName(), column==getTibColumnName())
  if(nrow(row)==1 ){
    widget<-row$selectedWidget
  } 
  if( !(widget %in% widgets) ){
    widget<-widgets[1] # or 'radio'
  }
  return(widget)
})

getWidgetVal<-reactive({
  row<-filter(handler$choices, name==getTibName(), column==getTibColumnName())
})

getPointMax<-reactive({
  colMax<-filter(handler$choices, name==getTibName(), column==getTibColumnName())$maxVal
  if(length(colMax)==0 || is.na(colMax)){
    NULL
  } else {
    colMax
  }
})


# getHandler<-reactive({
#   # if(!is.null(colName)){
#   #   cat("serverSelection.R:: getHandler: colName=",colName,"\n")
#   #   if(is.null(columnValues)){
#   #     cat("columnValues is NULL")
#   #     print(getTib())
#   #   }
#   #   print(columnValues)
#   # }
#   # else{
#   #   cat("colName is NULL\n")
#   # }
#   
#   if(!is.null(columnValues)){
#     if(is.character(columnValues) && isColorString(columnValues)){
#       # cat('column is colourable\n\n')
#       return('colourable')
#     } else if (isPoints(columnValues)){
#       # cat('Column',colName,' is Points\n')
#       return('points')
#     }
#   }
#   NULL
# })

# setHandlerValue<-function(hValue){ # hValue==NULL iff is 'default'
#   handler<-getHandler()
#   # if(handler=='default') bail
#   if(is.null(handler)) {
#     return(NULL)
#   }
#   # if(!is.null(hValue) && hValue!=handler){
#   #   return(NULL)
#   # }
#   if(is.null(request$inputHandler)){
#     request$inputHandle<-list()
#   }
#   name<-getTibName()
#   colName<-getTibColumnName()
#   # if tibName not in request$inputHandler list, add it with vector as arg
#   if(is.null(request$inputHandler[[name]])){ 
#     request$inputHandler[[name]]<- list()
#   }
#   request$inputHandler[[name]][[colName]]<-hValue
# }


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
