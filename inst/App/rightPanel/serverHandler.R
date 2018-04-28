

handler<-reactiveValues(
  choices=tibble(tabId='Tab0', name='x',column='y',type='character',minVal=NA, maxVal=NA,step=1, selectedWidget=NA)[0,]
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
  # cat('\n=================Entering updateWidgetChoicesRow=============\n')
  # cat( "initial value of handler$choices\n")
  # cat("nrow(handler$choices)=",nrow(handler$choices),"\n")
  # print(handler$choices)
  # cat('input value of selectedWidget is', format(selectedWidget),'\n')
  tabId<-input$pages
  tibName<-getTibName()
  colName<-getTibColumnName()
  colType<-getColumnType()
  
  # cat("tabId=",format(tabId)," tibName=",tibName," colName=",colName, " colType=",colType, "\n")
  if(length(tabId)>0){
    
    rowNo<-which(
        handler$choices$tabId==tabId & 
        handler$choices$name==tibName & 
        handler$choices$column==colName
    ) # & handler$choices$type==colType)
    # cat("rowNo=",format(rowNo),"\n")
    # cat("length(rowNo)=",length(rowNo),"\n")
    if(length(rowNo)>0){ #not much changes, just replace selected (assuming selected in colVal)
      # cat("branch: length(rowNo)>0\n")
      nn<-names(match.call()[-1])
      # cat(paste(nn,collapse=", "),"\n")
      for(n in nn){
        # cat(format(n),"=",get(n),"\n")
        handler$choices[[n]][rowNo]<-get(n)
      }
    } else { #remove the row
      # cat("branch: length(rowNo)==0\n")
      # cat( "1 printing handler$choices")
      # print(handler$choices)
      widgets<-type2WidgetChoices(colType)
      
      tmp<-handler$choices[!(handler$choices$tabId==tabId & handler$choices$name==tibName & handler$choices$column==colName),]
      # cat( "1 printing tmp")
      # print(tmp)
      handler$choices<-add_row(tmp, tabId=tabId, name=tibName, column=colName,  minVal=minVal, maxVal=maxVal, step=step, selectedWidget=selectedWidget)
      # cat( "2 pringing final handler$choices")
      # print(handler$choices)
    }
  }
} 



getWidgetChoices<-reactive({
  colType<-getColumnType()
  widgetChoices<-type2WidgetChoices(colType)
})

getWidget<-reactive({
  # cat('entering getWidget\n')
  # cat("handler$choices")
  # print(handler$choices)
  widgets<-getWidgetChoices()
  widget<-widgets[1]
  colName<-getTibColumnName()
  columnValues<-getTib()[[colName]]
  
  #tabId<-input$pages
  #colType<-extractColType(columnValues)
  # cat("\ngetWidget:: tabId=", getTibTabId() , "name=", getTibName(), "column=",getTibColumnName() )
  row<-filter(handler$choices, tabId==getTibTabId() , name==getTibName(), column==getTibColumnName())
  # cat("getWidget:: nrow=",nrow(row),"\n")
  if(nrow(row)==1 ){
    widget<-row$selectedWidget
    # cat('getWidget:: found widget=',widget,"\n")
  } else {
    # cat('getWidget:: widget not found')
    # cat('getWidget:: 1 default widget=',widget,"\n")
  } 
  if( !(widget %in% widgets) ){
    widget<-widgets[1] # or 'radio'
    # cat('getWidget:: 2 default widget=',widget,"\n")
  }
  return(widget)
})

getWidgetVal<-reactive({
  tabId<-input$pages
  row<-filter(handler$choices, tabId==tabId, name==getTibName(), column==getTibColumnName())
})

getPointMax<-reactive({
  # cat('\n---Entering -getPointMax---------\n')
  selectedTabId<-getTibTabId()
  # cat('tabId=',format(selectedTabId),"\n")
  # cat('handler$choices=\n')
  # print(handler$choices)
  colMax<-filter(handler$choices, tabId== getTibTabId() , name==getTibName(), column==getTibColumnName())$maxVal
  # cat('colMax=',format(colMax),"\n")
  # cat("length(colMax)=",length(colMax),"\n")
  if(length(colMax)==0 ){ #or length(colMax)!=1
    NA
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
