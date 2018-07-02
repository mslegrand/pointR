

handler<-reactiveValues(
  choices=tibble(tabId='Tab0', name='x',column='y',type='character',minVal=NA, maxVal=NA,step=1, selectedWidget=NA)[0,]
)

type2WidgetChoices<-function(colType){
  
    
 if(!is.null(colType)){
   choices<-list(
     point=c('radio','picker'),
     character=c('radio','picker'), #'switch', 'toggle'),
     character.list= c('radio','picker'), #, "multiInput", 'picker', 'checkbox'), #range
     character.list.2= c('slider','radio','picker'), #, "multiInput", 'picker', 'checkbox'), #range
     character.list.vec= c('radio','picker'), #, "multiInput", 'picker', 'checkbox'), #range
     integer=c('radio','picker','slider',  "numeric"), #,'knob'
     numeric=c('radio','picker','slider',  "numeric"), #,'knob'
     numeric.list=c('radio','picker'), #,'slider',  "numeric"), #,'knob'
     numeric.list.2=c('slider'), #,'knob'
     integer.list.2=c('slider'),
     numeric.list.vec=c('radio','picker'), #,'slider',  "numeric"), #,'knob'
     integer.list.vec=c('radio','picker'), #,'slider',  "numeric"),
     colourable=c('radio','picker', 'colourable') , #'spectrum', 'colorSelectorInput' ),
     other=c('radio','picker'),
     other.list=c('radio','picker')
     )[[colType]]   
 } else {
   choices<-NULL
 }
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
  tabId<-input$pages
  tibName<-getAssetName()
  colName<-getTibColumnName()
  colType<-getColumnType()
  
  if(length(tabId)>0){
    
    rowNo<-which(
        handler$choices$tabId==tabId & 
        handler$choices$name==tibName & 
        handler$choices$column==colName
    ) 
    if(length(rowNo)>0){ #not much changes, just replace selected (assuming selected in colVal)
      nn<-names(match.call()[-1])
      for(n in nn){
        handler$choices[[n]][rowNo]<-get(n)
      }
    } else { #remove the row
      widgets<-type2WidgetChoices(colType)
      
      tmp<-handler$choices[!(handler$choices$tabId==tabId & handler$choices$name==tibName & handler$choices$column==colName),]
      handler$choices<-add_row(tmp, tabId=tabId, name=tibName, column=colName,  minVal=minVal, maxVal=maxVal, step=step, selectedWidget=selectedWidget)
    }
  }
} 



getWidgetChoices<-reactive({
  colType<-getColumnType()
  widgetChoices<-type2WidgetChoices(colType)
})

getWidget<-reactive({
  # cat('entering getWidget\n')
  widgets<-getWidgetChoices()
  widget<-widgets[1]
  colName<-getTibColumnName()
  columnValues<-getTib()[[colName]]
  
  row<-filter(handler$choices, tabId==getTibTabId() , name==getAssetName(), column==getTibColumnName())
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
  }
  return(widget)
})

getWidgetVal<-reactive({
  tabId<-input$pages
  row<-filter(handler$choices, tabId==tabId, name==getAssetName(), column==getTibColumnName())
})

getPointMax<-reactive({
  # cat('\n---Entering -getPointMax---------\n')
  selectedTabId<-getTibTabId()
  colMax<-filter(handler$choices, tabId== getTibTabId() , name==getAssetName(), column==getTibColumnName())$maxVal
  if(length(colMax)==0 ){ #or length(colMax)!=1
    NA
  } else {
    colMax
  }
})


