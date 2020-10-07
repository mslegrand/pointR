
widgetDB<-reactiveVal(
  initialWidgetDB()
)

removePageWidgetDB<-function(pageId){
  stopifnot('tabId' %in% names(widgetDB()))
  db<-widgetDB()
  db<-filter(db, tabId!=pageId)
  widgetDB(db)
}


type2WidgetChoices<-function(colType){
 if(!is.null(colType)){
   choices<-list(
     point=c('radio','picker'),
     character=c('radio','picker'), #'switch', 'toggle'),
     character.list= c('radio','picker'), #, "multiInput", 'picker', 'checkbox'), #range
     character.list.2= c('picker','slider','radio'), #, "multiInput", 'picker', 'checkbox'), #range
     character.list.vec= c('picker','radio'), #, "multiInput", 'picker', 'checkbox'), #range
     #percentage, percentage.list.2
     integer=c('radio','picker','slider',  "numeric"), #,'knob'
     numeric=c('radio','picker','slider',  "numeric"), #,'knob'
     numeric.list=c('radio','picker'), #,'slider',  "numeric"), #,'knob'
     numeric.list.2=c('slider'), #,'knob'
     integer.list.2=c('slider'),
     numeric.list.vec=c('radio','picker'), #,'slider',  "numeric"), #,'knob'
     integer.list.vec=c('radio','picker'), #,'slider',  "numeric"),
     integer.list=c('radio','picker'),
     colourable=c('colourable','radio','picker' ) , #'spectrum', 'colorSelectorInput' ),
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


getPageWidgetDB<-function(pageId ){
  db<-widgetDB()
  filter(db, tabId==pageId )
}

# called soley by getWidget: returns a single row from the widgetDB
getRowWidgetDB<-reactive({ 
  log.fin(getRowWidgetDB)
  pageId<-  input$pages
  if(length(pageId>0)){
      wdb<-widgetDB()
      tibName<-getAssetName()
      colName<-getTibColumnName()
      row<-filter(wdb, tabId==pageId & name==tibName & column==colName)
  
      if(nrow(row)!=1){ #something is messed up: not there or multiple occurances
        # begin patch
        if(nrow(row)>1){ # remove multiple occurances
          wdb<-filter(wdb, !(tabId==pageId & name==tibName & column==colName))
        } 
        # and add back a default
        colType<-getColumnType()  
        widgets<-getWidgetChoices()
        chosenWidget<-widgets[1]
        wdb<-add_row(wdb, 
                     tabId=pageId,    name=tibName, 
                     column=colName,  type=colType,
                     minVal=NA, maxVal=NA, 
                     step=1, selectedWidget=chosenWidget
        )
        widgetDB(wdb)
        row<-filter(wdb, tabId==pageId & name==tibName & column==colName)
      } # end of patch
    log.fout(getRowWidgetDB)
  row
  } 

})


# TODO: populate handler with rows as needed: newPage or tabChange or ...
# TODO: rewrite to update just minVal or maxVal or step or selectedWidget
updateWidgetChoicesRow<-function(#tibName, colName, colType, 
  minVal=NA, maxVal=NA, step=1, selectedWidget='radio'){ # use current tib and col
  
  # can we really trust the following?
  pageId<-  input$pages
  tibName<-getAssetName()
  colName<-getTibColumnName()
  
  
  log.fin(updateWidgetChoicesRow)
  if(length(pageId)>0){
    
    wdb<-widgetDB()
    rowNo<-which(
        wdb$tabId==pageId & 
        wdb$name==tibName & 
        wdb$column==colName
    ) 
    if(length(rowNo)==1){ #not much changes, just replace selected (assuming selected in colVal)
      nn<-names(match.call()[-1])
      for(n in nn){
        wdb[[n]][rowNo]<-get(n)
      }
    } else { # not there, or multiple rows?
      widgets<-getWidgetChoices()
      chosenWidget<-selectedWidget #kludge to avoid name clash
      if(!chosenWidget %in% widgets){
        chosenWidget<-widgets[1]
      }
      tmp<-wdb[!(wdb$tabId==pageId & wdb$name==tibName & wdb$column==colName),] #remove the row  why?
      wdb<-add_row(wdb, tabId=pageId, name=tibName, column=colName, 
                   type=colType , minVal=minVal, maxVal=maxVal, step=step, selectedWidget=chosenWidget)
    }  
    widgetDB(wdb)
    log.fout(updateWidgetChoicesRow)
  }
} 



getWidgetChoices<-reactive({
  colType<-getColumnType()
  widgetChoices<-type2WidgetChoices(colType)
  tabId<-  getTibTabId()
  tibName<-getAssetName()
  colName<-getTibColumnName()
  scriptName<-getPreProcScriptName(tab_Id=tabId, tib_Name=tibName, column_Name=colName)
  if(getPlotState()=='value' && 	!is.null(scriptName) ){
        widgetChoices<- c( "immutable", widgetChoices)
  }
  widgetChoices
})

# called by 
#  serverEdTib init (line 33) 
#  then moduleEdTib (lines 108, 128), bothconditon by getTibEditState()==TRUE
getWidget<-reactive({
  rtv<-getRowWidgetDB()$selectedWidget
  rtv
})

# getWidgetVal<-reactive({
#   tabId<-input$pages
#   # row<-filter(handler$choices, tabId==tabId, name==getAssetName(), column==getTibColumnName())
#   row<-filter(widgetDB(), tabId==tabId & name==getAssetName() & column==getTibColumnName())
# })

getPointMax<-reactive({
  selectedTabId<-getTibTabId()

  colMax<-filter(widgetDB(), 
              tabId== getTibTabId() & 
              name == getAssetName()& 
              column==getTibColumnName()
  )$maxVal
  
  if(length(colMax)==0 ){ #or length(colMax)!=1
    NA
  } else {
    colMax
  }
})


