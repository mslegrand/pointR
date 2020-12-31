
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
     integer=c('picker','slider',  "numeric"), #'radio','knob'
     numeric=c('picker','slider',  "numeric"), #,'knob'
     numeric.list=c('picker'), #,'slider',  "numeric"), #'radio',,'knob'
     numeric.list.2=c('slider'), #,'knob'
     integer.list.2=c('slider'),
     numeric.list.vec=c('picker'), #,'slider',  "numeric"), #'radio',,'knob'
     integer.list.vec=c('picker'), #,'slider',  "numeric"), #radio',
     integer.list=c('picker'), #'radio',
     colourable=c('colourable','radio','picker' ) , #'spectrum', 'colorSelectorInput' ),
     other=c('picker'), #'radio',
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
  row<-NULL
  if(length(pageId>0)){
      wdb<-widgetDB()
      cat('widgetDB\n')
      print(wdb)
      tibName<-getAssetName()
      log.val(tibName)
      colName<-getTibColumnName()
      log.val(colName)
      row<-filter(wdb, tabId==pageId & name==tibName & column==colName)
      cat('row\n')
      print(row)
      if(nrow(row)!=1){ #something is messed up: not there or multiple occurances
        # begin patch
          if(nrow(row)>1){ # remove multiple occurances
            wdb<-filter(wdb, !(tabId==pageId & name==tibName & column==colName))
          }
          cat("wdb\n")
          print(wdb)
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
  } 
  row
})


# TODO: populate handler with rows as needed: newPage or tabChange or ...
# TODO: rewrite to update just minVal or maxVal or step or selectedWidget
updateWidgetChoicesRow<-function(#tibName, colName, colType, 
  minVal=NA, maxVal=NA, step=1, selectedWidget){ # use current tib and col
  log.fin(updateWidgetChoicesRow)
  
  # can we really trust the following?
  pageId<-  input$pages
  tibName<-getAssetName()
  colName<-getTibColumnName()
  log.fin(selectedWidget)
  log.val(pageId)
  log.val(tibName)
  log.val(colName)
  
  if(length(pageId)>0){

    wdb<-widgetDB()
    cat('length(',pageId,')>0\n')
    print(wdb)
    rowNo<-which(
        wdb$tabId==pageId & 
        wdb$name==tibName & 
        wdb$column==colName
    )     
    isCS<-FALSE
    if(length(aux$colChoiceSet)>0 && selectedWidget %in% names(aux$colChoiceSet)){
      columnValues<-getTib() %$$%  getTibColumnName()
      valueChoices<-aux$colChoiceSet[[selectedWidget]]
      isCS<-(length(valueChoices)>0 && length(setdiff(columnValues, valueChoices))==0 )
      if(!isCS){
        cat('if!isCS')
        log.val(selectedWidget)
        print(columnValues)
        print(valueChoices)
        
      }
    }
    log.val(isCS)
    if(length(rowNo)==1 && !isCS){ #not much changes, just replace selected (assuming selected in colVal)
      cat('xxx--\n')
      nn<-names(match.call()[-1])
      for(n in nn){
        wdb[[n]][rowNo]<-get(n)
      }
    } else { # not there, or multiple rows?
      cat('yyy--\n')
      widgets<-getWidgetChoices()
      log.val(widgets)
      chosenWidget<-selectedWidget #kludge to avoid name clash
      log.val(chosenWidget)
      if(!isCS && !chosenWidget %in% widgets){
        cat('should not happen\n')
        chosenWidget<-widgets[1]
      }
      log.val(chosenWidget)
      if(isCS){
        colType<-'choiceSet'
        log.val(colType)
      }
      tmp<-wdb[!(wdb$tabId==pageId & wdb$name==tibName & wdb$column==colName),] #remove the row  why?
      wdb<-add_row(tmp, tabId=pageId, name=tibName, column=colName, 
                   type=colType , minVal=minVal, maxVal=maxVal, step=step, selectedWidget=chosenWidget)
    }  
    print(wdb)
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
  cs<-getCompatibleChoicesSets()
  widgetChoices<-c(widgetChoices, cs)
  log.val(widgetChoices)
  scriptName<-getPreProcScriptName(tab_Id=tabId, tib_Name=tibName, column_Name=colName)
  if(getPlotState()=='value' && 	!is.null(scriptName) ){
        widgetChoices<- c( "immutable", widgetChoices)
  }
  widgetChoices
})

# called by 
#  serverEdTib init (line 33) 
#  then moduleEdTib (lines 108, 128), both by conditon: getTibEditState()==TRUE
getWidget<-reactive({
  log.fin(getWidget)
  log.fin(getTibTabId())
  cat('getWidget returning\n')
  db<-getRowWidgetDB()
  print(db)
  if(nrow(getRowWidgetDB())==1){
    rtv<-getRowWidgetDB()$selectedWidget
  }else {
    rtv<-NULL
  }
  print(rtv)
  log.fin(getWidget)
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


