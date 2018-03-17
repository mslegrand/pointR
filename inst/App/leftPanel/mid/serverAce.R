

observeEvent(input$messageFromAce, {
    # cat('serverAce:...observe input$messageFromAce:: entering\n')
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      
      request$code<-input$messageFromAce$code
      request$sender<-input$messageFromAce$sender
      clearErrorMssg()
      if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code) ){
        reqSelector<-input$messageFromAce$selector
        updateSelected4Ace(reqSelector)
      }
      
      if(length(input$messageFromAce$dirty)>0){
        editOption$.saved <- !(as.numeric(input$messageFromAce$dirty) > 0)
      }
      #cat('request$sender=',format(request$sender),"\n")
      if(request$sender %in% c('cmd.commit','cmd.openFileNow', 'cmd.saveFileNow', 'cmd.file.new', 'cmd.add.column')){
        #cat('getTibName()=',format(getTibName()),"\n")
        if(request$sender %in% c('cmd.commit', 'cmd.add.column') && !is.null(getTibName())){ 
          name=getTibName()
        } else { 
          name=NULL
        }
        tibs<-getPtDefs()$tib
        #cat('name=',format(name),"\n")
        processCommit()
        resetSelectedTibbleName(tibs=tibs, name=name)
        
      } 
      
      
      if(request$sender %in% 'cmd.saveFileNow'){
        datapath<-input$messageFromAce$auxValue
        txt<-input$messageFromAce$code
        writeLines(txt, datapath)
        setCurrentFilePath(datapath)
        editOption$currentFile<-basename(datapath)
        editOption$currentDirectory<-dirname(datapath)
        session$sendCustomMessage(
          type = "shinyAceExt",
          list(id= getAceEditorId(), setClean=TRUE, sender='cleanPlease')
        )
        
      }
    }
}, priority = 90, ignoreNULL = TRUE, ignoreInit = TRUE)

updateAceExtDef<-function(newPtDef, sender, selector=list() ){

  newPtDef$tib<-pts2Integers(newPtDef$tib )
  
  replacementList<-ptDef2ReplacementList(name, newPtDef, getCode() )
  if( length(replacementList)>0 ){
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= getAceEditorId(), replacement=replacementList, selector=selector, sender=sender, ok=1)
    )
  }
}

updateAceExt<-function(sender, ...){
  data<-as.list(...)
  if(length(data)>0){
    data<-c(list(id= getAceEditorId(), sender=sender), data )
    session$sendCustomMessage(
      type = "shinyAceExt",
      data
    )
  }
}

observeEvent(request$sender,{
    if(request$sender=='startup'){
      #cat('startup\n')
      cmdFileNew()
    }
}, priority=100)

# TODO!!!: rewrite
updateSelected4Ace<-function( reqSelector){
  
  if(!is.null(reqSelector[['name']])){
    #cat("reqSelector$name=", format(reqSelector$name ),"\n")
    selectedTibble$name=reqSelector[['name']]
  }
  if(!is.null(reqSelector[['ptColName']])){
    #cat("reqSelector$ptColName=", format(reqSelector$ptColName ),"\n")
    selectedTibble$ptColName=reqSelector[['ptColName']]
  }
  if(!is.null(reqSelector[['rowIndex']])){ # !!! may want to provide a check here
    #cat("reqSelector$ptColName=", format(reqSelector$rowIndex ),"\n")
    selectedTibble$rowIndex=reqSelector[['rowIndex']]
  }
  if(!is.null(reqSelector[['matCol']])){
    #cat("reqSelector$matCol=", format(reqSelector$matCol ),"\n")
    selectedTibble$matCol=reqSelector[['matCol']]

  }
  if(!is.null(reqSelector[['columnName']])){
    #cat("reqSelector$columnName=", format(reqSelector$columnName ),"\n")
    selectedTibble$columnName=reqSelector[['columnName']]
  }
  
} 

