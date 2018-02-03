observe({input$messageFromAce
  isolate({
    #cat('serverAce:...observe input$messageFromAce:: entering\n')
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      
      request$code<-input$messageFromAce$code
      request$sender<-input$messageFromAce$sender
      
      if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code) ){
        reqSelector<-input$messageFromAce$selector
        updateSelected4Ace(reqSelector)
      }
      
      if(length(input$messageFromAce$dirty)>0){
        editOption$.saved <- !(as.numeric(input$messageFromAce$dirty) > 0)
      }
      if(request$sender %in% c('cmd.commit','cmd.openFileNow', 'cmd.saveFileNow', 'cmd.file.new' )){
        
        if(request$sender=='cmd.commit' && !is.null(getTibName())){ 
          name=getTibName()
        } else { 
          name=NULL
        }
        tibs<-getPtDefs()$tib
        resetSelectedTibbleName(tibs=tibs, name=name)
        processCommit()
      } 
      # if( request$sender %in% 'cmd.openFileNow'){
      #   # !!! TODO: set point.index to end of points (if points)
      # }
      
      if(request$sender %in% 'cmd.saveFileNow'){
        datapath<-input$messageFromAce$auxValue
        txt<-input$messageFromAce$code
        writeLines(txt, datapath)
        setCurrentFilePath(datapath)
        editOption$currentFile<-basename(datapath)
        editOption$currentDirectory<-dirname(datapath)
        session$sendCustomMessage(
          type = "shinyAceExt",
          list(id= "source", setClean=TRUE, sender='cleanPlease')
        )
        
      }
    }
  })
})

updateAceExtDef<-function(newPtDef, sender, selector=list() ){

  newPtDef$tib<-pts2Integers(newPtDef$tib )
  
  replacementList<-ptDef2ReplacementList(name, newPtDef, getCode() )
  if( length(replacementList)>0 ){
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= "source", replacement=replacementList, selector=selector, sender=sender, ok=1)
    )
  }
}

updateAceExt<-function(sender, ...){
  data<-as.list(...)
  if(length(data)>0){
    data<-c(list(id=='source', sender=sender), data )
    session$sendCustomMessage(
      type = "shinyAceExt",
      data
    )
  }
}

observe({
  request$sender
  isolate({
    if(request$sender=='startup'){
      cat("request: startup cmdFileNew")
      cmdFileNew()
    }
  })
})

# TODO!!!: rewrite
updateSelected4Ace<-function( reqSelector){
  if(!is.null(reqSelector[['name']])){
    selectedTibble$name=reqSelector[['name']]
  }
  if(!is.null(reqSelector[['ptColName']])){
    selectedTibble$ptColName=reqSelector[['ptColName']]
  }
  if(!is.null(reqSelector[['rowIndex']])){ # !!! may want to provide a check here
    selectedTibble$rowIndex=reqSelector[['rowIndex']]
  }
  if(!is.null(reqSelector[['matCol']])){
    selectedTibble$matCol=reqSelector[['matCol']]

  }
  if(!is.null(reqSelector[['columnName']])){
    selectedTibble$columnName=reqSelector[['columnName']]
  }
  
} 

