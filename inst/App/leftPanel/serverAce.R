observe({input$messageFromAce
  isolate({
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      #cat('observe input$messageFromAce:: entering\n')
      request$code<-input$messageFromAce$code
      request$sender<-input$messageFromAce$sender
      cat('input$messageFromAce::sender=',request$sender,"\n")
      cat('nchar(code)=',nchar(request$code),"\n")
      
      if(length(input$messageFromAce$dirty)>0){
        editOption$.saved <- !(as.numeric(input$messageFromAce$dirty) > 0)
      }
      if(request$sender %in% c('cmd.commit','cmd.openFileNow', 'cmd.saveFileNow' )){
        processCommit()
      } 
      if( request$sender %in% 'cmd.openFileNow'){
        # !!! TODO: set point.index to end of points (if points)
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
          list(id= "source", setClean=TRUE, sender='cleanPlease')
        )
        
      }
    }
  })
})

updateAceExtDef<-function(newPtDef, sender ){
  #cat('sender=',sender,"\n")
  newPtDef$tib<-pts2Integers(newPtDef$tib )
  
  replacementList<-ptDef2ReplacementList(name, newPtDef, getCode() )
  if( length(replacementList)>0 ){
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= "source", replacement=replacementList, sender=sender, ok=1)
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
      cmdFileNew()
    }
    if(request$sender %in% c( "cmd.openFileNow", "cmd.newFile")){ #!!! check these names
      # get valid point name, then set index to last valid index. (length of points?)
      pd<-getPtDefs()
      if(length(pd)>0){
        tibs<-pd$tib #!!! check this
        name<-tail(names(tibs),1) # !!! KLUDGE, last name 
        resetSelectedTibbleName(name, tibs)
      }
    } 
    
  })
})
