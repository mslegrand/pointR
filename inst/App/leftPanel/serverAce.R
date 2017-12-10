observe({input$messageFromAce
  isolate({
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      #cat('observe input$messageFromAce:: entering\n')
      request$code<-input$messageFromAce$code
      request$sender<-input$messageFromAce$sender
      #cat('input$messageFromAce::sender=',request$sender,"\n")
      #cat('code=',nchar(request$code),"\n")
      if(length(input$messageFromAce$dirty)>0){
        editOption$.saved <- !(as.numeric(input$messageFromAce$dirty) > 0)
      }
      #processCommit()
      if(request$sender %in% c('cmd.commit','cmd.openFileNow', 'cmd.saveFileNow' )){
        processCommit()
      } 
      if( request$sender %in% 'cmd.openFileNow'){
        #set point.index to end of points (if points)
      }
      if(request$sender %in% 'cmd.saveFileNow'){
        #cat('observe {input$messageFromAce:: cmd.saveFileNow\n')
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
  
  #newPtDef<-olde2newFmtPtDef2ListOfLists(newPtDef)
  #cat('sender=',sender,"\n")
  newPtDef$tib<-pts2Integers(newPtDef$tib )
  
  replacementList<-ptDef2ReplacementList(name, newPtDef,getCode() )
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
      # displayOptions$insertMode=TRUE
      # displayOptions$showGrid=FALSE
      # displayOptions$ptMode="Normal"
      
      cmdFileNew()
    }
    if(request$sender %in% c( "cmd.openFileNow", "cmd.newFile")){ #!!! check these names
      # get valid point name, then set index to last valid index. (length of points?)
      pd<-getPtDefs()
      if(length(pd)>0){
        tibs<-pd$tib #!!! check this
        name<-tail(names(tibs),1) #kludge, last name
        pts<-tibs[[name]][[name]] # !!!KLUDGE for now, ASSUME TIB NAME AND POINTS NAME ARE SAME!!!
        if(!is.null(pts)){
          row<-nrow(pts)
          m<-pts[[row]]
          matCol<-ncol(m)
          point.index<-length(unlist(pts))/2
        } else{
          row<-0
          matCol<-0
          point.index<-0
        }
        
        # selectedPoint$name<-name
        # selectedPoint$point.index<-l/2
        
        updateSelected( name=name, row=row, matCol=matCol, point.index=point.index )
      }
    } 
    
  })
})
