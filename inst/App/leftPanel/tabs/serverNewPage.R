  # tabId=page$fileDescriptor.tabId
  # 
  # mode=page$fileDescriptor.mode
  # docFilePath=page$fileDescriptor.filePath
  # fileSaveStatus=page$fileDescriptor.isSaved 


# gets what we need from fileDescDB()
newPage<-function(tabId, title, txt, docFilePath, mode,  fileSaveStatus ){
  log.fin(newPage)
  if(is.null(tabId)){
    cat('big probs\n')
    stop()
  }
  # fd<-fileDescDB()
  # fd<-fd[fd$tabId==tabId,]
  # mode=fd.mode
  # docFilePath=fd.filePath
  aceId<-tabID2aceID(tabId)
  # fileSaveStatus<-fd.fileDescriptor.isSaved 
  
  # if(!identical(docFilePath, "?")){
  #   title=basename(docFilePath)
  # } else {
  #   title=paste('Anonymous', page$fileDescriptor.anonNo)
  # }
  # cat('newPage mode= ',format(mode),'\n')
  # cat('title=',title,"\n")
  if(mode=='javascript'){
    divClass="cAceContainer"
  } else if(mode=='ptr' && !title %in% c('app.R','App.R')){
    divClass="cAceContainer"
  } else {
    divClass="cAceRmdContainer"
  }
  appendTab(
    inputId = "pages", select=TRUE,
    tabPanel(
      title=tabTitleRfn(title, tabId, docFilePath), # maybe we should save title in fileDescriptor?
      div(
        class=divClass,
        overflow= "hidden",inline=FALSE,
        shinyAce4Ptr(
          outputId = aceId,
          value    = txt,
          mode     = mode,
          theme    = editOption$theme, 
          fontSize = as.numeric(editOption$fontSize), 
          autoComplete="enabled",
          if(mode=='ptr'){
            autoCompleteList =list(names(svgR:::eleDefs))
          } else { # handle js mode, ... here
            NULL
          },
          docFilePath =docFilePath,
          initSaved   =fileSaveStatus
        )
      ),
      value=tabId
    )
  )
  log.fout(newPage)
  aceId
} 

