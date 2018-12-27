  # tabId=page$fileDescriptor.tabId
  # 
  # mode=page$fileDescriptor.mode
  # docFilePath=page$fileDescriptor.filePath
  # fileSaveStatus=page$fileDescriptor.isSaved 


# gets what we need from fileDescDB()
newPage<-function(tabId, title, txt, docFilePath, mode,  fileSaveStatus ){
  # cat('>---> newPage\n')
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
  if(mode=='ptr'){
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
          theme    = defaultOpts["theme"],
          fontSize = defaultOpts["fontSize"], 
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
  # cat('<---< newPage\n')
  aceId
} 

# then addFileTab would look like

# addFileTab<-function(title, txt,  docFilePath='?', mode='ptr', fileSaveStatus=FALSE){
#  
#   addFileDesc(pageId=tabId, docFilePath=docFilePath, fileSaveStatus, fileMode=mode)
#   setUseTribble( pageId=tabId, value=TRUE)
#   addNewPage2dnippetsDB(tabId)
#   addPage(tabId, txt)
#   updateTabsetPanel(session,inputId = 'pages', selected = tabId)
#   
# }

# while the loop in serverLoadWorkSpace would become

# for(page in wsPages){
#   tabId<-page$fileDescriptor.tabId
#   addPage(tabId, txt)
#   restoreAssetState(tabId)
#   updateTabsetPanel(session, inputId='pages', selected=tabId)
# }  
# delay(500,{
#   for(page in wsPages){
#     tabId=page$fileDescriptor.tabId
#     cat('page in wsPages:: tabId=',tabId,"\n")
#     fileSaveStatus=page$fileDescriptor.isSaved 
#     savedStatus<-ifelse(fileSaveStatus, 'saved', 'notSaved')
#     
#     if(is.null(tabId)){ browser() }
#     addNewPage2dnippetsDB(tabId)
#     sendFileTabsMessage(resize=runif(1), tabId=tabId, savedStatus= savedStatus)
#   }
# })