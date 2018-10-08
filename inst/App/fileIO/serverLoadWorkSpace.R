
restoreWorkSpace<-function( workSpaceDir=getWorkSpaceDir(), pprjPath=getProjectFullPath() ){
  # cat('>---> restoreWorkSpace\n')
  
  fileWSPaths<-dir(workSpaceDir, pattern='PTR-TABID', full.names = T)
  
  if(length(fileWSPaths)==0){
    # cat("workSpaceDir = ",format(workSpaceDir), "\n")
    return(FALSE)
  }
  wsPages<-list()
  
  # 1. load all pages into a list.
  for(filePath in fileWSPaths){
    page<-readRDS(filePath)
    # A. assign tabIds to each page
    # id=getNextTabId()
    id=basename(filePath)
    # # substitute value ending in tabId with id
    # tbid<-grep("tabId$", names(page))
    # page[tbid]<-id
    wsPages[[id]]<-page
  } 
  # load project.pprj
 
  # if(length(pprjPath)>0){
  #   pprj<-read_json(pprjPath)
  #   if(!identical(pprj$pathToProj, getWorkSpaceDir())){
  #     for(page in wsPages){
  #       fp<-page$fileDescriptor.filePath
  #       page$fileDescriptor.filePath<-gsub(getWorkSpaceDir(), pprj$pathToProj, fp)
  #     }
  #   }
  # }
  # # 2 remove filePath file
  # for(filePath in fileWSPaths){
  #   file.remove(filePath)
  # }
  # 
  # # 3 write pages
  # for(id in names(wsPages)){
  #   newFilePath<-paste0(workSpaceDir,"/",id,".rda")
  #   page<-wsPages[[id]]
  #   saveRDS(object=page, file = newFilePath)
  # }
  
  #4. iterate through pages and add to serverAssetDB$tib
  # --- serverAssetDB
  tib<-serverAssetDB$tib
  pattern<-"^assetSelection."
  for(page in wsPages){
    tibAs<-page[ grep(pattern, names(page)) ]
    names(tibAs)<-gsub(pattern, '', names(tibAs))
    tib<-bind_rows(tib, tibAs)
  }
  serverAssetDB$tib<-tib
  
  # --- backDropDB
  tib<-backDropDB()
  pattern<-"^backdrop."
  for(page in wsPages){
    tibAs<-page[ grep(pattern, names(page)) ]
    names(tibAs)<-gsub(pattern, '', names(tibAs))
    tib<-bind_rows(tib, tibAs)
  }
  backDropDB(tib)
  
  # ---grid---
  tib<-svgGridDB()
  pattern<-"^grid."
  for(page in wsPages){
    tibAs<-page[ grep(pattern, names(page)) ]
    names(tibAs)<-gsub(pattern, '', names(tibAs))
    tib<-bind_rows(tib, tibAs)
  }
  svgGridDB(tib)
  
  #--- tribbleDB
  tib<-useTribbleFormatDB()
  pattern<-"^trib."
  for(page in wsPages){
    tibAs<-page[ grep(pattern, names(page)) ]
    names(tibAs)<-gsub(pattern, '', names(tibAs))
    tib<-bind_rows(tib, tibAs)
  }
  useTribbleFormatDB(tib)
  
  #--- dnippetsDB
  tib<-dnippetsDB$usage
  pattern<-"^dnip."
  for(page in wsPages){
    tibAs<-page[ grep(pattern, names(page)) ]
    names(tibAs)<-gsub(pattern, '', names(tibAs))
    tib<-bind_rows(tib, tibAs)
  }
  dnippetsDB$usage<-tib
  
  # --- preProcDB
  tib<-preProcDB$points
  pattern<-"^preprocScripts."
  for(page in wsPages){
    # extract the serverAsset portion and add
    asi<-grep(pattern, names(page))
    if(length(asi)>0){
      tibAs<-page[asi]
      tn<-gsub(pattern, '', names(tibAs))
      names(tibAs)<-tn
      # tibAs<-as.tibble(tibAs)
      tib<-bind_rows(tib, tibAs)
    }
  }
  preProcDB$points<-tib
  
  # ------- fileDescDB
  tib<-fileDescDB()
  pattern<-"^fileDescriptor."
  for(page in wsPages){
    # extract the serverAsset portion and add
    asi<-grep(pattern, names(page))
    if(length(asi)>0){
      tibAs<-page[asi]
      tn<-gsub(pattern, '', names(tibAs))
      names(tibAs)<-tn
      tib<-bind_rows(tib, tibAs)
    }
  }
  fileDescDB(tib)
  
  for(page in wsPages){
    # extract the serverAsset portion and add
    tabId=page$fileDescriptor.tabId
    
    aceId<-tabID2aceID(tabId)
    mode=page$fileDescriptor.mode
    docFilePath=page$fileDescriptor.filePath
    fileSaveStatus=page$fileDescriptor.isSaved 
    txt=page$code
    
    if(!identical(docFilePath, "?")){
      title=basename(docFilePath)
    } else {
      title=paste('Anonymous', page$fileDescriptor.anonNo)
    }
    
    if(mode=='ptr'){
      divClass="cAceContainer"
    } else {
      divClass="cAceRmdContainer"
    }
    
    appendTab(
      inputId = "pages",
      tabPanel( #tabId,
        title<-tabTitleRfn(title, tabId, docFilePath), # maybe we should save title in fileDescriptor?
        div(
          class=divClass,
          overflow= "hidden",inline=FALSE,
          shinyAce4Ptr(
            outputId = aceId,
            value=txt,
            mode=mode,
            theme=defaultOpts["theme"],
            fontSize=defaultOpts["fontSize"], autoComplete="enabled",
            if(mode=='ptR')
              autoCompleteList =list(names(svgR:::eleDefs))
            else
              NULL
            ,
            docFilePath=docFilePath,
            initSaved=fileSaveStatus
          )
        ),
        value=tabId
      )
    )
    if(!(tabId %in% serverAssetDB$tib$tabId) ){
      stop('tabId=', tabId, 'not found in serverAssetDB$tib$tabId')
    }
    restoreAssetState(tabId)
    updateTabsetPanel(session, inputId='pages', selected=tabId)
  }
  delay(500,{
    for(page in wsPages){
      
      tabId=page$fileDescriptor.tabId
      fileSaveStatus=page$fileDescriptor.isSaved 
      savedStatus<-ifelse(fileSaveStatus, 'saved', 'notSaved')
      
      if(is.null(tabId)){ browser() }
      addNewPage2dnippetsDB(tabId)
      sendFileTabsMessage(resize=runif(1), tabId=tabId, savedStatus= savedStatus)
    }
  })
  # cat('<---< restoreWorkSpace\n')
  return(TRUE)
  
}

