
restoreWorkSpace<-function( workSpaceDir=getWorkSpaceDir() ){
  cat('>---> restoreWorkSpace\n')
  wsPages<-list()
  fileWSPaths<-dir(workSpaceDir, pattern='PTR-TABID', full.names = T)

  if(length(fileWSPaths)==0){
    return(FALSE)
  }
  
  
  # 1. load all pages into a list.
  for(filePath in fileWSPaths){
    page<-readRDS(filePath)
    # 3. assign tabIds to each page
    id=getNextTabId()
    # substitute value ending in tabId with id
    tbid<-grep("tabId$", names(page))
    page[tbid]<-id
    wsPages<-c(wsPages, list(id=page))
    # 2 remove filePath file
    file.remove(filePath)
    newFilePath<-paste0(workSpaceDir,"/",id,".rda")
    
    saveRDS(object=page, file = newFilePath)
  }
  cat('#3 restoreWorkSpace\n')
  #4. iterate through pages and add to serverAssetDB$tib
  # --- serverAssetDB
  tib<-serverAssetDB$tib
  pattern<-"^assetSelection."
  for(page in wsPages){
    tibAs<-page[ grep(pattern, names(page)) ]
    names(tibAs)<-gsub(pattern, '', names(tibAs))
    #tibAs<-as.tibble(tibAs)
    tib<-bind_rows(tib, tibAs)
  }
  serverAssetDB$tib<-tib
  
  # --- backDropDB
  tib<-backDropDB()
  pattern<-"^backdrop."
  for(page in wsPages){
    tibAs<-page[ grep(pattern, names(page)) ]
    names(tibAs)<-gsub(pattern, '', names(tibAs))
    #tibAs<-as.tibble(tibAs)
    tib<-bind_rows(tib, tibAs)
  }
  backDropDB(tib)
  
  # ---grid---
  tib<-svgGridDB()
  pattern<-"^grid."
  for(page in wsPages){
    tibAs<-page[ grep(pattern, names(page)) ]
    names(tibAs)<-gsub(pattern, '', names(tibAs))
    #tibAs<-as.tibble(tibAs)
    tib<-bind_rows(tib, tibAs)
  }
  svgGridDB(tib)
  
  #--- tribbleDB
  tib<-useTribbleFormatDB()
  pattern<-"^trib."
  for(page in wsPages){
    tibAs<-page[ grep(pattern, names(page)) ]
    names(tibAs)<-gsub(pattern, '', names(tibAs))
    #tibAs<-as.tibble(tibAs)
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
  for(page in wsPages){
    # extract the serverAsset portion and add
    asi<-grep("^preprocScripts.", names(page))
    if(length(asi)>0){
      tibAs<-page[asi]
      tn<-gsub('^preprocScripts.', '', names(tibAs))
      names(tibAs)<-tn
      tibAs<-as.tibble(tibAs)
      tib<-bind_rows(tib, tibAs)
    }
  }
  preProcDB$points<-tib
  
  # ------- fileDescDB
  tib<-fileDescDB()
  for(page in wsPages){
    # extract the serverAsset portion and add
    asi<-grep("^fileDescriptor.", names(page))
    if(length(asi)>0){
      tibAs<-page[asi]
      tn<-gsub('^fileDescriptor.', '', names(tibAs))
      names(tibAs)<-tn
      tibAs<-as.tibble(tibAs)
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
    #missing the addFileDesc
    
    if(mode=='ptr'){
      divClass="cAceContainer"
    } else {
      divClass="cAceRmdContainer"
    }
    #addFileDesc(pageId=tabId, docFilePath=docFilePath, fileSaveStatus, fileMode=mode)
    
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
    
    restoreAssetState(tabId)
    #updateAceExt(id=aceId, sender='cmd.tabChange', roleBack=FALSE, setfocus=TRUE, getValue=TRUE)
    cat('#15 restoreWorkSpace\n')
    updateTabsetPanel(session, inputId='pages', selected=tabId)
    cat('#16 restoreWorkSpace\n')
  }
  delay(500,{
    for(page in wsPages){
      
      tabId=page$fileDescriptor.tabId
      fileSaveStatus=page$fileDescriptor.isSaved 
      savedStatus<-ifelse(fileSaveStatus, 'saved', 'notSaved')
      
      if(is.null(tabId)){ browser() }
      addNewPage2dnippetsDB(tabId)
      cat('#18 restoreWorkSpace\n')
      sendFileTabsMessage(resize=runif(1), tabId=tabId, savedStatus= savedStatus)
      cat('#19 restoreWorkSpace\n')
    }
  })
  cat('<---< restoreWorkSpace\n')
  return(TRUE)
  
}

