
restoreWorkSpace<-function( workSpaceDir=getWorkSpaceDir() ){
  cat('>---> restoreWorkSpace\n')
  wsPages<-list()
  fileWSPaths<-dir(workSpaceDir, pattern='PTR-TABID', full.names = T)
  cat('#1 restoreWorkSpace\n')
  if(length(fileWSPaths)==0){
    return(FALSE)
  }
  cat('#2 restoreWorkSpace\n')
  # browser()
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
    # newFilePath<-paste0(id,".rda")
    # cat(newFilePath,"\n")
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
  cat('#4 restoreWorkSpace\n')
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
  cat('#5 restoreWorkSpace\n')
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
  # cat("svgGRid::\n")
  # print(svgGridDB())
  cat('#6 restoreWorkSpace\n')
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
  
  cat('#7 restoreWorkSpace\n')
  #--- dnippetsDB
  tib<-dnippetsDB$usage
  pattern<-"^dnip."
  for(page in wsPages){
    tibAs<-page[ grep(pattern, names(page)) ]
    #browser()
    # for(n in names(tibAs)){
    #   cat("tibAs name=", format(n), "\n")
    #   cat("tibAs value=", format(tibAs[n]), "\n")
    # }
    names(tibAs)<-gsub(pattern, '', names(tibAs))
    #tibAs<-as.tibble(tibAs)
    tib<-bind_rows(tib, tibAs)
  }
  dnippetsDB$usage<-tib
  
  cat('#8 restoreWorkSpace\n')
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
  
  cat('#9 restoreWorkSpace\n')
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
  
  cat('#10 restoreWorkSpace\n')
  
  for(page in wsPages){
    # extract the serverAsset portion and add
    tabId=page$fileDescriptor.tabId
    
    aceId<-tabID2aceID(tabId)
    cat('page:: tabId=',format(tabId), 'aceId=',format(aceId),"\n" )
    mode=page$fileDescriptor.mode
    cat('page:: tabId=',format(tabId), 'mode=',format(mode),"\n" )
    docFilePath=page$fileDescriptor.filePath
    cat('page:: tabId=',format(tabId), 'docFilePath=',format(docFilePath),"\n" )
    fileSaveStatus=page$fileDescriptor.isSaved 
    cat('page:: tabId=',format(tabId), 'isSaved=',fileSaveStatus,"\n" )
    txt=page$code
    cat('#11 restoreWorkSpace\n')
    if(!identical(docFilePath, "?")){
      title=basename(docFilePath)
    } else {
      title=paste('Anonymous', page$fileDescriptor.anonNo)
    }
    #missing the addFileDesc
    cat('#12 restoreWorkSpace\n')
    if(mode=='ptr'){
      divClass="cAceContainer"
    } else {
      divClass="cAceRmdContainer"
    }
    #addFileDesc(pageId=tabId, docFilePath=docFilePath, fileSaveStatus, fileMode=mode)
    cat('#13 restoreWorkSpace\n')
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
    cat('#14 restoreWorkSpace\n')
    restoreAssetState(tabId)
    #updateAceExt(id=aceId, sender='cmd.tabChange', roleBack=FALSE, setfocus=TRUE, getValue=TRUE)
    cat('#15 restoreWorkSpace\n')
    
    updateTabsetPanel(session, inputId='pages', selected=tabId)
    
  }
  delay(500,{
    for(page in wsPages){
      cat('#16 restoreWorkSpace\n')
      tabId=page$fileDescriptor.tabId
      fileSaveStatus=page$fileDescriptor.isSaved 
      savedStatus<-ifelse(fileSaveStatus, 'saved', 'notSaved')
      cat('#17 restoreWorkSpace\n')
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

