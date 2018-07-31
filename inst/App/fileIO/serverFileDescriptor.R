
fileDescDB<-reactiveVal(
  tibble(
    tabId='bogus',
    isSaved=FALSE,
    filePath="?",
    anonNo =1,
    mode='ptr'
  )[0,]
)

getNextAnonymousFileName<-reactive({
  paste('Anonymous',getNextAnonFileNum())
})

getNextAnonFileNum<-reactive({
  tb<-fileDescDB()
  num<-max(c(0,filter(tb, filePath=="?")$anonNo))+1
  num
})

# to be called from serverFileTab.R::addFileTab
addFileDesc<-function( pageId, docFilePath, fileSaveStatus, fileMode){
  if(identical(docFilePath,"?")){
    anonNo<-getNextAnonFileNum()
  } else {
    anonNo<-0
  }
  tb<-tibble(tabId=pageId, isSaved=fileSaveStatus,  
             filePath=docFilePath, anonNo, mode=fileMode)
  fd<- fileDescDB()
  fd<-bind_rows(fd,tb)
  fileDescDB(fd)
}


getMode<-reactive({
  fd<-fileDescDB()
  fd[fd$tabId==input$pages,]$mode
})
  

getFileDescriptor<-function(pageId){
  fd<-fileDescDB()
  fd[fd$tabId==pageId,] #or use filter
}

# to be called from 
# where is the filePath reset?
setFileDescPath<-function(pageId, filePath){
  tb<-fileDescDB()
  tb[tb$tabId== pageId,"filePath"]<-filePath
  fileDescDB(tb)
}

setFileDescSaved<-function(pageId, fileSaveStatus){
  if(!is.null(pageId)){
      fileSaveStatus<-unlist(fileSaveStatus)
      cat('setFileDescSaved:: pageId=', pageId, ' fileSaveStatus=', fileSaveStatus, "\n")
      fd<-fileDescDB()
      tmp<-filter(fd, tabId==pageId)
      if(nrow(tmp)>0){
        fd[fd$tabId==pageId,"isSaved"]<-fileSaveStatus 
        fileDescDB(fd) 
        print(fd)
      }
  }

}

getFileSavedStatus<-reactive({
  pageId<-input$pages
  if(!is.null(pageId)){
    fd<-fileDescDB()
    tmp<-filter(fd, tabId==pageId)
    cat('nrow(tmp)=',format(nrow(tmp)),"\n")
    if(nrow(tmp)==1){
      cat('pageId=', format(pageId),"\n")
      cat('tmp$isSaved=',tmp$isSaved,"\n")
      tmp$isSaved
    } else {
      cat('returning F\n')
      FALSE
    }
  } else {
    cat('pageId is NULL\n')
    TRUE
  }
})

# to be called when tab is closed
# a tab can be closed in one of two ways:
# 1 server calls closeTabNow
# 2 user click on the tabs x icon.

# seems that closing a tab the second way is picked up by 
# input$close and forwarded to setTabRequest and request in serverRequest
# and then forwarded to ace. Ace picks this up and issues a closeTabNow(tabId)
# so removeFileDesc should be called from closeTabNow(tabId)
removeFileDesc<-function(pageId, path=getWorkSpaceDir() ){
  fdDB<-fileDescDB()
  fdDB<-filter(fdDB, !identical(tabId, pageId))
  fileDescDB(fdDB)
  fileName=paste0(path,"/",pageId,".rda")
  file.remove(fileName)
}


# presumably to be saved whenever there is a commit
savePage<-function(pageId, path=getWorkSpaceDir()){
  if(!is.null(pageId) && nchar(pageId)>0){
    fileName=paste0(path,"/",pageId,".rda")
    asel<-reactiveValuesToList(selectedAsset)
    fileDescriptor=getFileDescriptor(pageId)
    backdrop=getPageBackDrop(pageId)
    rtv<-c(
      fileDescriptor=getFileDescriptor(pageId),
      code=getCode(),
      assetSelection=asel,
      backdrop=backdrop
    )
    
    ppE<-getPreProcPtEntries(pageId)
    if(length(ppE)!=0 && nrow(ppE)>0){
      rtv<-c(rtv, preprocScripts=ppE)
    }
    saveRDS(object=rtv, file = fileName)

  }
}


restoreWorkSpace<-function( workSpaceDir=getWorkSpaceDir() ){
  
  wsPages<-list()
  fileWSPaths<-dir(workSpaceDir, pattern='PTR-TABID', full.names = T)
  if(length(fileWSPaths)==0){
    return(FALSE)
  }
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
  # browser()
  backDropDB(tib)
  
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
    cat('page:: tabId=',tabId, 'isSaved=',fileSaveStatus,"\n" )
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
    updateTabsetPanel(session, inputId='pages', selected=tabId)
  }
  delay(500,{
    for(page in wsPages){
      tabId=page$fileDescriptor.tabId
      fileSaveStatus=page$fileDescriptor.isSaved 
      savedStatus<-ifelse(fileSaveStatus, 'saved', 'notSaved')
      sendFileTabsMessage(resize=runif(1), tabId=tabId, savedStatus= savedStatus)
    }
  })
  
  return(TRUE)
 
}

