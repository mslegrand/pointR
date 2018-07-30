
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
  cat(" setFileDescPath::pageId=", format(pageId),"\n")
  cat(" setFileDescPath::filePath=", format(filePath),"\n")
  tb<-fileDescDB()
  tb[tb$tabId== pageId,"filePath"]<-filePath
  cat("tb$filePath=",tb$filePath,"\n")
  fileDescDB(tb)
  cat("fileDescDB()$filePath=",fileDescDB()$filePath,"\n")
}

setFileDescSaved<-function(pageId, fileSaveStatus){
  cat(" setFileDescPath::pageId=", format(pageId),"\n")
  cat(" setFileDescPath::fileSaveStatus=", format(fileSaveStatus),"\n")
  cat(" setFileDescPath::class(fileSaveStatus)=", format(class(fileSaveStatus)),"\n")
  fd<-fileDescDB()
  tmp<-filter(fd, tabId==pageId)
  if(nrow(tmp)>0){
    # if(tmp$filePath=='?'){
    #   fileSaveStatus<-FALSE
    #   cat("revised setFileDescPath::fileSaveStatus=", format(fileSaveStatus),"\n")
    # }
    fd[fd$tabId==pageId,"isSaved"]<-fileSaveStatus 
    fileDescDB(fd) 
    print(fd)
  }
  
}

getFileSavedStatus<-reactive({
  pageId<-input$pages
  if(!is.null(pageId)){
    fd<-fileDescDB()
    tmp<-filter(fd, tabId==pageId)
    cat('nrow(tmp)=',format(nrow(tmp)),"\n")
    if(nrow(tmp)==1){
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
  #cat('removeFileDesc removing')
  file.remove(fileName)
}


# presumably to be saved whenever there is a commit
savePage<-function(pageId, path=getWorkSpaceDir()){
  if(!is.null(pageId) && nchar(pageId)>0){
    # cat("path=",path,"\n")
    # cat("pageId=",pageId,"\n")
    fileName=paste0(path,"/",pageId,".rda")
    # cat("fileName=",format(fileName),"\n")
    # assuming pageId==selectedAsset$tabId
    asel<-reactiveValuesToList(selectedAsset)
    fileDescriptor=getFileDescriptor(pageId)
    # cat('savePage::getFileDescriptor(pageId)=')
    # print(fileDescriptor)
    rtv<-c(
      fileDescriptor=getFileDescriptor(pageId),
      code=getCode(),
      assetSelection=asel
    )
    
    ppE<-getPreProcPtEntries(pageId)
    if(length(ppE)!=0 && nrow(ppE)>0){
      rtv<-c(rtv, preprocScripts=ppE)
    }
    # print(ppE)
    # Now write the object
    saveRDS(object=rtv, file = fileName)
    # selectedAsset
    # request$code
    # request$mode
    # docFilePath
    # isSavedo
    # assetSelection
    # preproc code    
  }
}


restoreWorkSpace<-function( workSpaceDir=getWorkSpaceDir() ){
  
  wsPages<-list()
  fileWSPaths<-dir(workSpaceDir, full.names = T)
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
  tib<-serverAssetDB$tib
  for(page in wsPages){
    # extract the serverAsset portion and add
    asi<-grep("^assetSelection.", names(page))
    tibAs<-page[asi]
    tn<-gsub('^assetSelection.', '', names(tibAs))
    names(tibAs)<-tn
    tibAs<-as.tibble(tibAs)
    tib<-bind_rows(tib, tibAs)
  }
  serverAssetDB$tib<-tib
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
 
  # pages$tabIdCount=1
  # for(page in wsPages){
  #   mode=page$fileDescriptor.mode
  #   docFilePath=page$fileDescriptor.filePath
  #   fileSaveStatus=page$fileDescriptor.isSaved
  #   txt=page$code
  #   tabName<-getNextAnonymousFileName()
  #   if(!identical(docFilePath, "?")){
  #     title=baseName(docFilePath)
  #   } else {
  #     title=tabName
  #   }
  #   addFileTab(title=title,
  #              txt=txt,  
  #              docFilePath=docFilePath, 
  #              mode=mode, 
  #              fileSaveStatus=fileSaveStatus
  #   )
  # }
  #pages$tabIdCount=1
  for(page in wsPages){
    # extract the serverAsset portion and add
    tabId=page$fileDescriptor.tabId
   
    aceId<-tabID2aceID(tabId)
    mode=page$fileDescriptor.mode
    docFilePath=page$fileDescriptor.filePath
    fileSaveStatus=page$fileDescriptor.isSaved 
    cat('tabId=',tabId, 'isSaved=',fileSaveStatus,"\n" )
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
    #updateAceExt(id=aceId, sender='cmd.tabChange', getValue= TRUE )
    #browser()
    #Sys.sleep(.5)
    #delay(500,updateAceExt(id=aceId, sender='cmd.tabChange', getValue= TRUE ))
    
   # delay(500,
      
      
 #  )
    # 
    # delay(500,
    #       updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE, setDocFileSaved=TRUE, ok=TRUE )
    # )
  }
  delay(500,{
    for(page in wsPages){
      tabId=page$fileDescriptor.tabId
      fileSaveStatus=page$fileDescriptor.isSaved 
      savedStatus<-ifelse(fileSaveStatus, 'saved', 'notSaved')
      sendFileTabsMessage(resize=runif(1), tabId=tabId, savedStatus= savedStatus)
    }
  })
  
  # now set selection and resize
  # if(length(wsPages)>0){
  #   tabId<-wsPages[[1]]$fileDescriptor.tabId
  #   #updateTabsetPanel(session,  inputId = 'pages', selected = tabId)
  #   delay(500, 
  #         updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE, setDocFileSaved=TRUE, ok=TRUE )
  #   )
  # }
 
}


# 
# restoreWorkSpace<-function( workSpaceDir=getWorkSpaceDir() ){
#   
#   fileWSPaths<-dir(workSpaceDir, full.names = T)
#   #tmpWSDir<-tempdir()
#   #cat("tmpWSDir=", tmpWSDir, "\n")
#   #file.copy(workSpaceDir, tmpWSDir, recursive=TRUE)
#   #tmpWSDir<-paste0(tmpWSDir,'/workspace')
#  # file.remove(dir(workSpaceDir))
#   #filePaths<-dir(tmpWSDir, full.names = TRUE)
#   for(filePath in fileWSPaths){
#     cat('---> loadPage(', filePath,')\n')
#     loadPage(filePath)
#     #file.remove(filePath)
#   }
#   #cat("tmpWSDir=",tmpWSDir,"\n")
#   #unlink(tmpWSDir, recursive=T)
# }

# loadPage<-function( filePath ){
#   page<-readRDS(filePath)
#   file.remove(filePath)
#   src<-page$code
#   docFilePath<-page$fileDescriptor.filePath
#   mode<-page$fileDescriptor.mode
#   title<-page$fileDescriptor.filePath
#   isSaved<-page$fileDescriptor.isSaved
#   if(title=="?"){
#     title<-getNextAnonymousFileName()
#     isSaved<-FALSE
#   } 
#   pageId<-addFileTab(title=title, txt=src, docFilePath= docFilePath, mode=mode, fileSaveStatus=isSaved)
#   # may need to pause here and wait for tabId to get updated 
#   #(or some other way of getting the tabid just added)
#   # next update selectedAsset?
#   
#   # delay(500,
#   #       {
#   
#   # A better approach might be is to store back into the respective databases, then 
#   # and pull from the db.
#   
#   
#           selectionList=list()
#           page$assetSelection.tabId<-pageId
#           for(n in names(serverAssetDB$tib )){
#             nn<-paste0('assetSelection.', n)
#             val<-page[[nn]]
#             if(is.null(val)){
#               val<-NA
#             }
#             selectionList[[n]]<-val
#           }
#           serverAssetDB$tib<-bind_rows(serverAssetDB$tib, selectionList)
#           # update preproc if available
#           if(!is.null(page$preprocScripts.tabId)){
#             page$preprocScripts.tabId<-pageId
#             insertPreProcPtEntry(
#               tab_Id=page$assetSelection.tabId, 
#               tib_Name=page$preprocScripts.tibName, 
#               pt_Column_Name=page$preprocScripts.ptColName, 
#               newScript=preprocScripts.script
#             )
#           }
#           
#           # now save back into temp dir ( to be moved back to soon)
#           cat('---> savePage(', pageId,")\n")
#           savePage(pageId)
#         # }
#   # )
#     
#   
# }