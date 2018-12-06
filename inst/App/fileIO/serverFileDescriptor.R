
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
  cat('>---> getMode\n')
  tabId<-input$pages # getTibTabId()
  if(is.null(tabId) || identical(tabId, 'bogus')){
    mode<-NULL
  } else {
    fd<-fileDescDB()
    stopifnot('tabId' %in% names(fd))
    mode<-fd[fd$tabId==input$pages,]$mode
  }
  # cat('mode is ', format(mode),"\n")
  cat('<---< getMode\n')
  mode
})
  

getFileDescriptor<-function(pageId){ 
  cat('>---> getFileDescriptor\n')
  if(!is.null(pageId)){  # not really needed since caller checks for null pageId!!!
      fd<-fileDescDB()
      # print(fd)
      rtv<-fd[fd$tabId==pageId,] #or use filter
      # print(rtv)
  } else {
    rtv<-NULL
  }
  cat('<---< getFileDescriptor\n')
  rtv
}

# to be called from 
# where is the filePath reset?
setFileDescPath<-function(pageId, filePath, pathToProj){
  # filePath<-path_rel(filePath,pathToProj)
  # browser()
  tb<-fileDescDB()
  tb[tb$tabId== pageId,"filePath"]<-filePath
  fileDescDB(tb)
}

setFileDescSaved<-function(pageId, fileSaveStatus){
  if(!is.null(pageId)){
      cat('>---> setFileDescSaved\n')
      fileSaveStatus<-unlist(fileSaveStatus)
      fd<-fileDescDB()
      stopifnot('tabId' %in% names(fd))
      tmp<-filter(fd, tabId==pageId)
      if(nrow(tmp)>0){
        fd[fd$tabId==pageId,"isSaved"]<-fileSaveStatus 
        fileDescDB(fd) 
      }
      cat('<---< setFileDescSaved\n')
  }
}


getAllNamedUnsavedFiles<-reactive({
  cat('>---> getAllNamedUnsavedFiles\n')
  fd<-fileDescDB()
  fd<-filter(fd, isSaved==FALSE & filePath!="?")
  cat('<---< getAllNamedUnsavedFiles\n')
  fd
})

getFileSavedStatus<-reactive({
  cat('>---> getFileSavedStatus\n')
  pageId<-input$pages
  if(!is.null(pageId)){
    fd<-fileDescDB()
    stopifnot('tabId' %in% names(fd))
    # print(fd)
    tmp<-filter(fd, tabId==pageId)
    # print(tmp)
    if(nrow(tmp)==1){
      rtv<-tmp$isSaved
    } else {
      rtv<-FALSE
    }
  } else {
    rtv<-TRUE
  }
  cat('<---< getFileSavedStatus\n')
  rtv
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
  cat('>---> removeFileDesc\n')
  fdDB<-fileDescDB()
  stopifnot('tabId' %in% names(fdDB))
  # fdDB<-filter(fdDB, !identical(tabId, pageId))
  fdDB<-filter(fdDB, tabId!=pageId)
  fileDescDB(fdDB)
  fileName=paste0(path,"/",pageId,".rda")
  file.remove(fileName)
  cat('<---< removeFileDesc\n')
}

