
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
  if(!is.null(input$pages)){ # to avoid warning
    fd<-fileDescDB()
    fd[fd$tabId==input$pages,]$mode
  } else {
    NULL
  }
})
  

getFileDescriptor<-function(pageId){ 
  if(!is.null(pageId)){  # not really needed since caller checks for null pageId!!!
      fd<-fileDescDB()
      fd[fd$tabId==pageId,] #or use filter
  } else {
    NULL
  }
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
      fd<-fileDescDB()
      tmp<-filter(fd, tabId==pageId)
      if(nrow(tmp)>0){
        fd[fd$tabId==pageId,"isSaved"]<-fileSaveStatus 
        fileDescDB(fd) 
      }
  }
}


getAllNamedUnsavedFiles<-reactive({
  fd<-fileDescDB()
  fd<-filter(fd, isSaved==FALSE & filePath!="?")
  fd
})

getFileSavedStatus<-reactive({
  pageId<-input$pages
  if(!is.null(pageId)){
    fd<-fileDescDB()
    tmp<-filter(fd, tabId==pageId)
    if(nrow(tmp)==1){
      tmp$isSaved
    } else {
      FALSE
    }
  } else {
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

