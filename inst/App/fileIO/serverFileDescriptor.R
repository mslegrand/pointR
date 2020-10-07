
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
  # cat('>---> getMode\n')
  tabId<-input$pages # getTibTabId()
  if(is.null(tabId) || identical(tabId, 'bogus')){
    mode<-NULL
  } else {
    fd<-fileDescDB()
    stopifnot('tabId' %in% names(fd))
    mode<-fd[fd$tabId==input$pages,]$mode
    
  }
  mode
})

# in getModeX we need to insert flag to enable/disable appmode
# we could use shinyjs to query a property of a (hidden) node?
# or use ptR.js to do an on.change ?
# Or update a reactive on change of a given node or property
getModeX<-reactive({
  log.fin(getModeX)
  pageId<-input$pages 
  if(is.null(pageId) || identical(pageId, 'bogus')){
    mode<-NULL
  } else {
    fd<-fileDescDB()
    stopifnot('tabId' %in% names(fd))
    mode<-fd[fd$tabId==pageId,]$mode
    if(identical(mode,'ptr')){
      docFilePath<-fd[fd$tabId== pageId,]$filePath
      if(!is.null(docFilePath) && !is.na(docFilePath)){
        docFilePath<-tolower(basename(docFilePath))
        if(identical(docFilePath,'app.r')){
          mode<-'app'
        }
      }
    }
  }
  #log.val(mode)
  log.fout(getModeX)
  mode
})  

getFileDescriptor<-function(pageId){ 
  # cat('>---> getFileDescriptor\n')
  if(!is.null(pageId)){  # not really needed since caller checks for null pageId!!!
      fd<-fileDescDB()
      # print(fd)
      rtv<-fd[fd$tabId==pageId,] #or use filter
      if(nrow(rtv)==0){
        rtv<-NULL
      }
      # print(rtv)
  } else {
    rtv<-NULL
  }
  # cat('<---< getFileDescriptor\n')
  rtv
}

# to be called from 
# where is the filePath reset?
setFileDescPath<-function(pageId, filePath, pathToProj){
  # filePath<-path_rel(filePath,pathToProj)
  # browser()
  tb<-fileDescDB()
  tb[tb$tabId== pageId,"filePath"]<-filePath
  #tb[tb$tabId== pageId,"mode"]<-pathExt2mode(pathToProj)
  fileDescDB(tb)
}

setFileDescMode<-function(pageId, newMode){
  tb<-fileDescDB()
  tb[tb$tabId== pageId,"mode"]<-newMode
  fileDescDB(tb)
}

setFileDescSaved<-function(pageId, fileSaveStatus){
  if(!is.null(pageId)){
    log.fin(setFileDescSaved)
      fileSaveStatus<-unlist(fileSaveStatus)
      fd<-fileDescDB()
      stopifnot('tabId' %in% names(fd))
      tmp<-filter(fd, tabId==pageId)
      if(nrow(tmp)>0){
        fd[fd$tabId==pageId,"isSaved"]<-fileSaveStatus 
        fileDescDB(fd) 
      }
      # cat('setFileDescSaved: pageId=',pageId,',  savedStatus=',fileSaveStatus,"\n")
      
      sendFileTabsMessage(tabId=pageId, sender='savedStatus', saveStatus=fileSaveStatus)
      log.fout(setFileDescSaved)
  }
}


getAllNamedUnsavedFiles<-reactive({
  # cat('>---> getAllNamedUnsavedFiles\n')
  fd<-fileDescDB()
  fd<-filter(fd, isSaved==FALSE & filePath!="?")
  # cat('<---< getAllNamedUnsavedFiles\n')
  fd
})

# get the saved status for the current page
getFileSavedStatus<-reactive({
  pageId<-input$pages
  if(!is.null(pageId)){
    fd<-fileDescDB()
    stopifnot('tabId' %in% names(fd))
    tmp<-filter(fd, tabId==pageId)
    if(nrow(tmp)==1){
      rtv<-tmp$isSaved
    } else {
      rtv<-FALSE
    }
  } else {
    rtv<-FALSE
  }
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
  # cat('>---> removeFileDesc\n')
  fdDB<-fileDescDB()
  stopifnot('tabId' %in% names(fdDB))
  # fdDB<-filter(fdDB, !identical(tabId, pageId))
  fdDB<-filter(fdDB, tabId!=pageId)
  fileDescDB(fdDB)
  fileName=paste0(path,"/",pageId,".rda")
  file.remove(fileName)
  # cat('<---< removeFileDesc\n')
}

