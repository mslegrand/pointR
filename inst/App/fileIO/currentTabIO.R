
saveCurrentTab<-function(tabId){
  if(!is.null(tabId)){
    path<-getWorkSpaceDir()
    fileName<-path_join(c(path,"currentTab.rda"))
    saveRDS(object=tabId, file = fileName)
  }
}

readCurrentTab<-function(){
  path<-getWorkSpaceDir()
  fileName<-path_join(c(path,"currentTab.rda"))
  rtv<-NULL
  tryCatch({
    if(file.exists(fileName)){
      rtv<-readRDS(file = fileName)
    }
  }, 
    error=function(e){}
  )
  rtv
}