
saveDnippetsFileNames<-function(path=getWorkSpaceDir()){
  fileName=paste0(path,"/loadedDnippets.rda")
  rtv<-dnippetsDB$paths
  saveRDS(object=rtv, file = fileName)
}

readDnippetsFileNames<-function(path=getWorkSpaceDir()){
  fileName=paste0(path,"/loadedDnippets.rda")
  if(file.exists(fileName)){
    tb<-readRDS(file = fileName)
    #1. need the names to be unique
    fullpaths=unique(tb$fullpath)
  } else {
    fullpaths<-NULL
  }
  if(length(fullpaths)==0){
    fullpaths<-paste(system.file('App', package='pointR'), 'templates', 'sampleShapes.dnippets', sep='/')
  }
  
  for(fp in fullpaths){
    loadDndSnippets(fp, startup=TRUE)
  }
  #2. need to load each.
  
}
