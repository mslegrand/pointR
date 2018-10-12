
saveDnippetsFileNames<-function(path=getWorkSpaceDir()){
  fileName=paste0(path,"/loadedDnippets.rda")
  rtv<-dnippetsDB$paths
  saveRDS(object=rtv, file = fileName)
}

readDnippetsFileNames<-function(path=getWorkSpaceDir()){
  fileName=paste0(path,"/loadedDnippets.rda")
  if(file.exists(fileName)){
    tb<-readRDS(file = fileName)
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
  
  
}
