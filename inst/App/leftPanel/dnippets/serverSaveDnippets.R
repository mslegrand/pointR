
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
    cat(length(fullpaths),' many dnippets found in loadedDnippets.rda\n ')
  } else {
    fullpaths<-NULL
  }
  if(length(fullpaths)==0){
    fullpaths<-paste(system.file('App', package='pointR'), 'templates', 'sampleShapes.dnippets', sep='/')
    cat('no dnippets in loadedDnippets.rda\n Adding  sampleShapes\n')
  }
  
  for(fp in fullpaths){
    cat('loading fp=', format(fp),"\n")
    loadDndSnippets(fp)
  }
  #2. need to load each.
  
}
