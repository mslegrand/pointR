
saveDnippetsFileNames<-function(path=getWorkSpaceDir()){
  fileName=paste0(path,"/loadedDnippets.rda")
  rtv<-dnippetsDB$paths
  saveRDS(object=rtv, file = fileName)
}


# TODO!!! rewrite *readDnippetsFileNames* to make projects dnippets files relocatable
readDnippetsFileNames<-function(path=getWorkSpaceDir()){
  fileName=paste0(path,"/loadedDnippets.rda")
 
  
  if(file.exists(fileName)){ 
    
    tb<-readRDS(file = fileName)
    fullpaths<-tb$fullpath
    
  } else {
    fullpaths<-NULL
  }
  if(length(fullpaths)==0){
    fullpaths<-paste(system.file('App', package='pointR'), 'templates', 'sampleShapes.dnippets', sep='/')
  } else {
    # browser()
    prjPath=getProjectFullPath()
    ptRproj<-pprj() # doesn't work because pprj is initially NULL, 
    # but I need the current values in the proj dir.
    if(!is.null(ptRproj$pathToProj)){ #ptRproj is set when open
      fullpaths<-sub( ptRproj$pathToProj, editOption$currentProjectDirectory, fullpaths)
    }
    tb$fullpath<-fullpaths
    saveRDS(tb, fileName)
  }
  
  # resave if changes
  for(fp in unique(fullpaths)){
    loadDndSnippets(fp, startup=TRUE)
  }
  
  
}
