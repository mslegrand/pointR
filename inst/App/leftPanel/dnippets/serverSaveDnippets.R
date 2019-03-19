
saveDnippetsFileNames<-function(path=getWorkSpaceDir()){
  fileName=paste0(path,"/loadedDnippets.rda")
  rtv<-dnippetsDB$paths
  saveRDS(object=rtv, file = fileName)
}


# TODO!!! rewrite *readDnippetsFileNames* to make projects dnippets files relocatable
readDnippetsFileNames<-function(path=getWorkSpaceDir()){
  # read in the contents of the workspace dnippets db file
  fileName=paste0(path,"/loadedDnippets.rda")
  if(file.exists(fileName)){ 
    tb<-readRDS(file = fileName) #tb is the previously saved dnippetsDB:  has 2 fields
    # usage; a tibble with names = tabId
    # paths; a tibble: with names=c("fullpath", "dname" )
    fullpaths<-tb$fullpath
  } else {
    fullpaths<-NULL
  }
  if(length(fullpaths)>0){
   # browser()
    prjPath=getProjectFullPath() # obtains path from serverOptions:: editOptions
    # pprj is populated with the full project path during the calls to 
    # - openProj
    # - modalNewShinyCntrlProjOk
    # - input$modalCloneProjOk
    # -  during initial startup.
    #
    ptRproj<-pprj() # 
    if(!is.null(ptRproj$pathToProj)){ #ptRproj is set when open
      fullpaths<-sub( ptRproj$pathToProj, editOption$currentProjectDirectory, fullpaths)
    }
    tb$fullpath<-fullpaths
    # prune any entries that do not exist
    if(nrow(tb)>0){
      tb<-filter(tb, file.exists(fullpath))
    }
    fullpaths<-tb$fullpath
  }

  # if there is none availabe, use templates
  if(length(fullpaths)==0){
    # problem: if package is moved (or code on another machine) this path may become invalid.
    fullpaths<-paste(system.file('App', package='pointR'), 'templates', 'sampleShapes.dnippets', sep='/')
    tb<-tibble(fullpath=fullpaths, dname= 'sampleShapes.dnippets')
  } 
  saveRDS(tb, fileName)
  # resave if changes
  for(fp in unique(fullpaths)){
    loadDndSnippets(fp, startup=TRUE)
  }
}
