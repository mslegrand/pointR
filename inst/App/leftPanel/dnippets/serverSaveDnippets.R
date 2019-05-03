
saveDnippetsFileNames<-function(path=getWorkSpaceDir()){
  log.fin(saveDnippetsFileNames)
  fileName=path_join(c(path,"loadedDnippets.rda"))
  rtv<-dnippetsDB$paths
  saveRDS(object=rtv, file = fileName)
  log.fout(saveDnippetsFileNames)
}



# the usage of a dnippets db is dubious, since we are now
# keeping all dnippets in the resources/dnds dir. 
# A simpler more elegant solutionmight be to just load all in dnps the resources/dnds dir.
# But for non-projects it is not clear what the behaviour should be.

readDnippetsFileNames<-function(path=getWorkSpaceDir()){
  # read in the contents of the workspace dnippets db file
  log.fin(readDnippetsFileNames)
  fileName=path_join(c(path,'loadedDnippets.rda'))
  if(file.exists(fileName)){ 
    tb<-readRDS(file = fileName) #tb is the previously saved dnippetsDB:  has 2 fields
    # usage; a tibble with names = tabId
    # paths; a tibble: with names=c("fullpath", "dname" )
  } else {
    tb<-tibble(fullpath="",dname="")[0,]
  }
  
  sampleDnippetPath<-path_join(c(system.file('App', package='pointR'), 'templates', 'sampleShapes.dnds'))
  
  prjPath=getProjectFullPath() # obtains path from serverOptions:: editOptions
  ptRproj<-pprj() # The internal settings
   # pprj is populated with the full project path during the calls to 
    # - openProj
    # - modalNewShinyCntrlProjOk
    # - input$modalCloneProjOk
    # - during initial startup.
   
  if(!is.null(ptRproj$pathToProj)){  # we have a project so we adjust tb$fullpaths
      # first insure we have a dnds dir
      dndsDir<-path_join(c(editOption$currentProjectDirectory,'resources', 'dnds'))
      if(!dir.exists(dndsDir)){
        dir.create(dndsDir, showWarnings = F, recursive=T) # Todo!!! eventually can remove this.
      }
      # next adjust tibble tb as needed
      if(nrow(tb)>0){
        fullpaths<-tb$fullpath
        # reset paths to reside under resources
        fullpaths<-sub( ptRproj$pathToProj, editOption$currentProjectDirectory, fullpaths)
        # extract full paths not under -editOption$currentProjectDirectory/resouces
        indx<-grep(dndsDir, fullpaths) #indx are the indices of resources now properly located
        # move the files (if any)
        if(length(indx)>0){
          tomove<-fullpaths[-indx]
          if(length(tomove)>0){
            file.copy(tomove, dndsDir)
            # replace tomove by target in fullPaths
            dnipNames<-path_file(tomove )
            fullpaths[-indx]<- sapply( path_file(dnipNames), function(x) path_join(c(dndsDir,x)) )
          }
        }
        tb$fullpath<-fullpaths
      }
      
      if(nrow(tb)>0){
        tb<-filter(tb, file.exists(fullpath))   # prune any entries that do not exist
        tb<-filter(tb, fullpath!=sampleDnippetPath) # prune and dnippet that points to sampleDnippet inside project/app dir
      } 
      if(nrow(tb)==0){
        tomove<-sampleDnippetPath
        file.copy(tomove, dndsDir)
        tb<-tibble( fullpath=path_join(c(dndsDir,'sampleShapes.dnds' )) , dname='sampleShapes' )
      }
  } else { # we do not have a project, but if tb is null, lets load anyway
    if(nrow(tb)==0){
      tb<-tibble( fullpath=sampleDnippetPath , dname='sampleShapes' )
    }
 } 
  # if not inside a project, perform no modifications of tb (at least for now)
  
  # save changes 
  saveRDS(tb, fileName)
  # load DndSnippets
  for(fp in unique(tb$fullpath)){
    loadDndSnippets(fp, startup=TRUE)
  }
  log.fout(readDnippetsFileNames)
}
