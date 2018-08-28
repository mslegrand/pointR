# - a project config (aka pprj)
# - should contain
# - proj name
# - current tab
# - proj type
# - git options?
#   - date
# - workspace subdir

# 3. close all current files (exit current project)
# - observeEvent of *customFileDialog*
#  for the time being we just close
closeCurrentProj<-function(){
  cat('>---> closeCurrentProj\n')
  saveDnippetsFileNames() 
  savePage(input$pages)
  addToRecentProjects(editOption$currentProjectDirectory, editOption$currentProjectName )
  # iterate throug open tabs and close
  # addToRecentFiles(mssg$docFilePath)
  #save editOptions
  if(!is.null(editOption$currentProjectName)){
    editOption$recentProjects<-c(editOption$recentProjects,editOption$currentProjectName)
    editOption$currentProjectName<-NULL
  }
  if(!is.null(editOption$currentProjectName)){
    #opts$recentProjects<-c(opts$recentProjects,opts$currentProjectName)
    editOption$currentProjectName<-NULL
  }
  opts<-isolate(reactiveValuesToList((editOption)))
  opts<-sapply(opts,unlist, USE.NAMES = T, simplify = F )
  writeOptionsJSON(opts)
  # close all open tabs
  tabIds<-fileDescDB()$tabId
  for( tabId in tabIds){
    removeTab(inputId = "pages", tabId)
  }  
  # reinit dbs
  #dnippetsDB$usage<-initialDnippetsDBUsage()
  resetDnippetsDB()
  preProcDB$points<-initialPreprocDB()
  fileDescDB(initialFileDescDB())
  svgGridDB( initialSvgGridDB() )
  useTribbleFormatDB( initialTribbleDB() )
  backDropDB( initialBackDropDB() )
  svgGridDB( initialSvgGridDB() )
  cat('<---< closeCurrentProj\n')
}

setSfDir<-function(sf_id,path,root="home"){
  cat("sf_id=",sf_id,'path=',format(path),"\n")
  if(is.null(path)){
    jscode<-paste0(
      'Shiny.onInputChange("',sf_id,'-modal", {null} );'
    )
  } else {
    path<-sub(pattern = "^~","",path) ## todo: may need to fix for windos
    # path<-sub(pattern = "^/home","",path) 
    pp<-unlist(strsplit(x=path, split = .Platform$file.sep ))
    pp2<-paste0('"',pp,'"', collapse=",")
    jscode<-paste0(
      'Shiny.onInputChange("',
      sf_id,'-modal", {"path":[',
      pp2,
      '],"root":"home"});'
    )
  }
  cat(jscode,"\n\n")
  jscode
}

resetShinyFilesIOPaths<-function(pathToProj){
  cat( ">---> resetShinyFilesIOPaths\n")
  
  pathToProj<-path_rel(pathToProj, path_home() )
  pathToProj<-paste0("~/",pathToProj)
  cat('pathToProj=', format(pathToProj),"\n")
  fileIOIds<-c("buttonFileOpen", "buttonSnippetImport","buttonDnippetImport",
               "buttonPreProcPtImport","buttonExportSVG","buttonExportPreproc")
  for(id in c(fileIOIds, saveButtonFileNames)){
    jscode<-setSfDir(id, path="")
    #cat('1 jscode=')
    #cat(jscode)
    runjs(jscode)
  }
  Sys.sleep(.3)
  for(id in c(fileIOIds, saveButtonFileNames)){
    jscode<-setSfDir(id, pathToProj)
    #cat('2 jscode=')
    #cat(jscode)
    runjs(jscode)
  }
  cat( "<---< resetShinyFilesIOPaths\n")
}


setUpProj<-function(projName, pathToProj, projType="generic"){
  # 4. set 
  
  
  editOption$currentProjectDirectory=pathToProj
  editOption$currentProjectName=projName
  # reset all fileIO paths
  
  resetShinyFilesIOPaths(pathToProj)
  
  
  # remove from recent projects
  removeFromRecentProjects(projDir=pathToProj, projName=projName)
  # 5. save editOptions (aleady done closeCurrentProj)
}

