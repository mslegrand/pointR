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
  storeAssetState()
  savePage(input$pages)
  pprj(NULL)
  if(!is.null(editOption$currentProjectName)){
    addToRecentProjects(editOption$currentProjectDirectory, editOption$currentProjectName )
  }
  
  opts<-isolate(reactiveValuesToList((editOption)))
  opts<-sapply(opts,unlist, USE.NAMES = T, simplify = F )
  writeOptionsJSON(opts)
  
  # close all open tabs
  # stopifnot('tabId' %in% names(fileDescDB()) )
  tabIds<-fileDescDB()$tabId
  for( tabId in tabIds){
    removeTab(inputId = "pages", tabId)
  }  
  # reinit dbs

  resetDnippetsDB()
  preProcScriptDB$points=initialPreProcScriptDB()
  preProcScriptDB$attrs= initialPreProcScriptDB()
  fileDescDB(initialFileDescDB())
  svgGridDB( initialSvgGridDB() )
  useTribbleFormatDB( initialTribbleDB() )
  backDropDB( initialBackDropDB() )
  svgGridDB( initialSvgGridDB() )
  serverAssetDB$tib<-initialServerAsset()

}

# used only by resetShinyFilesIOPaths
setSfDir<-function(sf_id, path, root="home"){
  if(is.null(path)){
    jscode<-paste0(
      'Shiny.onInputChange("',sf_id,'-modal", {null} );'
    )
  } else {
    path<-sub(pattern = "^~","",path) ## todo: may need to fix for windos
    pp<-unlist(strsplit(x=path, split = .Platform$file.sep ))
    pp2<-paste0('"',pp,'"', collapse=",")
    jscode<-paste0(
      'Shiny.onInputChange("',
      sf_id,'-modal", {"path":[',
      pp2,
      '],"root":"home"});'
    )
  }
  jscode
}

# called whenever dirPath changes
# sets shinyFiles to use pathToProj
resetShinyFilesIOPaths<-function(pathToProj, resources='aux'){
  log.fin(resetShinyFilesIOPaths)
  log.val(pathToProj)
  log.val(optionDirPath())
  
  if( identical(pathToProj, optionDirPath())){ # optionDirPath is the .ptR directory
    log.val("case1")
    isProj=FALSE
    pathToProj<-path_home()
  } else {
    isProj=TRUE
  }
  pathToProj<-path_rel(pathToProj, path_home() )
  pathToProj<-paste0("~/",pathToProj) # !!!TODO:  do we really want to do this???
  fileIOIds<-c("buttonFileOpen",         "buttonFileSaveR",
               "buttonSnippetImport",    "buttonDnippetImport",
               "buttonPreProcPtImport",  "buttonPreprocPtExport",
               "buttonPreprocAtExport",  "buttonPreProcAtImport",
               "buttonChoiceSetImport", "buttonSvgExport")
  # first set to root
  for(id in c(fileIOIds, saveButtonFileNames)){
    jscode<-setSfDir(id, path="")
    runjs(jscode)
  }
  Sys.sleep(.3)
  # next set to pathToProj
  if(isProj==TRUE){
    for(id in c(fileIOIds, saveButtonFileNames)){
      if(id %in% c("buttonPreProcPtImport", "buttonPreprocPtExport")){
        jscode<-setSfDir(id, path= path_join( c(pathToProj, resourceDir, 'preprocPts') ))
      } else if(id %in% c("buttonPreProcAtImport", "buttonPreprocAtExport")){
        jscode<-setSfDir(id, path= path_join( c(pathToProj, resourceDir, 'preprocAts') ))
      } else if(id %in% c("buttonDnippetImport")){
        jscode<-setSfDir(id, path= path_join( c(pathToProj, resourceDir, 'dnds' )))
      } else if(id %in% c("buttonSnippetImport")){
        jscode<-setSfDir(id, path= path_join( c(pathToProj,resourceDir, 'snip' )))
      } else if(id %in% c("buttonChoiceSetImport")){
        jscode<-setSfDir(id, path= path_join( c(pathToProj,resourceDir, 'choices' )))
      } else {
        jscode<-setSfDir(id, path=pathToProj)
      }
      # cat(jscode,'\n')
      runjs(jscode)
    }
  } else {
    for(id in c(fileIOIds, saveButtonFileNames)){
      jscode<-setSfDir(id, path=pathToProj)
      runjs(jscode)
    }
  }

  
  log.fout(resetShinyFilesIOPaths)
}


setUpProj<-function(projName, pathToProj, projType="generic"){ #!!! currently projType not used 
  editOption$currentProjectDirectory=pathToProj
  editOption$currentProjectName=projName
  # remove from recent projects
  removeFromRecentProjects(projDir=pathToProj, projName=projName)
  # 5. save editOptions (aleady done closeCurrentProj)
}

