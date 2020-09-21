# defaultOpts initialized  in configIO.R
editOption<-do.call(reactiveValues, defaultOpts)

# defauinitialProjltOpts initialized  in configIO.R
pprj<-reactiveVal(initialProj)

#' getDirPath returns the path to the project, or .ptR if no project
#' used by 
#'  1 saveDnippetsFileNames
#'  2 readDnippetsFileNames
#'  3 restoreWorkSpace
#'  4 savePage
#'  
getDirPath<-reactive({
  if(!is.null(editOption$currentProjectName) && !is.null(editOption$currentProjectDirectory)){
    dirPath<-editOption$currentProjectDirectory
    if(!file.exists(dirPath)){
      dirPath<-optionDirPath()
      editOption$currentProjectName=NULL
      editOption$currentProjectDirectory=NULL
    }
  } else {
    dirPath<-optionDirPath()
  }
  dirPath
})

observeEvent(getDirPath(),{
    resetShinyFilesIOPaths(getDirPath())
})

getWorkSpaceDir<-reactive({
  dirPath<-getDirPath()  
  workSpaceDir<-file.path(dirPath,'.workspace')
})

getProjectFullPath<-reactive({
  if(!is.null(editOption$currentProjectName) && !is.null(editOption$currentProjectDirectory)){
    file.path(editOption$currentProjectDirectory, editOption$currentProjectName)
  } else {
    NULL
  }
})
  

# used by cmdFileExportSvg.R; serverEditBar.R; serverOptions.R
getCurrentFile<-reactive({
  basename(editOption$currentFilePath)
})

# not used 
getCurrentDir<-reactive({
  dirname(editOption$currentFilePath)
})

#not used
getCurrentFilePath<-reactive({
  editOption$currentFilePath
})

# TO REVISE!!!
# used by cmdFileExportSvg.R, serverEditBar.R
getFileNameStatus<-reactive({
  !is.null(editOption$currentFilePath) &&
    editOption$currentFilePath!="" &&
    editOption$currentFilePath!="." &&
    editOption$currentFilePath!="./"
})

# invoked by cmdFileOpen and cmdFileExportSvg.R
setCurrentFilePath<-function(filePath){
  editOption$currentFilePath<-filePath
}

addToRecentFiles<-function(filePath){
  filePath<-unlist(filePath)
  if(!is.null(filePath) && filePath!='?' && file.exists(filePath)){
    files<-unlist(editOption$recentFiles)
    if(!is.null(files)){
      files<-files[file.exists(files) ]
    }
    editOption$recentFiles <-unique(c(filePath,files ))
  }
}

removeFromRecentFiles<-function(filePath){
  tmp<-editOption$recentFiles[]
  tmp<-tmp[tmp!=filePath]
  editOption$recentFiles<-tmp
}


addToRecentProjects<-function(projDir, projName){
  recentProj<-file.path(projDir, projName)
  editOption$recentProjects <-unique(c(recentProj,editOption$recentProjects ))
}

removeFromRecentProjects<-function(projDir, projName){
  recentProj<-file.path(projDir, projName)
  tmp<-editOption$recentProjects[]
  tmp<-tmp[tmp!=recentProj]
  editOption$recentProjects<-tmp
}

mkFileSubMenu<-function(subMenuLabel, prefix, fullFilePaths){
  files<-unlist(fullFilePaths)
  if(length(files)>0){
    files<-files[file.exists(files)]
  }
  if(length(files)>0){
    # 4 make submenu
    files<-normalizePath(files)
    files<-unique(files)
    values<-paste(prefix,files, sep="-")
    labels<-basename(files)
    hints<-files
    mkmenuitem<-function(label, value, hint){
      shinyDMDMenu::menuItem(
        label=label, 
        value=value # ,
       # span(hint, class='tooltiptext')
      )
    }
    items<-mapply(mkmenuitem, label=labels, value=values, hint=hints,
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)
    submenu=do.call( function(...){ menuDropdown(subMenuLabel,...) }, items)
  } else{
    submenu=NULL
  }
  submenu  
}

observeEvent( editOption$recentProjects ,{
  # 1 remove submenu
  subMenuLabel= "Recent Projects"
  removeDMDM(session=session, menuBarId="editNavBar", entry=subMenuLabel)
  # 2 get files to populate submenu
  files<-unlist(editOption$recentProjects)
  # 3 create submenu
  submenu<-mkFileSubMenu( subMenuLabel= subMenuLabel, prefix='recentProj', files) 
  # 4 insertsubmenu
  if(!is.null(submenu)){
    insertAfterDMDM(
      session, menuBarId = "editNavBar", 
      entry="openProject", submenu= submenu
    )
  }
})


# must both add and delete entries.
observeEvent( editOption$recentFiles ,{
  # 1 remove menuDropdown
  removeDMDM(session=session, menuBarId="editNavBar", entry="Recent Files")
  # 2 get files to populate submenu
  files<-unlist(editOption$recentFiles)
  # 3 create submenu
  submenu<-mkFileSubMenu( subMenuLabel= "Recent Files", prefix="recentFile", files) 
  # 4 insertsubmenu
  if(!is.null(submenu)){
    insertAfterDMDM(
      session, menuBarId = "editNavBar", 
      entry="openFile", submenu= submenu
    )
  }
})

# observeEvent(editOption$whiteSpace,{
#   newLabel<-ifelse(editOption$whiteSpace, "Hide White Space",  "Show White Space")
#   renameDMDM(
#     session, menuBarId="editNavBar", 
#     entry=newLabel, 
#     newLabel = newLabel, 
#     type = "menuItem")
#   dirtyDMDM(session, "editNavBar")
# })
  
            
