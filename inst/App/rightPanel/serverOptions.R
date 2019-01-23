# initialized by defaultOpts in configIO.R
editOption<-do.call(reactiveValues, defaultOpts)
#editOption$.saved=TRUE

pprj<-reactiveVal(NULL)

#' getDirPath returns the path to the project, or .ptR if no project
#' used by 
#'  1 saveDnippetsFileNames
#'  2 readDnippetsFileNames
#'  3 restoreWorkSpace
#'  4 savePage
#'  
getDirPath<-reactive({
  # cat('editOption$currentProjectName=',      editOption$currentProjectName, "\n")
  # cat('editOption$currentProjectDirectory=', editOption$currentProjectDirectory, "\n")
  # cat('>---> getDirPath\n')
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
  # cat('<---< getDirPath\n')
  dirPath
})

observeEvent(getDirPath(),{
  # cat('\n>---> observeEvent getDirPath()\n')
    resetShinyFilesIOPaths(getDirPath())
  # cat('<---< observeEvent getDirPath()\n\n')
})

getWorkSpaceDir<-reactive({
  dirPath<-getDirPath()  
  workSpaceDir<-file.path(dirPath,'.workspace')
})

getProjectFullPath<-reactive({
  if(!is.null(editOption$currentProjectName) && !is.null(editOption$currentProjectDirectory)){
    #projName<-paste0(editOption$currentProjectName, ".pprj")
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

# revised to use fileDescriptor: fileDescDB
#getFileSavedStatus<-reactive({editOption$.saved})

# invoked by cmdFileOpen and cmdFileExportSvg.R
setCurrentFilePath<-function(filePath){
  editOption$currentFilePath<-filePath
}

# must both add and delete entries.
# This algorithm is very inefficient
observeEvent( editOption$recentFiles ,{
  # One strategy: remove all recentFiles and then reinsert
  # 1 remove menuDropdown("Recent Files")
  files<-unlist(editOption$recentFiles)
  if(!is.null(files)){
    files<-files[file.exists(files)]
  }
  N<-length(files)
  
  removeDMDM(
    session=session, menuBarId="editNavBar", entry="Recent Files")
  if(N>0){
    #1 make shortNames
    L<-str_split(files,"/")
    N<-length(files)
    toName<-function(x,n){
      t<-tail(L[[x]],n=n)
      paste(rev(t),collapse="~")
    }
    dupded<-function(S){
      sapply(1:length(S), function(i)sum(S==S[i])>1)
    }
    D<-rep(T,length.out=N)
    S<-D
    n<-0
    while(any(D)){
      I<-which(D)
      n<-n+1
      S[I]<-sapply(I, function(x)toName(x,n=n) )
      D<-dupded(S)
    }
    menuLabels<-S
    # 2 make dropdown containing menuItems for each recentFile 
    label="Recent Files"
    menuValues<-paste("recentFile",files, sep="-")
   
    items<-lapply(1:N, function(i){ 
      shinyDMDMenu::menuItem( label=menuLabels[i], value=menuValues[i])
    } )
    
    submenu=do.call(function(...){ menuDropdown(label,...) }, items)
    #3 add dropdown to menu
    insertAfterDMDM(
      session, menuBarId = "editNavBar", 
      entry="openFile", submenu= submenu)
  }
})


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


observeEvent( editOption$recentProjects ,{
  # cat('>---> observeEvent::editOption$recentProjects\n')
  rp<-unlist(editOption$recentProjects)
  rplabel="Recent Projects"
  # cat('recent Projects:\n')
  # print(rp)
  removeDMDM( session=session, menuBarId="editNavBar", entry=rplabel)
  # cat('recent Projects:  1\n')
  # remove any non-existing entries
  rp<-rp[file.exists(rp)]
  #add back
  #cat('recent Projects:  2\n')
  if(length(rp)>0){ 
    values<-paste('recentProj',rp,sep="-")
    labels<-rp
    
    items<-mapply(shinyDMDMenu::menuItem, label=labels, value=values, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    
    submenu=do.call( function(...){ menuDropdown(rplabel,...) }, items)
    # cat('recent Projects:  6\n')
    # cat('class(submenu)=',class(submenu), "\n")
    # cat('length(submenu)=',length(submenu), "\n")
    # for(i in 1:length(submenu)){
    #   print(submenu[[i]])
    # }
    insertAfterDMDM(session, menuBarId = "editNavBar", entry="openProject", submenu= submenu)
    #cat('recent Projects:  7\n')
  }
  # cat('<---< observeEvent::editOption$recentProjects\n')
})



# removeFromDnippetsFiles<-function(filePath){
#   tmp<-editOption$dnippetsFiles
#   tmp<-tmp[tmp!=filePath]
#   editOption$dnippetsFiles<-tmp
# }

# addToDnippetsFiles<-function(filePath){
#   importDnippet2DB(filePath)
#   # tmp<-editOption$dnippetsFiles
#   # tmp<-unique(c(tmp, filePath))
#   # editOption$dnippetsFiles<-tmp
# }


              
