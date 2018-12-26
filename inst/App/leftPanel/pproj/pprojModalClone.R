# ---beging code to inserted in ptR-------------------------------
cloneProjModal <- function(failed = 0, mssg=NULL, datapath=NULL, projectName=NULL, clonepath=NULL) {
  
  shinyDirChoose(input, id='browseForDir', roots=c(home='~'))
  observeEvent(input$browseForDir,{
    datapath<-parseDirPath(c(home='~'), input$browseForDir)
    if(length(datapath)==0 || nchar(datapath)==0 ){
      datapath='~'
    } else{
      updateTextInput(session,inputId = "parentProjDirectoryName", value=datapath)
    }
  })
  
  shinyFileChoose(input, "browseForClone",    session=session, roots=c(home="~"),  filetypes=c('pprj') ) #hidden
  fp.dt<-parseFilePaths(c(home='~'), input$browseForClone)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    datapath<-gsub(pattern = '^NA/', "~/", datapath)
    updateTextInput(session,inputId = "templateProjName", value=datapath)
  }
  
  observeEvent(input$browseForClone,{
    fp.dt<-parseFilePaths(c(home='~'), input$browseForClone)
    if(length(fp.dt)>0 && nrow(fp.dt)){
      datapath<-as.character(fp.dt$datapath[1])
      datapath<-gsub(pattern = '^NA/', "~/", datapath)
      updateTextInput(session,inputId = "templateProjName", value=datapath)
    }
  })
  
  modalDialog(
    h2('Create a new pointR project by cloning an existing pointR project'),
    if(failed==1){
      h4(mssg)
    },
    div( 
      textInput(inputId="modalProjName", "New Project Name",
                value = projectName,
                placeholder = 'The name of this pointR project'
      )),
    if(failed==2){
      h4(mssg)
    },
    div( style="display:inline-block",
         textInput(inputId="parentProjDirectoryName", label="Path to New project:",
                   value=datapath, 
                   placeholder = 'The parent directory for this pointR project'
         )),
    div( style="display:inline-block",
         shinyDirButton(id= 'browseForDir', label="browse", title='Browse...', FALSE)
    ),
    div( style="display:inline-block",
         textInput(inputId="templateProjName", label="Existing project to clone:",
                   value=datapath, 
                   placeholder = 'The pointR project to clone'
         )),
    div( style="display:inline-block",
         shinyFilesButton(id= 'browseForClone', label="browse", title='Browse...', FALSE)
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("modalCloneProjOk", "OK")
    )
  )
}

#to do: proj name should be restricted to letters, numbers, '.' and spaces.
#to do: proj dir should be restricted to letters, numbers, '.'  and spaces.

observeEvent(input$modalCloneProjOk, {
  # Check that data object exists and is data frame.
  projectName<-input$modalProjName
  if(!is.null(projectName)){
    projectName<-str_trim(projectName)
    if(nchar(projectName)==0){projectName<-NULL }
  }
  datapath<-input$parentProjDirectoryName
  if(!is.null(datapath)){
    datapath<-str_trim(datapath)
    if(nchar(datapath)==0){datapath<-NULL }
  }
  clonepath<-input$templateProjName
  if(!is.null(clonepath)){
    clonepath<-str_trim(clonepath)
    if(nchar(clonepath)==0){clonepath<-NULL }
  }
    
  if (is.null(projectName) ) {
    mssg='Please specify the project name'
    showModal(newProjModal(failed = 1, mssg=mssg, datapath=datapath, projectName = projectName, clonepath=clonepath))
  } else if (is.null(datapath) ) {
    mssg='Please specify the project path'
    showModal(newProjModal(failed = 1, mssg=mssg, datapath=datapath, projectName = projectName, clonepath=clonepath))
  } else if (is.null(clonepath) ) {
    mssg='Please specify the project to clone'
    showModal(newProjModal(failed = 1, mssg=mssg, datapath=datapath, projectName = projectName,clonepath=clonepath))
  } else  if(file.access(datapath, mode=0)< 0){
    mssg<- paste('This path specified below does not exist. Please specify a different project path')
    showModal(newProjModal(failed = 2, mssg=mssg, datapath=datapath, projectName= projectName,clonepath=clonepath))
  } else if( file.access(datapath, mode=2)<0 ){
    mssg<- paste('This path specified below is not writable. Please specify a different project path')
    showModal(newProjModal(failed = 2, mssg=mssg, datapath=datapath, projectName= projectName,clonepath=clonepath))
  } else if( !file.exists(clonepath) ){
    mssg<- paste('This path specified below is not writable. Please specify a different project path')
    showModal(newProjModal(failed = 2, mssg=mssg, datapath=datapath, projectName= projectName,clonepath=clonepath))
  } else {
    # prepare to process
    
    templatePath<-dirname(input$templateProjName)
    pathToProjParent<-input$parentProjDirectoryName
    
    projName<-input$modalProjName
    pattern<-gsub('\\.pprj$','', basename(input$templateProjName))
    projName<-gsub('\\.pprj$','',projName) 
    projNameExt<-paste0(projName,'.pprj')
    
    
     # 0. close current project
    closeCurrentProj()
    # 1. clone project
    fullpathProjName<-copyAndRenameProject(
      pattern=pattern, 
      templatePath=templatePath, 
      projName=projName, 
      pathToProjParent=pathToProjParent 
    )
    # 2. open cloned project
    ptRproj<-read_json(fullpathProjName) 
    pprj(ptRproj)
    # 3. 
    pathToProj<-path_join(c(pathToProjParent,projName))
    setUpProj(projName=projNameExt, pathToProj=pathToProj, projType='cloned')
    #invoke startup
    request$sender<-'startup'
    
    removeModal() 
  }
})
