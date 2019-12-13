# ---beging code to inserted in ptR-------------------------------
sampleProjModal <- function(failed = 0, mssg=NULL, datapath=NULL, projectName=NULL) {
 
  shinyDirChoose(input, id='browseForDir', roots=c(home='~'))
  observeEvent(input$browseForDir,{
    datapath<-parseDirPath(c(home='~'), input$browseForDir)
    if(length(datapath)==0 || nchar(datapath)==0 ){
      datapath='~'
    } else{
      updateTextInput(session,inputId = "parentProjDirectoryName", value=datapath)
    }
  })
  modalDialog(
    h2('Import a new sample project'),
    if(failed==1){
      h4(mssg)
    },
    div(
      h2(paste("Project Name:", projectName))
    ), 
    #span('Create the project as a subdirectory of:)'),
    if(failed==2){
      h4(mssg)
    },  
    div( style="visibility:hidden",
       textInput(inputId="modalProjName", "Project Name",
                value = projectName
      )
    ) ,
    div( style="display:inline-block",
         textInput(inputId="parentProjDirectoryName", label="Path to project:",
                   value=datapath, 
                   placeholder = 'The parent directory for this pointR project'
    )),
    div( style="display:inline-block",
         shinyDirButton(id= 'browseForDir', label="browse", title='Browse...', FALSE)
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("modalSampleProjOk", "OK")
    )
  )
}



observeEvent(input$modalSampleProjOk, {
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
  pathToProjParent<-datapath
  pathToProj<-file.path(datapath,projectName)
  
  if (is.null(projectName) ) {
    mssg='Please specify the project name'
    showModal(sampleProjModal(failed = 1, mssg=mssg, datapath=datapath, projectName = projectName))
  } else if (is.null(datapath) ) {
    mssg='Please specify the project path'
    showModal(sampleProjModal(failed = 1, mssg=mssg, datapath=datapath, projectName = projectName))
  } else  if(file.access(datapath, mode=0)< 0){
    mssg<- paste('This path specified below does not exist. Please specify a different project path')
    showModal(sampleProjModal(failed = 2, mssg=mssg, datapath=datapath, projectName= projectName))
  } else if( file.access(datapath, mode=2)<0 ){
    mssg<- paste('This path specified below is not writable. Please specify a different project path')
    showModal(sampleProjModal(failed = 2, mssg=mssg, datapath=datapath, projectName= projectName))
  } else if( file.access(pathToProj, mode=0)==0 ){
    mssg<- paste('Cannot create',pathToProj,'. That dir already exists.  Please specify a different project path')
    showModal(sampleProjModal(failed = 2, mssg=mssg, datapath=datapath, projectName= projectName))
  } else {
    # try to add file and workspace, if not writable , return fail
    
    projSourcePath<- projSamplesPaths[projectName]
    projectName.pprj<-dir(projSourcePath,pattern=".pprj$")
    
    # 0. close current project
    closeCurrentProj()
    
    # 1. copy project
    dir_copy( projSourcePath,  pathToProjParent)
    
    # 2. open copied project
    fullpathProjName<-file.path(pathToProjParent, projectName, projectName.pprj)
    ptRproj<-read_json(fullpathProjName, simplifyVector = TRUE) 
    pprj(ptRproj)
    
    # 3. setup
    setUpProj(projName=projectName.pprj, pathToProj=pathToProj, projType='other')
    
    #invoke startup
    requestStartUp()
    removeModal()   
    
  }
})
