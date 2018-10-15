# ---beging code to inserted in ptR-------------------------------
newProjModal <- function(failed = 0, mssg=NULL, datapath=NULL, projectName=NULL) {
  #shinyDirChoose(input, id='browseForDir', roots=c(wd='~'), filetypes='')
  shinyDirChoose(input, id='browseForDir', roots=c(wd='~'))
  observeEvent(input$browseForDir,{
    datapath<-parseDirPath(c(wd='~'), input$browseForDir)
    if(length(datapath)==0 || nchar(datapath)==0 ){
      datapath='~'
    } else{
      updateTextInput(session,inputId = "parentProjDirectoryName", value=datapath)
    }
  })
  modalDialog(
    h2('Create a new pointR project'),
    if(failed==1){
      h4(mssg)
    },
    div( 
      textInput(inputId="modalProjName", "Project Name",
                value = projectName,
                placeholder = 'The name of this pointR project'
      )),
    #span('Create the project as a subdirectory of:)'),
    if(failed==2){
      h4(mssg)
    },
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
      actionButton("modalNewProjOk", "OK")
    )
  )
}

#to do: proj name should be restricted to letters, numbers, '.' and spaces.
#to do: proj dir should be restricted to letters, numbers, '.'  and spaces.

observeEvent(input$modalNewProjOk, {
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
  if (is.null(projectName) ) {
    mssg='Please specify the project name'
    showModal(newProjModal(failed = 1, mssg=mssg, datapath=datapath, projectName = projectName))
  } else if (is.null(datapath) ) {
    mssg='Please specify the project path'
    showModal(newProjModal(failed = 1, mssg=mssg, datapath=datapath, projectName = projectName))
  } else  if(file.access(datapath, mode=0)< 0){
    mssg<- paste('This path specified below does not exist. Please specify a different project path')
    showModal(newProjModal(failed = 2, mssg=mssg, datapath=datapath, projectName= projectName))
  } else if( file.access(datapath, mode=2)<0 ){
    mssg<- paste('This path specified below is not writable. Please specify a different project path')
    showModal(newProjModal(failed = 2, mssg=mssg, datapath=datapath, projectName= projectName))
  } else {
    # try to add file and workspace, if not writable , return fail
    newProj(projectName, datapath, projType="generic")
    
    # 1 cd to datapath
    # 2 create new file with name = input$modalProjName
    #   2.5 may need to put in request, since it will need to add windows.
    # 3 register in recent projects.
    
    removeModal() 
  }
})
