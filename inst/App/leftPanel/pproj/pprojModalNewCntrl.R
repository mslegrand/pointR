# ---beging code to inserted in ptR-------------------------------
newProjShinyCntrlModal <- function(failed = 0, mssg=NULL, datapath=NULL, projectName=NULL) {
  #shinyDirChoose(input, id='browseForDir', roots=c(wd='~'), filetypes='')
  cat('>----> newProjShinyCntrlModal\n')
  shinyDirChoose(input, id='browseForDir', roots=c(home='~'))
  observeEvent(input$browseForDir,{
    datapath<-parseDirPath(c(home='~'), input$browseForDir)
    if(length(datapath)==0 || nchar(datapath)==0 ){
      datapath='~'
    } else{
      updateTextInput(session,inputId = "parentProjDirectoryName", value=datapath)
    }
  })
  cat('>----> modalDialog\n')
  modalDialog(
    h2('Create a new Shiny Custom Input Project'),
    div(
      selectInput(inputId='prjTmplName', label='Custom template', choices=
                    c('SimpleInputWidget', 'EfficientInputWidget'))
    ),
    if(failed==1){
      h4(mssg)
    },
    div( 
      textInput(inputId="modalProjName", "Project Name",
                value = projectName,
                placeholder = 'The name of the new Shiny Custom Input'
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
      actionButton("modalNewShinyCntrlProjOk", "OK")
    )
  )
}

#to do: proj name should be restricted to letters, numbers, '.' and spaces.
#to do: proj dir should be restricted to letters, numbers, '.'  and spaces.

observeEvent(input$modalNewShinyCntrlProjOk, {
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
    #newProj(projectName, datapath, projType="generic")
    
    # 1 cd to datapath
    # 2 create new file with name = input$modalProjName
    #   2.5 may need to put in request, since it will need to add windows.
    # 3 register in recent projects.
    
    
    
    # prepare to process
    templateName<-input$prjTmplName
    templatePath<- projTemplatesPaths[templateName] # the clone path of this project.
    templateName.pprj<-'widget.pprj' #namesassigned to the .pprj
    pathToProjParent<-datapath # input$parentProjDirectoryName # parent directory of new project
    projName<-projectName # input$modalProjName # the name of of new project
    
    pattern<-gsub('\\.pprj$','', basename(templateName.pprj)) # = widget
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
    ptRproj<-read_json(fullpathProjName, simplifyVector = TRUE) 
    pprj(ptRproj)
    # 3. 
    pathToProj<-path_join(c(pathToProjParent,projName))
    setUpProj(projName=projNameExt, pathToProj=pathToProj, projType='shinyInputCntrl')
    #invoke startup
    request$sender<-'startup'
    
    removeModal() 
  }
    
    
})
