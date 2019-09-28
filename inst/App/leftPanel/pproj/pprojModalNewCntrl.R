# ---beging code to inserted in ptR-------------------------------
newProjShinyCntrlModal <- function(failed = 0, mssg=NULL, datapath=NULL, projectName=NULL, projTemplateName) {
  #shinyDirChoose(input, id='browseForDir', roots=c(wd='~'), filetypes='')
  # cat('>----> newProjShinyCntrlModal\n')
  requestedProjTemplateName(projTemplateName)
  shinyDirChoose(input, id='browseForDir', roots=c(home='~'))
  observeEvent(input$browseForDir,{
    datapath<-parseDirPath(c(home='~'), input$browseForDir)
    if(length(datapath)==0 || nchar(datapath)==0 ){
      datapath='~'
    } else{
      updateTextInput(session,inputId = "parentProjDirectoryName", value=datapath)
    }
  })
  # cat('>----> modalDialog\n')
  modalDialog(
    h4('Create a new Project using the template '),
    h2( projTemplateName),
    if(failed==1){
      h4(mssg)
    },
    div( 
      textInput(inputId="modalProjName", "Project Name",
                value = projectName,
                placeholder = 'The name of the new Shiny Custom Input'
      )),
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

requestedProjTemplateName<-reactiveVal("")
#to do: proj name should be restricted to letters, numbers, '.' and spaces.
#to do: proj dir should be restricted to letters, numbers, '.'  and spaces.

observeEvent(input$modalNewShinyCntrlProjOk, {
  # Check that data object exists and is data frame.
  templateName<-requestedProjTemplateName()
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
    mssg='Please specify the project name'; 
    failed = 1
  } else if (is.null(datapath) ) {
    mssg='Please specify the project path'; 
    failed = 1
  } else  if(file.access(datapath, mode=0)< 0){
    mssg<- paste('This path specified below does not exist. Please specify a different project path')
    failed = 2
  } else if( file.access(datapath, mode=2)<0 ){
    mssg<- paste('This path specified below is not writable. Please specify a different project path')
    failed = 2
  } else if( file.exists(file.path(datapath,projectName))){
    mssg=paste(file.path(datapath,projectName),'exists. Please specify different name or path');
    failed=1
  } else{
    failed =0
  }
  if(failed!=0){
    showModal(newProjShinyCntrlModal(failed = failed, mssg=mssg, datapath=datapath, 
                           projectName = projectName, projTemplateName=templateName))
  } else {
    
    # try to add file and workspace, if not writable , return fail
    #newProj(projectName, datapath, projType="generic")
    
    # 1 cd to datapath
    # 2 create new file with name = input$modalProjName
    #   2.5 may need to put in request, since it will need to add windows.
    # 3 register in recent projects.
    
    # prepare to process
    templateName<-requestedProjTemplateName() 
    templatePath<- projTemplatesPaths[templateName] # the clone path of this project.
    templateName.pprj<-dir(templatePath,pattern=".pprj$")

    pathToProjParent<-datapath # input$parentProjDirectoryName # parent directory of new project
    
    projName<-gsub('\\.pprj$','',projectName)  # the name of of new project
    projNameExt<-paste0(projName,'.pprj')
    
    # 0. close current project
    closeCurrentProj()
    # 1. clone project
    fullpathProjName<-copyAndRenameProject(
      sourceProject= path_join(c(templatePath, templateName.pprj)),
      targetName=projName,
      pathToTargetParent=pathToProjParent
    )
    # 2. open cloned project
    ptRproj<-read_json(fullpathProjName, simplifyVector = TRUE) 
    pprj(ptRproj)
    # 3. 
    pathToProj<-path_join(c(pathToProjParent,projName))
    setUpProj(projName=projNameExt, pathToProj=pathToProj, projType='other')
    #invoke startup
    requestStartUp()
    removeModal() 
  }
    
    
})
