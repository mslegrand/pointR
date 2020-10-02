

observeEvent( input$editNavBar, { 
  
  fileCmd<-getLeftMenuCmd()
  
  if(length(fileCmd)>0){
    if( fileCmd =="addTemplate"){
       # cat('******* addTemplate\n')
      # get projectPath
      source<-getProjectFullPath()
      source=dirname(source)
      # getProject Name
      name<-basename(source) # should be same as editOption$currentProjectName)
      name<-sub("\\.pprj$",'',name)
      target<-file.path(homeDir, '.ptR','.templates',name)
      # cat('source=',source,"\n")
      # cat('target=',target,"\n")
      copyDirectory(from=source, to=target,  private=TRUE, recursive=TRUE)
      # copy to .ptR
      # update menus
      updateNewProjectMenu(session)
      updateRemoveTemplateMenu(session)
    }
    if(grepl("removeTemplate-",fileCmd)){
      target<- str_split(fileCmd,'-')[[1]][2]
     # delete templatePath
      # unlink(target)
      dir_delete(target)
      # update menus
      updateNewProjectMenu(session)
      updateRemoveTemplateMenu(session)
    }
    if( fileCmd %in% c("newPtrTibScript", "newPtRMatScript", "newPtRSVGScript", "newRScript" )){ #-----new
      cmdFileNewPtR(fileCmd)
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="newRmd"){ #-----open 
      cmdFileNewRmd()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="newJavascript"){ #-----open 
      cmdFileNewJavascript()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="newSnippets"){ #-----open 
      cmdFileNewSnippet()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="newText"){ #-----open 
      cmdFileNewTxt()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=='newIOSlides'){
      cmdFileNewIOSlides()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="openFile"){ #-----open 
      cmdFileOpen()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="newBasicProject"){
      showModal(newProjModal())
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="newCloneProject"){
      showModal(cloneProjModal())
      dirtyDMDM(session, "editNavBar")
    }
    if(grepl("projectTemplate-",fileCmd)){
      templateName<- str_split(fileCmd,'-')[[1]][2]
      showModal(newProjShinyCntrlModal(projTemplateName=templateName))
      dirtyDMDM(session, "editNavBar")
    }
    # if(grepl("projectSample-",fileCmd)){
    #   projectName<- str_split(fileCmd,'-')[[1]][2]
    #   showModal(sampleProjModal(projectName=projectName))
    #   dirtyDMDM(session, "editNavBar")
    # }
    if(fileCmd=="openProject"){ #-----open
      dirtyDMDM(session, "editNavBar") 
      cmdFileOpenProject()
      # dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="saveAs"){ #-----save
      dirtyDMDM(session, "editNavBar")
      cmdFileSaveAs()
    } 
    if(fileCmd=="Save"){ #-----save
      cmdFileSave()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="saveAll"){
      cmdFileSaveAll()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="close"){
      cmdFileClose()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="closeAll"){
      #cat(' fileCmd=="closeAll" \n')
      cmdFileCloseAll()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="closeProject"){
      #cat(' fileCmd=="closeAll" \n')
      closeCurrentProj()
      editOption$currentProjectDirectory<-NULL
      editOption$currentProjectName<-NULL
      dirtyDMDM(session, "editNavBar")
      #delay(500, {request$sender='startup'})
      #delay(500, {setRequestSender('startup')})
      delay(500, {requestStartUp()})
    }
    if(fileCmd=="quit"){
      # cat(' fileCmd=="quit" \n')
      cmdFileQuit()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="Export as SVG"){ #-----save
      cmdFileExportSvg()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="Theme"){
      cmdOptionsTheme()
      dirtyDMDM(session, "editNavBar")
    } 
    if(fileCmd=="Font Size"){
      cmdFileFontSize()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="adjustTabs"){
      indentSizes<-1:12
      size<-as.character(editOption$tabSize)
      modalIndentSize <- function() {
        modalDialog(
          selectInput("selectIndentSize", "Select Indent Spaces", indentSizes, multiple=FALSE, 
                      selectize = FALSE, width="90px", selected=size  ), 
          footer = tagList(actionButton("modalIndentSizeCancel", "Cancel"),actionButton("modalIndentSizeOk", "OK") )
        ) 
      }
      showModal( modalIndentSize() )
      dirtyDMDM(session, "editNavBar")
    }
    #whiteSpaceCmds<-c("Show White Space", "Hide White Space")
    if(fileCmd == "toggleWhiteSpace"){
      editOption$whiteSpace<-!(editOption$whiteSpace)
      val=ifelse(editOption$whiteSpace, 'show', 'hide' )
      updateAceExt(id=getAceEditorId(), sender='updateAll', whiteSpace=val )
      dirtyDMDM(session, "editNavBar")
    }
    
    tabType<-c("Use Soft Tabs", "Use Hard Tabs")
    
    if(fileCmd %in% tabType){
      indx<-which(fileCmd==tabType)
      editOption$tabType<-fileCmd
      newLabel<-ifelse(indx==2,"Use Soft Tabs", "Use Hard Tabs" )
      updateAceExt(id= getAceEditorId(), sender='fileCmd.toggleTab', toggleTabType=TRUE )
      renameDMDM(
        session, menuBarId="editNavBar", 
        entry=fileCmd, 
        newLabel = newLabel, 
        type = "menuItem")
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=='print'){
      updateAceExt( id= getAceEditorId(), sender='fileCmd.print', tbMssg='print' )
      dirtyDMDM(session, "editNavBar")
    }
    
    if(fileCmd=="Editor ShortCuts"){
      updateAceExt(id= getAceEditorId(), sender='fileCmd.showShortCuts', showKeyboardShortCuts=TRUE)
      dirtyDMDM(session, "editNavBar")
    }

    if(fileCmd=="Editor ShortCuts2"){
      updateAceExt(id= getAceEditorId(), sender='fileCmd.showShortCuts2', getKeyboardShortcuts=TRUE )
      dirtyDMDM(session, "editNavBar")
    }
    
    if(fileCmd=="Element Reference"){
      query<-"Element-Index"
      cmdSVGHelp(query)
      dirtyDMDM(session, "editNavBar")
    }
 
    if(fileCmd=="newDndSnippetsFile"){
      cmdDndSnippetNew()
      dirtyDMDM(session, "editNavBar")
    }
    
    if(fileCmd=="importDndSnippetsFile"){
      cmdDnippetImport()
      dirtyDMDM(session, "editNavBar")
    }
    
    if(fileCmd=="importSnippetFile"){
      # cat('serverEditBar---------------fileCmd=="importSnippetFile"---------------------------------------\n ')
      cmdSnippetImport()
      dirtyDMDM(session, "editNavBar")
    }
    
    if(fileCmd=="unloadSnippets"){
      cmdSnippetUnload()
      dirtyDMDM(session, "editNavBar")
    }
    
    if(fileCmd=="aboutCmd"){
      cmdAbout()
      dirtyDMDM(session, "editNavBar")
    }
    
    if(usingElectron){
      if(identical(fileCmd,"svgRUserGuide")){
        #href='http://mslegrand.github.io/svgR/User_Guide.html'
        #sendPtRManagerMessage(sender='cmd.electron', openLink= href)
        sendPtRManagerMessage(sender='cmd.electron', openWindow= "svgRUserGuide")
        dirtyDMDM(session, "editNavBar")
      }
      if(identical(fileCmd,"io.svgR")){
        href='http://mslegrand.github.io/svgR'
        sendPtRManagerMessage(sender='cmd.electron',  openLink= href)
        dirtyDMDM(session, "editNavBar")
      }
      if(identical(fileCmd,"W3C SVG reference")){
        href='https://www.w3.org/TR/SVG/intro.html'
        sendPtRManagerMessage(sender='cmd.electron',  openLink= href)
        dirtyDMDM(session, "editNavBar")
      }
    }
    
    if(grepl("recentFile-",fileCmd)){
      #get the filename
      fileName<-sub("recentFile-","",fileCmd)
      #if file fails to exist remove
      dirtyDMDM(session, "editNavBar")
      if(!file.exists(fileName)){
        # remove from recentFiles
        #send alert message 
        showNotification("File Not Found.")
        rf<-editOption$recentFiles
        rf<-rf[-which(rf==fileName)]
        editOption$recentFiles<-rf
      } else {
        openFileNow(fileName)
      }
      
    }
    
    if(grepl("recentProj-",fileCmd)){
      # cat('>---> recentProjects\n')
      #get the name
      name<-sub("recentProj-","",fileCmd)
      #cat('>---> recentProjects: name=', format(name),"\n")
      #if file fails to exist remove
      dirtyDMDM(session, "editNavBar")
      if(!file.exists(name)){
        # remove from projects
        #send alert message 
        showNotification("Project Not Found.")
        rf<-editOption$recentProjects
        rf<-rf[-which(rf==name)]
        editOption$recentProjects<-rf
      } else {
        projName<-basename(name)
        pathToProj<-dirname(name)
        # cat('recentFiles:: pathToProj', format(pathToProj),"\n")
        # pathToProj<-path_rel(pathToProj, path_home() )
        openProj(projName, pathToProj )
      }
      dirtyDMDM(session, "editNavBar")
      # cat('<---< recentProjects:\n')
    }
  }
}, label='editNavBar') 

# keep file menu save uptodate
observeEvent(getFileSavedStatus(),{
  fd<-getFileDescriptor(input$pages)
  status<-getFileSavedStatus()
  if(!is.null(input$pages) && getFileSavedStatus()==FALSE && 
     !identical(fd$filePath,"?")){
    # set dmdm file save active
    enableDMDM(
      session, 
      menuBarId="editNavBar", 
      entry="Save"
    )
    #status<-FALSE #'notSaved'
  } else {
    # set dmdm file save inactive
    disableDMDM(
      session, 
      menuBarId="editNavBar", 
      entry="Save"
    )
    #status<-TRUE #'saved'
  }
  if(!is.null(input$pages)){
      sendFileTabsMessage( tabId=input$pages, sender='savedStatus', saveStatus=status)
  }
})



#------- editor options handlers

observeEvent(input$modalIndentSizeCancel, {
  removeModal()
}) 

observeEvent(input$modalIndentSizeOk, {
  editOption$tabSize<-input$selectIndentSize
  removeModal()
})


#------fileName-------------
output$fileName <- renderText({ 
  if(getFileNameStatus()==TRUE){
    fileName<-getCurrentFile()
  } else {
    fileName<-"Unnamed File"
  }
  paste("Editing", fileName)
})

#----project---------------
observeEvent( editOption$currentProjectName, {
  if(length(editOption$currentProjectName)==0){
    title='project: <none>'
    disableDMDM(session, "editNavBar", 'closeProject')
  } else {
    title=paste0('project: ', editOption$currentProjectName)
    enableDMDM(session, "editNavBar", 'closeProject')
  }
  renameDMDM(session, menuBarId="editNavBar", 
                       entry='project', newLabel=title, newValue='project')
  
}, ignoreNULL = FALSE)
# -----------ACE EDITOR------------------------


observeEvent(editOption$fontSize, {
  updateAceExt(id=getAceEditorId(), sender='updateAll', fontSize=as.numeric(editOption$fontSize) )
  #updateAceEditor(session, getAceEditorId(), fontSize=as.numeric(editOption$fontSize) )
})

observeEvent(editOption$theme, {
  updateAceExt(id=getAceEditorId(), sender='updateAll', theme=editOption$theme )
  #updateAceEditor(session, getAceEditorId(), theme=editOption$theme)
})

observeEvent(editOption$tabSize, {
  # todo: change to update all tabSizes?
  updateAceExt(id=getAceEditorId(), sender='updateAll', 
               tabSize=as.numeric(editOption$tabSize)) 
  # updateAceEditor(session, getAceEditorId(),  tabSize=as.numeric(editOption$tabSize))
}, ignoreNULL = TRUE, ignoreInit = TRUE)



#----------------------------------------------
