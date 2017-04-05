

observeEvent( input$editNavBar, { 
  #fileCmd<-input$editNavBar$item
  fileCmd<-getLeftMenuCmd()
  if(is.null(fileCmd)){
    fileCmd<-"New"
  }
  if(length(fileCmd)>0){
    if( fileCmd=="New"){ #-----new
      cmdFileNew()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="Open"){ #-----open 
      cmdFileOpen()
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="saveAs"){ #-----save
      cmdFileSaveAs()
      dirtyDMDM(session, "editNavBar")
    } 
    if(fileCmd=="Save"){ #-----save
      curFile<-getCurrentFile()
      if(nchar(curFile)>0){
        cmdFileSave()
      } else {
        cmdFileSaveAs()
      }
      dirtyDMDM(session, "editNavBar")
    }
    if(fileCmd=="quit"){
      cmdFileQuit()
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
    whiteSpace<-c("Show White Space", "Hide White Space")
    
    if(fileCmd %in% whiteSpace){
      indx<-which(fileCmd==whiteSpace)
      newLabel<-ifelse(indx==2,"Show White Space", "Hide White Space" )
      session$sendCustomMessage(
        type = "shinyAceExt",    
        list(id= "source", toggleWhiteSpace=TRUE)
      )
      renameDMDM(
        session, menuBarId="editNavBar", 
        entry=fileCmd, 
        newLabel = newLabel, 
        type = "menuItem")
      dirtyDMDM(session, "editNavBar")
    }
    
    tabType<-c("Use Soft Tabs", "Use Hard Tabs")
    
    if(fileCmd %in% tabType){
      indx<-which(fileCmd==tabType)
      editOption$tabType<-fileCmd
      newLabel<-ifelse(indx==2,"Use Soft Tabs", "Use Hard Tabs" )
      session$sendCustomMessage(
        type = "shinyAceExt",    
        list(id= "source", toggleTabType=TRUE)
      )
      renameDMDM(
        session, menuBarId="editNavBar", 
        entry=fileCmd, 
        newLabel = newLabel, 
        type = "menuItem")
      dirtyDMDM(session, "editNavBar")
    }
    
    if(fileCmd=="Editor ShortCuts"){
      session$sendCustomMessage(
        type = "shinyAceExt",    
        list(id= "source", showKeyboardShortCuts=TRUE)
      )
      dirtyDMDM(session, "editNavBar")
    }

    if(fileCmd=="Editor ShortCuts2"){
      session$sendCustomMessage(
        type = "shinyAceExt",    
        list(id= "source", getKeyboardShortcuts=TRUE)
      )
      dirtyDMDM(session, "editNavBar")
    }
    
    if(fileCmd=="Element Reference"){
      query<-"Element-Index"
      cmdSVGHelp(query)
      dirtyDMDM(session, "editNavBar")
    }
 
    if(fileCmd=="importSnippetFile"){
      cmdSnippetFileOpen()
    }
    
    if(fileCmd=="aboutCmd"){
      cmdAbout()
      dirtyDMDM(session, "editNavBar")
    }
    
    if(grepl("recent-",fileCmd)){

      #get the filename
      fileName<-sub("recent-","",fileCmd)
      #if file fails to exist remove
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
      dirtyDMDM(session, "editNavBar")
    }
    
    
    
  }
  
}) 

# keep file menu save uptodate
observeEvent(getFileSavedStatus(),{
  if(getFileSavedStatus()==FALSE){
    # set dmdm file save active
    enableDMDM(
      session, 
      menuBarId="editNavBar", 
      entry="Save"
    )
  } else {
    # set dmdm file save inactive
    disableDMDM(
      session, 
      menuBarId="editNavBar", 
      entry="Save"
    )
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

# -----------ACE EDITOR------------------------
observeEvent(
  getCode(), {
    if(mssg$error==""){
      updateAceEditor( session,"source", value=getCode() ) 
    }
  }
)

observe({
  updateAceEditor(session, "source", fontSize=as.numeric(editOption$fontSize) )
})
observe({
  updateAceEditor(session, "source", theme=editOption$theme)
})
observe({
  session$sendCustomMessage(type = "shinyAceExt",  
                            list(id= "source", tabSize=as.numeric(editOption$tabSize)))
})



#----------------------------------------------

