

observeEvent( input$editNavBar, { 
  fileCmd<-input$editNavBar$item
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
    if(fileCmd=="Indentation"){
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
    
  }
  
}) 

#- editor options handlers

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
  user$code, {
    if(mssg$error==""){
      updateAceEditor( session,"source", value=user$code)
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

