

observeEvent( input$editNavBar, { 
  fileCmd<-input$editNavBar
  if(fileCmd=="New"){ #-----new
    cmdFileNew()
  }
  if(fileCmd=="Open"){ #-----open 
    cmdFileOpen()
  }
  if(fileCmd=="SaveAs"){ #-----save
    cmdFileSaveAs()
  } 
  if(fileCmd=="Save"){ #-----save
    curFile<-getCurrentFile()
    if(nchar(curFile)>0){
      cmdFileSave()
    } else {
      cmdFileSaveAs()
    }
    
    # fileName=""
    # default="newfile.R"
    # #TODO alert if editOption$.unNamed==TRUE
    # try(fileName<-dlgSave(title = "Save R script to", 
    #                       filters = dlgFilters[c("R", "All"), ])$res
    # )
    # if(length(fileName)>0 && nchar(fileName)>0){
    #   file$name<-fileName
    #   isolate({
    #     editOption$.unNamed<-FALSE
    #     editOption$.notSaved<-FALSE
    #   })
    #   txt<-user$code
    #   writeLines(txt, fileName)
    #   editOption$currentFile<-basename(fileName)
    #   editOption$currentDirectory<-dirname(fileName) 
    # }
    # updateNavbarPage(session, "editNavBar", selected ="tab1")
  }
  if(fileCmd=="quit"){
    cmdFileQuit()
    # TODO!!! ALERT if need to save, ie. editOption$.notSaved==TRUE
    # opts<-isolate(reactiveValuesToList((editOption)))
    # opts<-unlist(opts)
    # writeOptions(optionFile, opts)
    # js$closeWindow()
    # stopApp()
    #q("no")
  }
  if(fileCmd=="Export as SVG"){ #-----save
    cmdFileExportSvg()
  }
  if(fileCmd=="Theme"){
    cmdOptionsTheme()
    # themes<-getAceThemes()
    # modalTheme <- function() {
    #   modalDialog(
    #     selectInput("selectTheme", "Theme", themes, multiple=FALSE, 
    #                 selected=editOption$theme,
    #                 selectize = FALSE, width="300px", size=1  ), 
    #     footer = tagList(actionButton("modalThemeCancel", "Cancel"),actionButton("modalThemeOk", "OK") )
    #   ) 
    # }
    # showModal( modalTheme() )
  } 
  if(fileCmd=="Font Size"){
    cmdFileFontSize()
    # fontsSizes<-6:36
    # modalFontSize <- function() {
    #   modalDialog(
    #     selectInput("selectFontSize", "Select Font Size", fontsSizes, 
    #                 multiple=FALSE, selected=editOption$fontSize,
    #                 selectize = FALSE, width="90px", size=1  ), 
    #     footer = tagList(actionButton("modalFontSizeCancel", "Cancel"),actionButton("modalFontSizeOk", "OK") )
    #   ) 
    # }
    # showModal( modalFontSize() )
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
  }
  
}) 

#- editor options handlers

observeEvent(input$modalIndentSizeCancel, {
  updateNavbarPage(session, "editNavBar", selected ="tab1")
  removeModal()
}) 

observeEvent(input$modalIndentSizeOk, {
  editOption$tabSize<-input$selectIndentSize
  #session$sendCustomMessage(type = "shinyAceExt",  
  #      list(id= "source", tabSize=as.numeric(editOption$tabSize))) 
  updateNavbarPage(session, "editNavBar", selected ="tab1")  
  removeModal()
})

# observeEvent(input$modalFontSizeCancel, {
#   updateNavbarPage(session, "editNavBar", selected ="tab1")
#   removeModal()
# }) 

# observeEvent(input$modalFontSizeOk, {
#   #nextSize<-select.list(choices = 6:24, graphics=TRUE)
#   editOption$fontSize<-input$selectFontSize
#   #updateAceEditor(session, "source", fontSize=as.numeric(editOption$fontSize) )
#   updateNavbarPage(session, "editNavBar", selected ="tab1")  
#   removeModal()
# })


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

