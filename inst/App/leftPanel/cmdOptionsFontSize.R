

cmdFileFontSize<-reactive({
  fileCmd<-input$editNavBar #This is necessary to trigger reactive expression
  
  fontsSizes<-6:36
  modalFontSize <- function() {
    modalDialog(
      selectInput("selectFontSize", "Select Font Size", fontsSizes, 
                  multiple=FALSE, selected=editOption$fontSize,
                  selectize = FALSE, width="90px", size=1  ), 
      footer = tagList(actionButton("modalFontSizeCancel", "Cancel"),actionButton("modalFontSizeOk", "OK") )
    ) 
  }
  showModal( modalFontSize() )
})

observeEvent(input$modalFontSizeCancel, {
  updateNavbarPage(session, "editNavBar", selected ="tab1")
  removeModal()
}) 

observeEvent(input$modalFontSizeOk, {
  #nextSize<-select.list(choices = 6:24, graphics=TRUE)
  editOption$fontSize<-input$selectFontSize
  #updateAceEditor(session, "source", fontSize=as.numeric(editOption$fontSize) )
  updateNavbarPage(session, "editNavBar", selected ="tab1")  
  removeModal()
})

