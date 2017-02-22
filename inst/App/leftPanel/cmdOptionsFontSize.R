cmdFileFontSize<-function(){
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
}

observeEvent(input$modalFontSizeCancel, {
  removeModal()
}) 

observeEvent(input$modalFontSizeOk, {
  editOption$fontSize<-input$selectFontSize
  removeModal()
})

