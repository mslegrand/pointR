
setMatColMaxModal <- function(columnName, currentValue=1) {
    title<-paste0(
      'Henceforth,  for the column "',
      columnName,
      '" the number of points in each row are to be limited by the following.'
    )
    
    modalDialog(
      span(title),
      numericInput( "tibMatColMax", 'max', currentValue, min=1, step=1,  width= '70px' ),
      checkboxInput(
        'limitTibMatCol', 
        'Apply to limit the no. of columns (points) in matrix',
        value = !is.null(currentValue) 
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("setMatColMaxModalOk", "OK") 
      )
    ) 
}
  
observeEvent(input$setMatColMaxModalOk, {
  if(!is.null(input$tibMatColMax) && !is.na(input$tibMatColMax)){
     updateWidgetChoicesRow(maxVal= input$tibMatColMax )
  } else {
    updateWidgetChoicesRow(maxVal= NULL )
   }
  removeModal()
})
