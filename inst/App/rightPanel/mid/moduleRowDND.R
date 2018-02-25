moduleRowDNDUI<-function(id, input, output) { 
  ns <- NS(id)  
  #tagList(
    absolutePanel( "class"= "cRowContainer",
                   uiOutput(ns("rowPanel"))
    )
  #)
}

moduleRowDND<-function(input, output, session, 
    getTibNRow,  
    getRowIndex
){
  ns <- session$ns
  output$rowPanel<-renderUI({
    if( getTibNRow()>0 ){
      rowIndx<-getRowIndex()
      N<-getTibNRow()
      if( !is.null(rowIndx) && !is.null(N)){
        sortableRadioButtons(ns("rowIndex"), label=NULL,
                             choices=1:(getTibNRow()),
                             selected= getRowIndex() #getSelectedRow()
        )
      }
    }
  })
  observeEvent(getRowIndex(),{
    cat("observeEvent(getRowIndex())\n")
    cat("getTibNRow()=",format(getTibNRow()),"\n")
  })
  observeEvent(input$rowIndex,{
    cat("observeEvent(input$rowIndex)\n")
  })
  
  list(
    rowIndex      = reactive({input$rowIndex}),
    rowReorder      = reactive({input$rowIndex_order})
  )
}
  