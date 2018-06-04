moduleEdTransformUI<-function(id, input, output) { 
  ns <- NS(id)
    div(  'class'='topHeadCol2 topHeadRow2 ptR2',  
          uiOutput(ns("transformPanelContainer"))
    )
}

moduleEdTransform<-function(input, output, session, 
                      assetName, 
                      getTransformType
){
  ns<-session$ns
  
  output$transformPanelContainer<-renderUI({
    if( !is.null(assetName()) && assetName()==transformTag ){
      div( 
      tabsetPanel( id=ns("transformType"),
                   tabPanel("Translate"),
                   tabPanel("Rotate"),
                   tabPanel("Scale"),
                   type="pills"
      ))
    } 
  })
  
  observeEvent( getTransformType(), {
    if(identical(assetName(), transformTag) ){
        updateTabsetPanel(session, input$transformType, selected=getTransformType() )
    }
  }, ignoreNULL = TRUE)
  
  list(
    type=reactive(input$transformType)
  )
}
  