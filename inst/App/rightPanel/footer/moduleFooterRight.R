moduleFooterRightUI<-function(id, input, output) { 
  ns <- NS(id)
  #tagList(
    absolutePanel( "class"="footerPanel", draggable=FALSE,
          uiOutput(ns('footer'))         
    )
  #)
}

moduleFooterRight<-function(input, output, session, 
                      getTibEditState,
                      getPanelState,
                      getHandler,
                      getHandlerValue
                      
){
  
  output$footer<-renderUI({
    footerPanelState<-getPanelState()
    if(!is.null( footerPanelState )){
        if (footerPanelState=='point'){
          tagList(
            actionButton(session$ns("forwardPt" ), label = "Forward Pt"),
            actionButton(session$ns("backwardPt"), label = "Backward Pt"),
            actionButton(session$ns("removePt"), label = "Delete Pt"),
            actionButton(session$ns("tagPt"), label = "Tag Pt")
          )
        } else if (footerPanelState=='matrix'){
          tagList(
            actionButton(session$ns("tagClone"),   label = "Clone"   ),
            actionButton(session$ns("tagDelete"),  label = "Delete"),
            actionButton(session$ns("tagMoveUp"), label = "Send Up"),
            actionButton(session$ns("tagMoveDown"), label = "Send  Down")
          )
        } else if (footerPanelState=='value'){
          if( !is.null(getHandler() ) && getHandler()=='colourable'){
            value<-!is.null( getHandlerValue() ) && getHandlerValue()=='colourable'
            checkboxInput(session$ns("useColourPalette"), label = "Use colour palette", value=value)
          }
        }
    } else {
      NULL
    }
  })
  
  list( 
    tagClone    =reactive({input$tagClone}) ,
    tagDelete   =reactive({input$tagDelete}),
    tagMoveUp   =reactive({input$tagMoveUp}),
    tagMoveDown   =reactive({input$tagMoveDown}),
    backwardPt   = reactive(input$backwardPt),
    forwardPt    = reactive(input$forwardPt),
    removePt     =reactive({input$removePt}),
    tagPt        =reactive({input$tagPt}),
    useColourPalette = reactive({input$useColourPalette})
  )
}