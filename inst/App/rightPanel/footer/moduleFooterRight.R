moduleFooterRightUI<-function(id, input, output) { 
  ns <- NS(id)
          uiOutput(ns('footer'))         
}

moduleFooterRight<-function(input, output, session, 
                      getTibEditState,
                      getPointMax,
                      getPanelState
){
  
  output$footer<-renderUI({
    footerPanelState<-getPanelState()
    if(!is.null( footerPanelState )){
          if (footerPanelState=='point' && length(getPointMax())>0 ){
              absolutePanel( "class"="footerPanel", draggable=FALSE, style="bottom: 0; left: 10px; display:inline-block",
                  absolutePanel(  id='rightFooterPointButtons',  bottom=5,  
                     actionGroupButtons(
                        inputIds=c(session$ns("forwardPt" ),session$ns("backwardPt" ),session$ns("removePt" ),session$ns("tagPt" )),
                        labels=list(
                          span(span('class'="icon-circle"), span('class'="icon-right-big"))  %>% bs_embed_tooltip(title = "Forward Select"),
                          span(span('class'="icon-circle"), span('class'="icon-left-big")) %>% bs_embed_tooltip(title = "Backward Select"),
                          span(span('class'="icon-circle"), span('class'="icon-cancel")) %>% bs_embed_tooltip(title = "Delete Select"),
                          span(span('class'="icon-circle"), span('class'="icon-fork")) %>% bs_embed_tooltip(title = "Split Select")
                        ),
                        status='primary'
                       )
                  ),
                  absolutePanel( right=90,  bottom=10, width=110,
                      absolutePanel( left=0, bottom=0, 'class'='ptR2', style="font-weight: bold; color: #00FFFF;", span('max pts/row:')),
                      absolutePanel( left=90, bottom=-20, 'class'='ptR2',
                          numericInput(session$ns("matColLim"), width='70px', label = NULL, min=1, value = getPointMax() )
                      )
                  )
              )
          } else if (footerPanelState=='matrix'){
            absolutePanel( "class"="footerPanel", draggable=FALSE, style="bottom: 0; left: 10px; display:inline-block",
              absolutePanel(  id='rightFooterMatrixButtons', bottom=5, 
                actionGroupButtons(
                  inputIds=c(session$ns("tagMoveUp" ),session$ns("tagMoveDown" ),session$ns("tagDelete" ),session$ns("tagClone" )),
                  labels=list(
                    span(span('class'="icon-window-minimize"), span('class'="icon-up-big"))  %>% bs_embed_tooltip(title = "Send Up Row"),
                    span(span('class'="icon-window-minimize"), span('class'="icon-down-big")) %>% bs_embed_tooltip(title = "Send  Down Row"),
                    span(span('class'="icon-window-minimize"), span('class'="icon-cancel")) %>% bs_embed_tooltip(title = "Delete Row"),
                    span(span('class'="icon-window-minimize"), span('class'="icon-clone")) %>% bs_embed_tooltip(title = "Clone Row")
                  ),
                  status='primary'
                )
              )
            )
          } else if (footerPanelState=='value'){
          }
      
    } else {
      NULL
    }
  })
  
  list( 
    tagClone    =reactive({input$tagClone}) ,
    tagDelete   =reactive({input$tagDelete}),
    tagMoveUp   =reactive({input$tagMoveUp}),
    tagMoveDown  =reactive({input$tagMoveDown}),
    backwardPt   = reactive(input$backwardPt),
    forwardPt    = reactive(input$forwardPt),
    removePt     =reactive({input$removePt}),
    tagPt        =reactive({input$tagPt}),
    matColLim    = reactive({input$matColLim}),
    useColourPalette = reactive({input$useColourPalette})
  )
}