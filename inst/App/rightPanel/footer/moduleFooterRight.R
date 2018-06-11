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
                      getPointMax,
                      getPanelState #,
                      # getHandler,
                      # getHandlerValue
                      
){
  
  output$footer<-renderUI({
    footerPanelState<-getPanelState()
    if(!is.null( footerPanelState )){
        if (footerPanelState=='point' && length(getPointMax())>0 ){
          # cat("length(getPointMax())=",length(getPointMax()),"\n")
          # cat("getPointMax()=",format(getPointMax()),"\n")
          absolutePanel( "class"="footerPanel", draggable=FALSE, style="bottom: 0; left: 10px; display:inline-block", 
            absolutePanel( left=5, bottom=0,
               actionGroupButtons(
                  inputIds=c(session$ns("forwardPt" ),session$ns("backwardPt" ),session$ns("removePt" ),session$ns("tagPt" )),
                  labels=list(
                    span(span('class'="icon-circle"), span('class'="icon-right-big"))  %>% bs_embed_tooltip(title = "Forward Select"),
                    span(span('class'="icon-circle"), span('class'="icon-left-big")) %>% bs_embed_tooltip(title = "Backward Select"),
                    span(span('class'="icon-circle"), span('class'="icon-cancel")) %>% bs_embed_tooltip(title = "Delete Select"),
                    #span(span('class'="icon-circle"), span('class'="icon-split")) %>% bs_embed_tooltip(title = "Split Select")
                    span(span('class'="icon-circle"), span('class'="icon-fork")) %>% bs_embed_tooltip(title = "Split Select")
                  ),
                  status='primary'
                 )          
            ),
            absolutePanel( left=450, bottom=5, 'class'='ptR2', style="font-weight: bold; color: #00FFFF;", span('max pts/row:')),
            absolutePanel( left=540, bottom=-15, 'class'='ptR2', 
                numericInput(session$ns("matColLim"), width='70px', label = NULL, min=1, 
                             value = getPointMax()
                )
            )
          )
        } else if (footerPanelState=='matrix'){
          actionGroupButtons(
            inputIds=c(session$ns("tagMoveUp" ),session$ns("tagMoveDown" ),session$ns("tagDelete" ),session$ns("tagClone" )),
            labels=list(
              span(span('class'="icon-window-minimize"), span('class'="icon-up-big"))  %>% bs_embed_tooltip(title = "Send Up Row"),
              span(span('class'="icon-window-minimize"), span('class'="icon-down-big")) %>% bs_embed_tooltip(title = "Send  Down Row"),
              span(span('class'="icon-window-minimize"), span('class'="icon-cancel")) %>% bs_embed_tooltip(title = "Delete Row"),
              #span(span('class'="icon-circle"), span('class'="icon-split")) %>% bs_embed_tooltip(title = "Split Select")
              span(span('class'="icon-window-minimize"), span('class'="icon-clone")) %>% bs_embed_tooltip(title = "Clone Row")
            ),
            status='primary'
          )      
          # tagList(
          #   #  icon-up-big   icon-down-big icon-cancel icon-clone  icon-level-up icon-level-down  icon-menu icon-window-minimize
          #   
          #   actionButton(session$ns("tagDelete"),  label = "Delete Select"),
          #   actionButton(session$ns("tagMoveUp"),  label = "Send Up Select"),
          #   actionButton(session$ns("tagMoveDown"), label = "Send  Down Select"),
          #   actionButton(session$ns("tagClone"),   label = "Clone Select"   )
          # )
        } else if (footerPanelState=='value'){
          # if( !is.null(getHandler() ) && getHandler()=='colourable'){
          #   value<-!is.null( getHandlerValue() ) && getHandlerValue()=='colourable'
          #   checkboxInput(session$ns("useColourPalette"), label = "Use colour palette", value=value)
          # }
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