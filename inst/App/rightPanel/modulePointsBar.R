
modulePointsBarUI <- function(id, input, output) { 
  ns <- NS(id)
  tagList(
    absolutePanel( "class"="footerPanel", draggable=FALSE,
          actionButton(ns("forwardPt" ), label = "Forward Pt"),
          actionButton(ns("backwardPt"), label = "Backward Pt"),
          actionButton(ns("removePt"), label = "Delete Pt"),
          actionButton(ns("tagPt"), label = "Tag Pt") 
          
    ),
    absolutePanel( id='header', top=50, left=0, width="100%", "class"="headerPanel", draggable=FALSE,
        div(style="display:inline-block",
              selectInput( ns("name"), "Point Matrix", list("x"), 
                           selected="x", multiple=FALSE,  selectize = FALSE,
                           width="150px",size=1  )
        ),
        div(style="display:inline-block",
              selectInput(ns("displayMode"), "Display Mode",
                          list("Normal","Labeled","Hidden"), selected="Normal", 
                          multiple=FALSE, selectize = FALSE,
                          width="150px", size=1 )
        ),
        # div(style="display:inline-block",
        #       textOutput(ns("tagFreq"), "Auto Tag",
        #                   c(list("Off"),1:20), selected="Off", 
        #                   multiple=FALSE, selectize = FALSE,
        #                   width="80px", size=1  )
        #),
        div(style="display:inline-block",
              checkboxInput(ns("insertMode" ),"Insert",value = TRUE, width = "50px")
        ),
        div(style="display:inline-block",
              checkboxInput(ns("showGrid"), "Grid",   value = FALSE, width = "50px")
        )
      ) 
  ) #end taglist
} 


modulePointsBar<-function(
        input, output, session,
        barName, 
        getSelectInfo,
        getPtDefs, 
        name, 
        index,
        isTaggable,
        headerId){
  
  result<-reactiveValues( #
    point.index=0
  )
  
  getIndx<-reactive({point.indx})
  output$indx<-renderText({ getIndx() })
  
  triggerRefresh<-function(sender, rollBack=TRUE, auxValue=FALSE){ # to be used to force a code refresh???
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= "source", sender=sender, getValue=TRUE, rollBack=rollBack, auxValue=auxValue)
    )
  }
  
  # !!! Unclear what this observer is ablout
  observe({ # updates name when changing points using mouse
    if(identical( barName(), 'Points')){
      sender=paste0(barName(),'.point')
      triggerRefresh(sender, rollBack=TRUE)
      # session$sendCustomMessage(
      #   type = "shinyAceExt",
      #   list(id= "source", sender=sender, getValue=TRUE)
      # )
      ptRList<-getPtDefs()$pts #
      if(length(names(ptRList))==0){
        hideElement( headerId )
      } else {
        showElement( headerId)
      }
      res<-getSelectInfo() # triggerd by changes for getPtName, getPtIndex. getPtDefs
      result$point.index<-res$point.index
      updateSelectInput(session, "name",
                        choices=names(ptRList),
                        selected= res$selected ) #res$selected is name
    } 
  })
  
  # observe({
  #   ptRList<-getPtDefs()$pts #trigger is name or index
  #   #res<-getSelectInfo()
  #   if(result$point.index<=1){
  #     disable("backwardPt")
  #   } else {
  #     enable("backwardPt")
  #   }
  # })
  
  observe({
    selection<-getSelectInfo()$selected
    ptRList<-getPtDefs()$pts
    len<-length(ptRList[[selection ]])/2
    # if(result$point.index>=len){
    #     disable("forwardPt")
    #   } else {
    #     enable("forwardPt")
    #   }
  })
  
  # observe(
  #   if(isTaggable()){
  #     enable("tagPt")
  #   } else {
  #     disable("tagPt")
  #   }
  # )
  
 
  #---selected point forward button-----
  observeEvent(input$forwardPt,{
    selection<-input$name
    #selection<-input$ptRSelect
    ptRList<-getPtDefs()$pts
    len<-length(ptRList[[selection ]])/2
    result$point.index<-min(len, result$point.index+1)
  })
  

  #---selected point backward button-----
  observeEvent(input$backwardPt,{
    #decrement selectedPointIndex
    selection<-input$name
    #selection<-selectedPoint$name
    ptRList<-getPtDefs()$pts
    len<-length(ptRList[[selection ]])/2
    if(len>0){
      result$point.index<-max(1,result$point.index-1)
    } else {
      result$point.index<-0
    }
  })
  
  list(
    name         =reactive({input$name}),
    index        =reactive({result$point.index}),    
    displayMode  =reactive({input$displayMode}),
    #tagFreq      =reactive({input$tagFreq}),
    insertMode   =reactive({input$insertMode}),
    showGrid     =reactive({input$showGrid}),
    removePt     =reactive({input$removePt}),
    tagPt        =reactive({input$tagPt})
  )  
}

