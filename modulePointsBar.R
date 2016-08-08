
modulePointsBarUI <- function(id, input, output) { 
  ns <- NS(id)
  tagList(
    absolutePanel( bottom=0, left=0, width=650, draggable=FALSE,
                        style="margin:0px; padding:0px;",
          actionButton(ns("forwardPt" ), label = "Forward Pt",  style=cstyle$button),
          actionButton(ns("backwardPt"), label = "Backward Pt", style=cstyle$button),
          actionButton(ns("removePt"), label = "Delete Pt",     style=cstyle$button),
          actionButton(ns("tagPt"), label = "Tag Pt",           style=cstyle$button) 
          
    ),
    absolutePanel( top=50, left=0, width=650, draggable=TRUE,
      style=cstyle$wellPoint,
      fluidRow(
        column(4, 
              selectInput( ns("name"), "Point Matrix", list("x"), 
                           selected="x", multiple=FALSE,  selectize = FALSE,
                           width="150px",size=1  )
        ),
        column(3,
              selectInput(ns("displayMode"), "Display Mode",
                          list("Normal","Labeled","Hidden"), selected="Normal", 
                          multiple=FALSE, selectize = FALSE,
                          width="150px", size=1 )
        ),
        column(2,
              selectInput(ns("tagFreq"), "Auto Tag",
                          c(list("Off"),1:20), selected="Off", 
                          multiple=FALSE, selectize = FALSE,
                          width="80px", size=1  )
        ),
        column(3,
              checkboxInput(ns("insertMode" ),"Insert",value = TRUE, width = "100px"),
              checkboxInput(ns("showGrid"), "Grid",   value = TRUE, width = "100px")
        )
      ) #end fluidRow 
    )  #end absolutePanel
  ) #end taglist
} 


modulePointsBar<-function(
        input, output, session,
        barName, 
        getSelectInfo,
        getPtDefs, 
        name, 
        index ){
  
  result<-reactiveValues( #
    point.index=0
  )
  
  getIndx<-reactive({point.indx})
  output$indx<-renderText({ getIndx() })
  
  observe({ # updates name when changing points using mouse
    if(identical( barName(), 'Points')){
      ptRList<-getPtDefs()$pts #trigger is name or index
      res<-getSelectInfo()
      result$point.index<-res$point.index
      updateSelectInput(session, "name",
                        choices=names(ptRList),
                        selected= res$selected ) #res$selected is name
    } 
  })
  
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
    tagFreq      =reactive({input$tagFreq}),
    insertMode   =reactive({input$insertMode}),
    showGrid     =reactive({input$showGrid}),
    removePt     =reactive({input$removePt}),
    tagPt        =reactive({input$tagPt})
  )  
}

