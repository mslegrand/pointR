
modulePointsBarUI <- function(id, input, output) { 
  ns <- NS(id)
  #tagList(
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
    ) #end absolutePanel
  #) #end taglist
} 


modulePointsBar<-function(
        input, output, session,
        barName, 
        getSelectInfo,
        getPtDefs, 
        name, 
        index ){
  
  result<-reactiveValues(
    point.index=0
  )
  
  observe({
    if(identical( barName(), 'Points')){
      ptRList<-getPtDefs()$pts
      res<-getSelectInfo()
      result$point.index<-res$point.index
      updateSelectInput(session, "name",
                        choices=names(ptRList),
                        selected= res$selected )
    } 
  }) 
  
  list(
    name         =reactive({input$name}),
    index        =reactive({result$point.index}),    
    displayMode  =reactive({input$displayMode}),
    tagFreq      =reactive({input$tagFreq}),
    insertMode   =reactive({input$insertMode}),
    showGrid     =reactive({input$showGrid})
  )  
}

