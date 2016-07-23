
#ui
output$PointsPanel<-renderUI({
  conditionalPanel( "input.plotNavBar=='Points'", 
    absolutePanel( top=50, left=0, width=650, draggable=TRUE,
      style=cstyle$wellPoint,
      fluidRow(
        column(4, 
          selectInput("ptRSelect", "Point Matrix", list("x"), 
            selected="x", multiple=FALSE,  selectize = FALSE,
            width="150px",size=1  )
        ),
        column(3,
          selectInput("ptDisplayMode", "Display Mode",
            list("Normal","Labeled","Hidden"), selected="Normal", 
            multiple=FALSE, selectize = FALSE,
            width="150px", size=1 )
        ),
        column(2,
          selectInput("tagFreq", "Auto Tag",
            c(list("Off"),1:20), selected="Off", 
            multiple=FALSE, selectize = FALSE,
            width="80px", size=1  )
        ),
        column(3,
          checkboxInput("insertMode","Insert",value = TRUE, width = "100px"),
          checkboxInput("showGrid", "Grid",   value = TRUE, width = "100px")
        )
      )) #end Points 
    )
})

#server
# -----------ACTIVE POINT MATRIX------------------------
#  observes code and plotNavBar
#  sets active Point, point selection,  and selectedPoint$point.index
observe({
user$code
  #input$plotNavBar
  name<-selectedPoint$name
  point.index<-selectedPoint$point.index  
  if(input$plotNavBar=='Points'){
    isolate({
      ptRList<-getPtDefs()$pts
      res<-ex.getSelectInfo(ptRList, name, point.index)
      selectedPoint$point.index<-res$point.index
      updateSelectInput(session, "ptRSelect",
                        choices=names(ptRList),
                        selected= res$selected )
    })
  }
})

# fires when ptRmatrix changes
observe({
  input$ptRSelect
  if(input$plotNavBar=='Points'){
    isolate({
      if(!is.null(input$ptRSelect)){
        selectedPoint$name<-input$ptRSelect
        #ptRList<-getPtDefs()$pts
  #      selectedPoint$point.index<-length(ptRList[[input$ptRSelect]])
  #      selected=reactiveTag$freq[[input$ptRSelect]] 
  #      if(is.null(selected)){
  #        selected<-"Off"
  #      }
  #      updateSelectInput(session, "tagFreq", selected=selected )
      }
    })
  }
})

observe({
  displayOptions$insertMode<-input$insertMode
  displayOptions$showGrid<-input$showGrid
  displayOptions$ptMode<-input$ptDisplayMode
  #showOptions$showGrid<-input$showGrid
})


