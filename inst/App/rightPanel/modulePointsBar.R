
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
              selectInput( ns("name"), "Name", list("x"), 
                           selected="x", multiple=FALSE,  selectize = FALSE,
                           width="100px",size=1  )#,
               
              # numericInput( ns("col"), "matrix col", 1, min=1, step=1,
              #               width="50px" )
        ),
        div(style="display:inline-block",
            numericInput( ns("rowIndex"), "Row", 1, min=1, max=10, step=1, width="80px" )
        ),
        div(style="display:inline-block",
            numericInput( ns("matColIndex"), "Mat Col", 1, min=1, max=10, step=1, width="80px" )
        ),
        div(style="display:inline-block",
            numericInput( ns("ptIndx"), "ptIndx", 1, min=1, max=10, step=1, width="80px" )
        ),
        div(style="display:inline-block",
              selectInput(ns("displayMode"), "Display Mode",
                          list("Normal","Labeled","Hidden"), selected="Normal", 
                          multiple=FALSE, selectize = FALSE,
                          width="100px", size=1 )
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

#TODO name,  row, rowRange, matRange, matIndex
modulePointsBar<-function(
        input, output, session,
        barName,
        #getTibPtsColEndIndex, #not used??
        #getSelectInfo, 
        #getPtDefs, 
        name, 
        nameChoices,
        ptIndex,
        ptIndexChoices,
        rowIndex,
        rowIndexChoices,
        matColIndex,
        matColIndexChoices,
        #isTaggable,
        headerId){
  
  result<-reactiveValues( #
    point.index=0,
    rowIndex=1,
    matColIndex=0
  )
  
  triggerRefresh<-function(sender, rollBack=TRUE, auxValue=FALSE){ # to be used to force a code refresh???
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= "source", sender=sender, getValue=TRUE, rollBack=rollBack, auxValue=auxValue)
    )
  }
  

  observeEvent(barName(), { # updates only when either barName changes to points
    if(identical( barName(), 'Points')){
      # cat("ModulePointsBar:: observeEvent 123\n")
      # cat("nameChoices=")
      # print(nameChoices())
      if(length(nameChoices())==0){ #name choices
        hideElement( headerId )
      } else {
        showElement( headerId)      
        updateSelectInput(session, "name", choices=nameChoices(), selected= name()  )
        updateNumericInput(session, "ptIndx", 
                           min=min(ptIndexChoices()),
                           max=max(ptIndexChoices()),
                           value=ptIndex()
         )
        # result$rowIndex=rowIndex()
        # result$matColIndex=matColIndex()
        # #updateSelectInput(session, "rowIndex", choices=rowIndexChoices(), selected=rowIndex() )
        updateNumericInput(session, "rowIndex", 
                           min=min(rowIndexChoices()),
                           max=max(rowIndexChoices()),
                           value=rowIndex()
        )
        updateNumericInput(session, "matColIndex", 
                           min=min(matColIndexChoices()),
                           max=max(matColIndexChoices()),
                           value=matColIndex()
        )
        
        result$point.index<-ptIndex()
        result$rowIndex<-rowIndex()
        result$matColIndex<-matColIndex()
      }
    } 
  })
  
  
  
  observeEvent( c(ptIndex(),ptIndexChoices()), {
    if(identical( barName(), 'Points')){
      # cat("ModulePointsBar:: observeEvent 124\n")
      choices=ptIndexChoices()
      # cat("ModulePointsBar:: observeEvent choices\n")
      # print(choices)
      # print(ptIndex())
      updateSelectInput(session, "ptIndx", choices=choices, selected=ptIndex())
      result$point.index=ptIndex()
      # if( result$point.index < max(ptIndexChoices())){
      #   enable("forwardPt")
      # } else {
      #   disable("forwardPt")
      # }
      # if( result$point.index > min(ptIndexChoices())){
      #   enable("backwardPt")
      # } else {
      #   disable("backwardPt")
      # }
      # cat('leaving ModulePointsBar:: observeEvent 124\n')
    }
  } )
  
  observeEvent( c(barName(), matColIndex(),matColIndexChoices()), {
    if(identical( barName(), 'Points')){
      # cat("ModulePointsBar:: observeEvent 124\n")
      choices=matColIndexChoices()
      if( matColIndex() < max(matColIndexChoices())){
        enable("forwardPt")
      } else {
        disable("forwardPt")
      }
      if( matColIndex() > min(matColIndexChoices())){
        enable("backwardPt")
      } else {
        disable("backwardPt")
      }
      # cat('leaving ModulePointsBar:: observeEvent 124\n')
    }
  } )
  
  observeEvent( matColIndex(), { #!!! this allows for empty rows of points
    if(matColIndex()!=0){
      cat("matColIndex()",matColIndex(),"\n")
      enable("removePt")
      enable("tagPt")
    } else {
      disable("removePt")
      disable("tagPt")
    }
  })
  
  
  
  observeEvent( c( rowIndex(),rowIndexChoices(), matColIndex(), matColIndexChoices() ),{
    if(identical( barName(), 'Points')){
      # cat("ModulePointsBar:: observeEvent 125\n")
      # cat("ModulePointsBar:: observeEvent rowChoices\n")
      # print("rowIndex")
      # print(rowIndex())
      # print("matColIndex")
      # print(matColIndex())
      # result$rowIndex=rowIndex()
      # result$matColIndex=matColIndex()
      #updateSelectInput(session, "rowIndex", choices=rowIndexChoices(), selected=rowIndex() )
      updateNumericInput(session, "rowIndex", 
                         min=min(rowIndexChoices()),
                         max=max(rowIndexChoices()),
                         value=rowIndex()
      )
      updateNumericInput(session, "matColIndex", 
                         min=min(matColIndexChoices()),
                         max=max(matColIndexChoices()),
                         value=matColIndex()
      )
      #updateSelectInput(session, "matColIndex", choices=matColIndexChoices(), selected=matColIndex() )
    }
  })

  
  
  
  #---selected point forward button-----
  # observeEvent(input$forwardPt,{
  #   len<-max(getPtIndexChoices)
  #   if(length(ptIndex())>0){
  #     result$point.index<-min(len, ptIndex())
  #   } else {
  #     result$point.index<-0
  #   }
  # })
  

  #---selected point backward button-----
  # observeEvent(input$backwardPt,{
  #   if(length(ptIndex())>0){
  #     result$point.index<-max(0, ptIndex()-1)
  #   } else {
  #     result$point.index<-0
  #   }
  # })
  
  list(
    name         =reactive({input$name}),
    pointIndex   =reactive({result$point.index}), 
    rowIndex     = reactive(input$rowIndex),
    matColIndex   = reactive(input$matColIndex),
    displayMode  =reactive({input$displayMode}),
    #tagFreq      =reactive({input$tagFreq}),
    backwardPt   = reactive(input$backwardPt),
    forwardPt    = reactive(input$forwardPt),
    insertMode   =reactive({input$insertMode}),
    showGrid     =reactive({input$showGrid}),
    removePt     =reactive({input$removePt}),
    tagPt        =reactive({input$tagPt})
  )  
}

