
modulePointsBarUI <- function(id, input, output) { 
  ns <- NS(id)
  tagList(
    # beginfooter panel
    absolutePanel( "class"="footerPanel", draggable=FALSE,
          actionButton(ns("forwardPt" ), label = "Forward Pt"),
          actionButton(ns("backwardPt"), label = "Backward Pt"),
          actionButton(ns("removePt"), label = "Delete Pt"),
          actionButton(ns("tagPt"), label = "Tag Pt") 
          
    ),
    #end footer panel
    #begin headerPanel
    #absolutePanel( id='header', top=50, left=0, width="100%", "class"="headerPanel", draggable=FALSE,
    absolutePanel( id='header',
        top=50, left=0, width="100%", "class"="headerPanel", draggable=FALSE, height="80px"
        #bottom=0, #,
    ),                
        #div(style="display:inline-block",
            absolutePanel( top=50, left=15 ,style="display:inline-block",
            selectizeInput( ns("name"), "Data", choices=list("x"),  selected="x",
                            width= '100px'
            )
            
            # selectInput(ns('name'),'Name',list('x'),
            #             selected='x', multiple=FALSE, selectize = FALSE,
            #             width='100px', size=1)
        ),
        absolutePanel( top=50, left=125 ,
                       numericInput( ns("rowIndex"), "Row", value=1, 
                                     min=1, max=10, step=1, 
                                     width= '80px' 
                       )
        ), 
        absolutePanel( top=50, left=210 ,
                       numericInput(ns("matColIndex"), label="Mat Col", value=1,
                                      min=1, max=1, step=1,
                                      width= '80px' 
                       )
        ),
        # absolutePanel( top=50, left=295 ,
        #                selectizeInput(ns("displayMode"), label="Display Mode",
        #                   choices=list("Normal","Labeled","Hidden"), selected="Normal", 
        #                   width="100px" 
        #               )
        # ),
        absolutePanel( top=50, left=400,
              checkboxInput(ns("insertMode" ),"Insert",value = TRUE, width = "50px")
        )
      #) 
  ) #end taglist
} 

#TODO name,  row, rowRange, matRange, matIndex
modulePointsBar<-function(
        input, output, session,
        barName,
        name, 
        nameChoices,
        rowIndex,
        rowIndexChoices,
        matColIndex,
        matColIndexChoices,
        headerId){
  
  result<-reactiveValues( #
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
        # updateNumericInput(session, "ptIndx", 
        #                    min=min(ptIndexChoices()),
        #                    max=max(ptIndexChoices()),
        #                    value=ptIndex()
        #  )
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
        result$rowIndex<-rowIndex()
        result$matColIndex<-matColIndex()
      }
    } 
  })
  
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
    }
  } )
  
  observeEvent( matColIndex(), { #!!! this allows for empty rows of points
    if(matColIndex()!=0){
      enable("removePt")
      enable("tagPt")
    } else {
      disable("removePt")
      disable("tagPt")
    }
  })
  
  
  
  observeEvent( c( rowIndex(),rowIndexChoices(), matColIndex(), matColIndexChoices() ),{
    if(identical( barName(), 'Points')){
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
    }
  })

  
  
  
  list(
    name         =reactive({input$name}),
    rowIndex     = reactive(input$rowIndex),
    matColIndex   = reactive(input$matColIndex),
    backwardPt   = reactive(input$backwardPt),
    forwardPt    = reactive(input$forwardPt),
    insertMode   =reactive({input$insertMode}),
    showGrid     =reactive({input$showGrid}),
    removePt     =reactive({input$removePt}),
    tagPt        =reactive({input$tagPt})
  )  
}

