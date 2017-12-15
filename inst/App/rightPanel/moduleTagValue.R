

#currently update triggers value from choice to ace
# propose
# tagValHeadWidth<-c(100,70,120,200)
# tagValHeadLeft<-cumsum(c(10,tagValHeadWidth)+5)
# tagValHeadWidth<-paste0(tagValHeadWidth,"px")
# tagValHeadLeft<-paste0(tagValHeadLeft,"px")

moduleTagValUI<-function(id, input, output) { 
  ns <- NS(id)
  #useShinyjs()
  tagList(
    # beginfooter panel
    absolutePanel( "class"="footerPanel", draggable=FALSE,
                   actionButton(ns("newColumn"),    label = "Add Column"   ),
                   actionButton(ns("deleteColumn"),  label = "Delete Column")
    ), 
    #end footer panel
    #begin headerPanel
    absolutePanel( 
        top=50, left=0, width="100%", "class"="headerPanel", draggable=FALSE, height="80px"
    ),
        #bottom=0, #height="200px",
        #div(style="display:inline-block",
        absolutePanel( top=50, left=15 ,style="display:inline-block",
          selectizeInput( ns("name"), "Data", choices=list(),  selected=NULL, 
            width= '100px' 
          )
        ),
        absolutePanel( top=50, left=125 ,
          numericInput( ns("rowIndex"), "Row", 1, min=1, max=10, step=1, 
             width= '70px' 
          )
        ), 
        absolutePanel( top=50, left=200 ,
          selectizeInput(ns("columnName"), label="Column",
            choices=list(),  selected=NULL, 
            width= '120px' 
          )
        ),
        absolutePanel( top=50, left=325 ,
          selectizeInput(ns("entryValue"), "Value", 
            options = list(create = TRUE),
            choices=list(),  selected=NULL, 
            width= '200px' 
          )
        )
        # ,
        # absolutePanel(top=0, left=530, 
        #   sliderInput(ns("decimal"), "Decimal:",
        #          min = 0, max = 1, width='200px', 
        #         value = 0.5, step = 0.1)
        # )
      #) # end header panel
  ) # end taglist
}

moduleTagVal<-function(input, output, session, 
  id2,
  barName, 
  name, 
  nameChoices,
  rowIndex,
  rowIndexChoices,
  matColIndex,
  matColIndexChoices,
  columnName,
  columnNameChoices,
  getTibEntry,
  getTibEntryChoices
){
  ns<-NS(id2)
  result <- reactiveValues(
    ptDefs=NULL
  ) # note we add name post mortem!!!
  
  
  
  observeEvent(barName(), { #update the name 
    if(identical( barName(), 'tagValues')){
      
      updateSelectInput(session, "name", 
                        choices=nameChoices(), 
                        selected=name())
      updateNumericInput(
        session, 
        "rowIndex", 
        min=min(rowIndexChoices()),
        max=max(rowIndexChoices()),
        value=rowIndex()
      )
      updateSelectInput(session, "columnName", 
                        choices=columnNameChoices(), 
                        selected=columnName() )
      entry=getTibEntry()
      if(is(entry,'matrix')){
        entry='matrix'
        entryChoices=c('point','matrix')
      } else {
        entryChoices=getTibEntryChoices()
      }
      updateSelectInput(session, "entryValue", 
                        choices=entryChoices, 
                        selected=entry )
    } 
  })  
  
  observeEvent( c(barName(), rowIndex(), rowIndexChoices() ),  { #update index
    if(identical( barName(), 'tagValues')){
      updateNumericInput(
        session, 
        "rowIndex", 
        min=min(rowIndexChoices()),
        max=max(rowIndexChoices()),
        value=rowIndex()
      )
      entry=getTibEntry()
      
      if(is(entry,'matrix')){
        entry='matrix'
        cat("observeEvent( c(barName(), rowIndex(), rowIndexChoices(): entry=", entry, "\n")
        cat("observeEvent( c(barName(), rowIndex(), rowIndexChoices(): input$entryValue=", input$entryValue, "\n")
        entryChoices=c('point','matrix')
        if( !(input$entryValue %in% entryChoices ) ){
          updateSelectInput(session, "entryValue", 
                            choices=entryChoices, 
                            selected=entry )
        }
      } else {
        entryChoices=getTibEntryChoices()
        updateSelectInput(session, "entryValue", 
                          choices=entryChoices, 
                          selected=entry )
      }
    } 
  }) 
  
  observeEvent( columnName() , { 
    if(identical( barName(), 'tagValues')){
      updateSelectInput(session, "columnName", 
                        choices=columnNameChoices(), 
                        selected=columnName() )
      entry=getTibEntry()
      if(is(entry,'matrix')){
        entry='matrix'
        entryChoices=c('point','matrix')
      } else {
        entryChoices=getTibEntryChoices()
      }
      cat("observeEvent( columnName(): entry=", entry, "\n")
      updateSelectInput(session, "entryValue", 
                        choices=entryChoices, 
                        selected=entry )
    } 
  })

  
  #when name, index, attrName valid, and attrVal changes, update the ptDefs and code
  list( 
    name         =reactive({input$name}),
    rowIndex     = reactive({input$rowIndex}),
    columnName    =reactive({input$columnName}),
    entryValue   =reactive(input$entryValue),
    newColumn =    reactive({input$newColumn}),
    deleteColumn = reactive({input$deleteColumn}),
    applyTibEdit = reactive({input$applyTibEdit})
  )
}