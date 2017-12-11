
#currently update triggers value from choice to ace
# propose
moduleTagValUI<-function(id, input, output) { 
  ns <- NS(id)
  #useShinyjs()
  tagList(
    absolutePanel( "class"="footerPanel", draggable=FALSE,
                   actionButton(ns("newColumn"),   label = "Add Column"   ),
                   actionButton(ns("deleteColumn"),  label = "Delete Column")#,
                   #actionButton(ns("applyTibEdit"), label = "Apply Changes") 
    ), #end footer panel
    absolutePanel( 
      top=50, left=0, width="100%", "class"="headerPanel", draggable=FALSE,
      div(style="display:inline-block",
        selectInput( ns("name"), "Tibble",
        multiple=FALSE, size=1, selectize = FALSE,
        choices=list(),  selected=NULL, width="80px"  )
      ),
      div(style="display:inline-block",
          numericInput( ns("rowIndex"), "Row", 1, min=1, max=10, step=1, width="80px" )
      ), 
      div(style="display:inline-block",
        selectInput(ns("columnName"), label="Column",
        multiple=FALSE, size=1, selectize = FALSE, 
        choices=list(),  selected=NULL, width="120px"  )
      ),
      div(style="display:inline-block", 
          selectizeInput(ns("entryValue"), "Value", 
          #multiple=FALSE, #size=1, 
          options = list(create = TRUE),
        #selectize = TRUE,  
          choices=list(),  selected=NULL, width="100px"  )
      )
  ) # end header panel
  )
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