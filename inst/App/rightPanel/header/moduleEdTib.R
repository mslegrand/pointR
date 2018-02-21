
moduleEdTibUI<-function(id, input, output) { 
  ns <- NS(id)
  top0=40
  top1=top0+32
  top2=top1+32
  top=80
  left0=5
  wid0=90
  left=120
  tagList(
    #begin-----------headerPanel
  
    #---header backdrop
    absolutePanel( id=ns('header'),
         "class"="headerPanel", draggable=FALSE
    ),
    #---name
    #---button
    
    absolutePanel(top= top0, left=left0,
                  div( 'class'="ptRBtn2",
                    actionButton(ns("newAssetsButton"), span(class='icon-plus'," Assets"), width=wid0+20)
                  )
                  
    ),
    #chooser
    absolutePanel(top= top0, left=left,  right=10,  
        uiOutput(ns("dataSetUI"))
    ),
    
    #---column
    # condition: !(name %in% c( transformTag, logTag, svgTag))
    conditionalPanel( 
      condition = sprintf("input['%s'] != '%s' && input['%s'] != '%s' && input['%s'] != '%s'", 
      ns("name"), transformTag, ns("name"), logTag, ns("name"), svgTag),
      #---column
        #---add column button---
        absolutePanel(top= top1, left=left0,
            div( 'class'="ptRBtn2",
                      actionButton(ns("newColumnButton"), span(class='icon-plus'," Variables"), width=wid0+20)
            )
        ),
        #---chooser
        absolutePanel( top=top1, left=left,  right=10,
          uiOutput(ns("columnUI"))
        ),
    #---add entry selection---
    absolutePanel(top= top2, left=left0, 
                  uiOutput(ns("widgetChooserUI"))
    )),
      #---columnEntries
        #---entry chooser
        absolutePanel( top=top2, left=left,  right=10,
                       uiOutput(ns("colEntryUI"))
        ),
   #),
    
    #---transform content---#   display only if selected name is transform
    conditionalPanel( condition = sprintf("input['%s'] == '%s'", ns("name"), transformTag),
      absolutePanel( 
        top=top+25, left=145, width="100%", 
        "class"="headerPanel", draggable=FALSE, "background-color"='#666688',
        tabsetPanel( id=ns("transformType"),  
                     tabPanel("Translate"), 
                     tabPanel("Rotate"), 
                     tabPanel("Scale"),
                     type="pills"
        ) 
      )    
    )
    # ,
    # textInputAddon(inputId = ns('newChoiceXX'), label = "dog", 
    #                placeholder = "Username", addon = icon("at"))
    
    #---tibble content---# display only if input name is a tibble
    #conditionalPanel( condition = sprintf("input['%s'] != '%s' && input['%s'] != '%s' ", ns("name"), transformTag, ns("name"), logTag),
          #---row---
          # absolutePanel( top=top, left=145 ,
          #   numericInput( ns("rowIndex"), "Row", 1, min=1, max=10, step=1, 
          #      width= '70px' 
          #   )
          # ), 
          #---column---
          # absolutePanel( top=top, left=220 ,
          #   selectizeInput(ns("columnName"), label="Column",
          #     choices=list(),  selected=NULL, 
          #     width= '120px' 
          #   )
          # ),
          #uiOutput(ns("columnUI")),
          # ---Entry Input Handlers ---
          #absolutePanel( top=top, left=345, uiOutput(ns("editEntryUI"))),
          # ---point column---- # display if state is  point
          #absolutePanel(top=top, left=550, uiOutput(ns("matColIndexUI"))
        #)
      ) # end taglist
  
}

moduleEdTib<-function(input, output, session, 
  name, 
  nameChoices,
  getRowIndex,
  getRowIndexChoices,
  getTibNRow,
  matColIndex,
  matColIndexChoices, 
  getMatColIndex,
  getMatColMax,
  getColumnName,
  getColumnNameChoices,
  getTibEntry,
  getTibEntryChoices, 
  getTibEditState,
  getTransformType,
  getWidgetChoices,
  getWidget
){
  ns <- session$ns

  entry<-reactiveValues(
    picker="buttons",
    result=NULL
  )

  
  output$dataSetUI<-renderUI({
    if(length(nameChoices())>0){
      butts<- nameChoices()
      radioGroupButtons(inputId=ns("name"), choices=butts, selected=name(),
                        justified=TRUE)
    }
  })
  
  output$columnUI<-renderUI({
    if( getTibEditState()==TRUE ){
      if(!is.null(getColumnName()) && !is.null(getColumnNameChoices())){ 
        radioGroupButtons(inputId=ns("columnRadio"), 
                          choices=as.list(getColumnNameChoices()), 
                          selected=getColumnName() ,
                          justified=TRUE)
      }
    } 
  })

  output$widgetChooserUI<-renderUI({ #widgetChoice
    if( getTibEditState()==TRUE ){
      #choices=c('radio', 'select')
  ### TODO!!! get column entries, then colEntryType, from which we get handlerChoices
  ### if handler has a choice, use it ow. default to ...
  ### 
      choices<-getWidgetChoices()
      widget<-getWidget()
  # choices come from 
      if( !is.null(choices ) && !is.null(widget)){
        div( "class"='ptR2',
           selectInput(ns("selectedWidget"), label=NULL,
                       choices=choices, selected=widget, width=110)
        )  
      }
    }
  })
  
  output$colEntryUI<-renderUI({
    if( getTibEditState()==TRUE ){
      widget<-getWidget()
      cat('colEntryUI widget=', format(widget),"\n")
      if(!is.null(widget) && !is.null(getTibEntry()) && !is.null(getTibEntryChoices())){ 
            #handlerValue<-getHandlerValue()
            
            cat("output$colEntryUI: widget=",format(widget),"\n")
            selected<-getTibEntry()
            cat("output$colEntryUI:selected=",format(selected),"\n")
            choices<-unique(unlist(getTibEntryChoices()))
            cat('output$colEntryUI:choices=',format(choices),"\n")
            if(widget=='radio'){
              radioGroupButtons(inputId=ns("columnEntryRadio"), 
                          choices=choices, 
                          selected=selected,
                          justified=TRUE
              )
            } else if (widget=='picker'){
              div( "class"="ptR2", width='800px',
                selectizeInput(ns("entryValue"), label=NULL,
                             choices=choices, selected=selected, 
                             options = list(create = TRUE, allowEmptyOption=FALSE, maxItems=1)
                )
              )
            } else if(widget=='colourable') {
              colourInput(
                ns("entryColour"), label=NULL, value=selected
              )
            } else if(widget=='numeric'){
              numericInput(
                ns('entryNumeric'), label = NULL, min=1, max = 100, value = as.numeric(selected)
              )
            } else if(widget=='slider'){
              sliderInput(
                inputId=ns("entrySlider"),label = NULL, min=1,max = 100, value = as.numeric(selected)
              )
            }
            
      }
    } 
  })
  
  output$matColIndexUI<-renderUI({
    selected<-getTibEntry() %AND% getMatColMax() %AND% getMatColIndex() 
    if(!is.null(selected) && getTibEntry()=='point'){
      matColIndex=getMatColIndex() 
      matColMax=getMatColMax()
      matColMin=ifelse(matColMax==0, 0, 1)
      numericInput(ns("matColIndex"), label="Mat Col", value= matColIndex,
                   min=matColMin, max=matColMax, step=1,
                   width= '80px'
      )
    }
  })
  
  # output$editEntryUI<-renderUI({
  #   handlerValue<-getHandlerValue()
  #   widget<-getWidget()
  #   selected<-getTibEntry()
  #   choices<-getTibEntryChoices()
  #   if(!is.null(handlerValue) && handlerValue=='colourable'){
  #      colourInput(
  #       ns("entryColour"), label='Choose Colour', value=selected
  #     )
  #   } else {
  #     selectizeInput(ns("entryValue"), label='Value',
  #          choices=choices, selected=selected, width="200px",
  #          options = list(create = TRUE, allowEmptyOption=FALSE, maxItems=1)
  #     )
  #   }
  # })
  

  
  #---name---
  observeEvent(c( name(), nameChoices() ), { #update the name 
    if(length(nameChoices())==0){ #name choices
    } else {
      updateRadioGroupButtons(session, inputId=ns("name" ),
        choices=nameChoices(), selected=name()
      )
    }
  }) 
  
  #---update getRowIndex selection
  # observeEvent( c( getRowIndex(), getTibNRow() ),  { #update index
  #   if( 
  #     getTibEditState()==TRUE && 
  #     !is.null(getRowIndex())    && 
  #     !is.null(getTibNRow())  &&
  #     getTibNRow()>0 
  #   ){
  #     updateNumericInput(
  #       session,
  #       "rowIndex",
  #       min=1,
  #       max=getTibNRow(),
  #       value=getRowIndex()
  #     )
  #   }
  # }) 
  

  #---column Name change, update ColumnName choice---
  # observeEvent( c(getColumnName(), getColumnNameChoices()) , {
  #   if( getTibEditState()==TRUE ){
  #     if(!is.null(getColumnName()) && !is.null(getColumnNameChoices())){ 
  #       updateSelectizeInput(session, "columnName", 
  #                            choices=as.list(getColumnNameChoices()), 
  #                            selected=getColumnName() )
  #     }
  #   } 
  # })
  
  observeEvent(getTransformType(), {
    if(!is.null(input$name) && input$name==transformTag){
      updateTabsetPanel(session, input$transformType, selected=getTransformType() )
    }
  }, ignoreNULL = TRUE)
  
  #---the next pair of observers are used to return for the entry value---
  observeEvent( input$columnEntryRadio ,{
    val<- input$columnEntryRadio
    if(!is.null(val) && nchar(val)>0){
        entry$result<-val
    }
  })
  
  observeEvent( input$entryValue, {
    val<-input$entryValue
    if(!is.null(val) && nchar(val)>0){
      entry$result<-val
    }
  })
  
  observeEvent( input$entrySlider ,{
    val<-input$entrySlider
    if(!is.null(val) && nchar(val)>0){
      entry$result<-val
    }
  })
  
  observeEvent( input$entryNumeric ,{
    val<-input$entryNumeric
    if(!is.null(val) && nchar(val)>0){
      entry$result<-val
    }
  })
  
  observeEvent( input$entryColour ,{
    val<-input$entryColour
    if(!is.null(val) && nchar(val)>0){
      entry$result<-val
    }
  })
  
  observeEvent(input$selectedWidget,{
    cat("input$selectedWidget cnanged...................\n")
  })
  
  #when name, index, attrName valid, and attrVal changes, update the ptDefs and code
  list( 
    name          = reactive({input$name}),
    rowIndex      = reactive({input$rowIndex}),
    columnName    = reactive({input$columnRadio}),
    entryValue    = reactive(entry$result), 
    selectedWidget  = reactive(input$selectedWidget),
    matColIndex   = reactive(input$matColIndex),
    transformType = reactive({input$transformType})
  )
}
