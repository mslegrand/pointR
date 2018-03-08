
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
  
    # #---header backdrop
    # absolutePanel( id=ns('header'),
    #      "class"="headerPanel", draggable=FALSE
    # ),
    # #---asset name
    # #-------asset button
    # absolutePanel(top= top0, left=left0,
    #     div( 'class'="ptRBtn2",
    #       actionButton(ns("newAssetsButton"), span(class='icon-plus'," Assets"))
    #     )
    # ),
    # #------asset chooser
    # absolutePanel(top= top0, left=left,  right=10,
    #     uiOutput(ns("dataSetUI"))
    # ),
    
    #---tib 
    div( 
      id=ns('headEdTib'),
      #---tib column
        #---add  button---
        div( 'class'="ptRBtn2 topHeadCol1 topHeadRow1", actionButton(ns("newColumnButton"), span(class='icon-plus'," Variables"))),
        #---tib  chooser
        div( 'class'='ptR2  topHeadCol2   topHeadRow1',  uiOutput(ns("columnUI"))),
      
      #---tib columnEntries
        #------- tib entry widget selection---
        div( 'class'=' topHeadCol1 topHeadRow2 ptR2',  uiOutput(ns("widgetChooserUI")) ),
        #-------tib entry value
        div(  'class'='topHeadCol2 topHeadRow2 ptR2',  uiOutput(ns("columnEntryUI"))  
       )
    ),
    
    #---transform content---#   display only if selected name is transform
    #conditionalPanel( condition = sprintf("input['%s'] == '%s'", ns("name"), transformTag),
      div( id=ns("transformPanelContainer"), #'class'='topHeadCol2 topHeadRow2 ptR2'
        top=top+25, left=left, width="100%",
        "class"="headerPanel", draggable=FALSE, "background-color"='#666688',
        tabsetPanel( id=ns("transformType"),
                     tabPanel("Translate"),
                     tabPanel("Rotate"),
                     tabPanel("Scale"),
                     type="pills"
        )
      )
    #)
  ) # end taglist
  
}

moduleEdTib<-function(input, output, session, 
  name, 
  nameChoices,
  getRowIndex,
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
  
  
  
  
  

#------------ui ouput----------------------
  
  #---assets
  # output$dataSetUI<-renderUI({
  #   if(length(nameChoices())>0){
  #     butts<- nameChoices()
  #     radioGroupButtons(inputId=ns("name"), choices=butts, selected=name(),
  #                       justified=TRUE)
  #   }
  # })
  
  
  
  #---columns
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
  
  #---column values
  output$widgetChooserUI<-renderUI({ #widgetChoice
    if( getTibEditState()==TRUE ){
      choices<-getWidgetChoices()
      widget<-getWidget()
      if( !is.null(choices ) && !is.null(widget)){
        div( "class"='ptR2',
           selectInput(ns("selectedWidget"), label=NULL,
                       choices=choices, selected=widget, width="110px")
        )  
      }
    }
  })
  
  output$columnEntryUI<-renderUI({
    if( getTibEditState()==TRUE ){
      #cat("\nEntering----------output$colEntryUI---------------\n")
      widget<-getWidget()
      if(!is.null(widget) && !is.null(getTibEntry()) && !is.null(getTibEntryChoices())){ 
            selected<-getTibEntry()
            choices<-sort(unique(unlist(getTibEntryChoices())))
            
            if(widget=='radio'){
              radioGroupButtons(inputId=ns("entryRadio"), 
                          choices=choices, 
                          selected=selected,
                          justified=TRUE
              )
            } else if (widget=='picker'){
              div( "class"="ptR2", width='800px',
                selectizeInput(ns("entryValue"), label=NULL,
                             choices=choices, selected=selected, 
                             options = list(create = TRUE, allowEmptyOption=FALSE, maxItems=1, width='200px')
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
            } else if(widget=='knob'){
               div(knobInput(
                 ns('entryKnob'), label = NULL, min=1, max = 100, value = as.numeric(selected), width=100, height=100
               ))
            } 
            # else if( widget=='spectrum'){
            #   spectrumInput(
            #     inputId = ns("entrySpectrum"),
            #     label = NULL,
            #     choices = list(
            #       list('black', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
            #       as.list(brewer.pal(n = 9, name = "Blues")),
            #       as.list(brewer.pal(n = 9, name = "Greens")),
            #       as.list(brewer.pal(n = 11, name = "Spectral")),
            #       as.list(brewer.pal(n = 8, name = "Dark2"))
            #     ),
            #     options = list(`toggle-palette-more-text` = "Show more")
            #   )
            # }
      }
    } 
  })
  
  # output$matColIndexUI<-renderUI({
  #   selected<-getTibEntry() %AND% getMatColMax() %AND% getMatColIndex() 
  #   if(!is.null(selected) && getTibEntry()=='point'){
  #     matColIndex=getMatColIndex() 
  #     matColMax=getMatColMax()
  #     matColMin=ifelse(matColMax==0, 0, 1)
  #     numericInput(ns("matColIndex"), label="Mat Col", value= matColIndex,
  #                  min=matColMin, max=matColMax, step=1,
  #                  width= '80px'
  #     )
  #   }
  # })
  # 

  #---asset name---
  observeEvent(c( name(), nameChoices() ), { #update the name
    toggleElement(
      id='transformPanelContainer' ,
      condition=(!is.null(name())&& name()=='transformTag')
    )
    if(length(nameChoices())==0){ #name choices
    } else {
      # updateRadioGroupButtons(session, inputId=ns("name" ),
      #   choices=nameChoices(), selected=name()
      # )
      toggleElement(
        id='headEdTib' ,
        condition=!(name() %in% c( transformTag, RPanelTag, errorPanelTag, svgPanelTag))
      )
    }
  }) 

  observeEvent( getTransformType(), {
    if(!is.null(name()) && name()==transformTag){
      updateTabsetPanel(session, input$transformType, selected=getTransformType() )
    }
  }, ignoreNULL = TRUE)

  #---the next collection of observers are used to return for the entry value---
  observeEvent( input$entryRadio ,{
    val<- input$entryRadio
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
  
  observeEvent( input$entryKnob ,{
    val<-input$entryKnob
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
  
  observeEvent( input$entrySpectrum ,{
    val<-input$entrySpectrum
    if(!is.null(val) && nchar(val)>0){
      entry$result<-val
    }
  })
  

  #when name, index, attrName valid, and attrVal changes, update the ptDefs and code
  list( 
    #name          = reactive({input$name}),
    columnName    = reactive({input$columnRadio}),
    entryValue    = reactive(entry$result), 
    selectedWidget  = reactive(input$selectedWidget),
    matColIndex   = reactive(input$matColIndex),
    transformType = reactive({input$transformType}),
    newColumn = reactive({input$newColumn})
  )
}
