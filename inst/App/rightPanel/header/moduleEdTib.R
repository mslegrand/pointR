
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
                  actionButton(ns("newAssetsButton"), span(class='icon-plus'," Assets"), width=wid0)
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
                      actionButton(ns("newColumnButton"), span(class='icon-plus'," Columns"), width=wid0)
        ),
        #---chooser
        absolutePanel( top=top1, left=left,  right=10,
          uiOutput(ns("columnUI"))
        ),
        #---add entry button---
        # absolutePanel(top= top2, left=left0,
        #               actionButton(ns("newColumnEntryButton"), span(class='icon-plus'," Entries"), width=wid0)
        # ),
    absolutePanel(top= top2, left=left0, 
        div( "class"='ptR2',
              # tags$style(type='text/css',  
              #     ".selectize-input,  
              #     .selectize-input input,
              #     .selectize-control.single, 
              #     .selectize-input.input-active, 
              #     .selectize-input.full    
              #     { font-size: 12px; line-height: 8px;  background-color: #222244; color: #00ffff; padding:0; width: 110px;}
              #         .selectize-dropdown { font-size: 12px; line-height: 12px; background-color: #44aaaa; }
              #             .selectize-input.full {
              #               background: url(http://i62.tinypic.com/15xvbd5.png) no-repeat 96% 0;
              #               height: 20px;
              #               width: 110px;
              #               overflow: hidden;
              #             }
              #           "),
                  selectInput(ns("columnEntryType"), label=NULL,
                              choices=c('radio', 'select'), width=110)
        )
                              #span(class='icon-plus'," Entries"), width=wid0)
    )),
      
        # absolutePanel(top= top2, left=left0,
        #               actionButton(ns("newColumnEntryButton"), span(class='icon-plus'," Entries"), width=wid0)
        # ),
      #---columnEntries
        #---entry chooser
        absolutePanel( top=top2, left=left,  right=10,
                       uiOutput(ns("colEntryUI"))
        ),
   #),
    
    # ---name input --- #(always visible) 
    # absolutePanel( top=top, left=15 ,style="display:inline-block",
    #   selectizeInput( ns("name"), "Data", choices=list(),  selected=NULL, 
    #     width= '120px' 
    #   )
    # ),
    
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
  getHandler,
  getHandlerValue
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

  output$colEntryUI<-renderUI({
    if( getTibEditState()==TRUE ){
      
      if(!is.null(getTibEntry()) && !is.null(getTibEntryChoices())){ handlerValue<-getHandlerValue()
            selected<-getTibEntry()
            choices<-unique(unlist(getTibEntryChoices()))
            #print(choices)
            # if( length(choices)!=2 || !all.equal(choices, c('point', 'matrix')) ){
            #   choices<-c("New Entry",choices)
            # }
            
            if(input$columnEntryType=='radio'){
              radioGroupButtons(inputId=ns("columnEntryRadio"), 
                          choices=as.list(choices), 
                          selected=getTibEntry() ,
                          justified=TRUE
              )
            } else if (input$columnEntryType=='select'){
              # textInputAddon(inputId = ns('columnEntryText'), label = NULL, 
              #                placeholder = selected, addon = span(class="icon-edit"))
              div( "class"="ptR2", width='800px',
                selectizeInput(ns("entryValue"), label=NULL,
                             choices=choices, selected=selected, 
                             options = list(create = TRUE, allowEmptyOption=FALSE, maxItems=1)
              )
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
  
  observeEvent(input$newColumnEntryButton,{
    #jqui_draggable(selector= paste0('#', ns('newChoiceXX')))
    # toggle output$colEntryUI edit state to output editbox
    entry$picker='textBox'
    #todo:
    #1. set focus to newChoiceXX
    #cat(ns("newColumnEntryButton"),"\n")
    #hide(ns("newAssetsButton"))
    session$sendCustomMessage(
      type = "ptRManager",
      list(setFocus=ns('columnEntryText'), hide=ns("newColumnEntryButton") )
      #list(setFocus=ns('colEntryUI') )
    )
    #2. add ok/cancel buttons or on blur/ cancelbutton to set
    #3. set width?
  })
  
  output$editEntryUI<-renderUI({
    handlerValue<-getHandlerValue()
    selected<-getTibEntry()
    choices<-getTibEntryChoices()
    if(!is.null(handlerValue) && handlerValue=='colourable'){
       colourInput(
        ns("entryColour"), label='Choose Colour', value=selected
      )
    } else {
      selectizeInput(ns("entryValue"), label='Value',
           choices=choices, selected=selected, width="200px",
           options = list(create = TRUE, allowEmptyOption=FALSE, maxItems=1)
      )
    }
  })
  

  
  #---name---
  observeEvent(c( name(), nameChoices() ), { #update the name 
    if(length(nameChoices())==0){ #name choices
    } else {
      # updateSelectizeInput(session, "name", 
      #                   choices=nameChoices(), 
      #                   selected=name())
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
        cat("ModuleEdTib::...---------- entering input$columnEntryRadio--------\n")
        cat("ModuleEdTib::...----------val=<",val,">\n")
        print(val)
        entry$result<-val
        # cat("\nModuleEdTib::...----------returning entry$result=", entry$result,"\n")
        # cat("ModuleEdTib::...---------- exiting input$entryValue--------\n")
    }
  })
  
  observeEvent( input$entryValue, {
    val<-input$entryValue
    if(!is.null(val) && nchar(val)>0){
      # cat("ModuleEdTib::...---------- entering input$columnEntryText--------\n")
      # cat("ModuleEdTib::...----------entryValue=<",val,">\n")
      entry$result<-val
      # cat("\nModuleEdTib::...----------returning entry$result=", entry$result,"\n")
      # cat("ModuleEdTib::...---------- exiting input$columnEntryText--------\n")
    }
  })
  
  observeEvent( input$entryColour ,{
    val<-input$entryColour
    if(!is.null(val) && nchar(val)>0){
      # cat("ModuleEdTib::...---------- entering input$entryColour--------\n")
      # cat("ModuleEdTib::...----------entryValue=<",val,">\n")
      entry$result<-val
      # cat("\nModuleEdTib::...----------returning entry$result=", entry$result,"\n")
      # cat("ModuleEdTib::...---------- exiting input$entryColour--------\n")
    }
  })
  
  #when name, index, attrName valid, and attrVal changes, update the ptDefs and code
  list( 
    name          = reactive({input$name}),
    rowIndex      = reactive({input$rowIndex}),
    columnName    = reactive({input$columnRadio}),
    entryValue    = reactive(entry$result), 
    matColIndex   = reactive(input$matColIndex),
    transformType = reactive({input$transformType})
  )
}
