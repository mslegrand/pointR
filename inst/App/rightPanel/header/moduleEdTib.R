
moduleEdTibUI<-function(id, input, output) { 
  ns <- NS(id)
  tagList(
    #begin-----------headerPanel
    # ---header backdrop
    absolutePanel( id=ns('header'), 
        top=50, left=0, width="100%", "class"="headerPanel", draggable=FALSE, height="80px"
    ),
    # ---name input ---
    #(always visible) 
    absolutePanel( top=50, left=15 ,style="display:inline-block",
      selectizeInput( ns("name"), "Data", choices=list(),  selected=NULL, 
        width= '120px' 
      )
    ),
    
    #---transform content---
    #   display only if selected name is transform
    conditionalPanel( condition = sprintf("input['%s'] == '%s'", ns("name"), transformTag),
      absolutePanel( 
        top=50+25, left=145, width="100%", 
        "class"="headerPanel", draggable=FALSE, "background-color"='#666688',
        tabsetPanel( id=ns("transformType"),  
                     tabPanel("Translate"), 
                     tabPanel("Rotate"), 
                     tabPanel("Scale"),
                     type="pills"
        ) 
      )    
    ),
    
    #---tibble content---
    # display only if input name is a tibble
    conditionalPanel( condition = sprintf("input['%s'] != '%s' && input['%s'] != '%s' ", ns("name"), transformTag, ns("name"), logTag),
          #---row---
          absolutePanel( top=50, left=145 ,
            numericInput( ns("rowIndex"), "Row", 1, min=1, max=10, step=1, 
               width= '70px' 
            )
          ), 
          #---column---
          absolutePanel( top=50, left=220 ,
            selectizeInput(ns("columnName"), label="Column",
              choices=list(),  selected=NULL, 
              width= '120px' 
            )
          ),
          
          # ---Entry Input Handlers ---
          absolutePanel( top=50, left=345, uiOutput(ns("editEntryUI"))),
          
          
          # ---point column----
          # display if state is  point
          absolutePanel(top=50, left=550, uiOutput(ns("matColIndexUI"))
          #   conditionalPanel( condition = sprintf("output[%s] == 'point'", ns("selected")),
          #     numericInput(ns("matColIndex"), label="Mat Col", value=0,
          #                  min=0, max=0, step=1,
          #                  width= '80px'
          #     )
          #   )
          # )
        )
        # ,
        # absolutePanel(top=0, left=530, 
        #   sliderInput(ns("decimal"), "Decimal:",
        #          min = 0, max = 1, width='200px', 
        #         value = 0.5, step = 0.1)
        # )
      ) # end header panel
  ) # end taglist
}

moduleEdTib<-function(input, output, session, 
  id2,
  name, 
  nameChoices,
  rowIndex,
  rowIndexChoices,
  getTibNRow,
  matColIndex,
  matColIndexChoices, 
  getColumnName,
  getColumnNameChoices,
  getTibEntry,
  getTibEntryChoices, 
  getTibEditState,
  getHandler,
  getHandlerValue
  
){
  #ns<-NS(id2) #can get this from session!!!!!
  ns <- session$ns
  # result <- reactiveValues(
  #   #ptDefs=NULL,
  #   
  #   entryValue=NULL # used to return an updated entryValue
  #   #colInput='default'
  # ) # note we add name post mortem!!!
  # 
  
  entry<-reactiveValues(
    value=list(selected=NULL, choices=NULL, matColIndex=NULL, matColMax=NULL),
    result=NULL
  )
  
  getValue<-reactive({
    entry$value
  })
  
   # output$selected<-reactive(entry$value$selected)
   # outputOptions(output, "selected", suspendWhenHidden=FALSE)
   # 
  
  output$matColIndexUI<-renderUI({
    selected<-entry$value$matColMax %AND% 
              entry$value$matColMax %AND%  
              entry$value$selected
    if(!is.null(selected) && selected=='point'){
      matColIndex=entry$value$matColIndex
      matColMax=entry$value$matColMax

      matColMin=ifelse(matColMax==0, 0, 1)
      
      numericInput(ns("matColIndex"), label="Mat Col", value= matColIndex,
                                   min=matColMin, max=matColMax, step=1,
                                   width= '80px'
                   )
    }
  })
  
  output$editEntryUI<-renderUI({
    cat("ModuleEdTib::...Entering--------------output$editEntryUI-------------------\n")
    handlerValue<-getHandlerValue()
    if(is.null(handlerValue)){
      cat('ModuleEdTib::...handlerValue is NULL\n')
    } else {
      cat('ModuleEdTib::...handlerValue=',handlerValue,"\n")
    }
    val<-getValue()
    selected<-val$selected
    choices<- val$choices
    cat("\n****************selectize*****************************************************\n")
    cat("ModuleEdTib::...selected=",selected,"\n")
    cat("ModuleEdTib::...class(selected)=",class(selected),"\n")
    cat("ModuleEdTib::...choices=",paste(choices,collapse=", ", sep=": "),"\n")
    cat("ModuleEdTib::rowIndex()=",rowIndex(),"\n")
    cat("ModuleEdTib::...class(choices)=",class(choices),"\n")
    cat("\n*********************************************************************\n")
    if(!is.null(handlerValue) && handlerValue=='colourable'){
      colourInput(
        ns("entryColour"), label='Choose Colour', value=selected
      )
    } else {
      selectizeInput(ns("entryValue"), label='Value',
                     choices=choices, selected=selected, width="200px",
                     options = list(create = TRUE, allowEmptyOption=FALSE, maxItems=1)
                     )
      # selectInput(ns("entryValue"), label='Value',
      #               choices=choices, selected=rowIndex(), width="200px"
      #               #$options = list(create = TRUE, allowEmptyOption=FALSE, maxItems=1
      # 
      # )
    }
  })
  
  
  
  #updateEntry is called by observer of c(getTibEntry(), getTibEntryChoices())
  # computes and assigns values for:
  #   - entryValue
  #   - entryChoices
  #   - matColIndex, matColChoices
  updateEntry<-function(){
    cat("ModuleEdTib::...entering--------------updateEntry-------------------\n")
    tibEntry=getTibEntry()
    entryValue<-entry$value
    if(is.null(tibEntry)){
      cat("ModuleEdTib::...Quick Exit of updateEntry------------------tibEntry is NULL\n")
      return(NULL)
    }
    # if(is(tibEntry,'matrix')){ 
    #   cat("ModuleEdTib::...enter:ismatrix----------updateEntry::isMatrix-------------------\n")
    #   entryValue$choices=c('point','matrix')
    #   lastEntry<-input$entryValue 
    #   #lastEntry<-entryValue$selected
    #   if(is.null(lastEntry)){ lastEntry<-'NULL'}
    #   if( !(lastEntry %in% entryValue$choices ) ){ 
    #     entryValue$selected='point' 
    #   }
    #   if( entryValue$selected=='point' &&  entryValue$selected!=lastEntry){
    #     entryValue$matColIndex=ncol(entry)
    #     entryValue$matColMax=ncol(entry)
    #   }
    #   
    # } else {
      cat("ModuleEdTib::...enter----------updateEntry::else-------------------\n")
      entryChoices=getTibEntryChoices()
      entryValue<-list(selected=tibEntry, choices=as.list(entryChoices), matColIndex=NULL, matColMax=NULL)
    #}
    entry$value<-entryValue
    #entry$result<-entryValue$selected
    cat("Exiting----moduleEdTib----------updateEntry-------------------\n")
  }
  
  #ToDo!!! 
  #---name---
  observeEvent(c( name(), nameChoices() ), { #update the name 
    #if(identical( barName(), 'tibEditor')){
      #cat('\n-----Entering----barName initialization for tibEditor (XX) \n')
      if(length(nameChoices())==0){ #name choices
      } else {
        updateSelectizeInput(session, "name", 
                          choices=nameChoices(), 
                          selected=name())
      #cat('\n***********************\n-----Leaving----barName initialization for tibEditor \n')
      }
    #}
  }) 
  
  #---row---
  observeEvent( c( rowIndex(), getTibNRow() ),  { #update index
    if( 
      getTibEditState()==TRUE && 
      !is.null(rowIndex())    && 
      !is.null(getTibNRow())  &&
      getTibNRow()>0 
    ){
      updateNumericInput(
        session, 
        "rowIndex", 
        min=1,
        max=getTibNRow(),
        value=rowIndex()
      )
    } 
  }) 
  
  #--row--- change
  
  observeEvent( c( rowIndex(), getTibNRow() ),  { #update entry 
    if( 
      getTibEditState()==TRUE && 
      !is.null(rowIndex())    && 
      !is.null(getTibNRow())  &&
               getTibNRow()>0 
    ){
      handlerValue<-getHandlerValue()
      if(!is.null(handlerValue) && handlerValue=='colourable'){
        cat('')
      } else {
        val<-getValue()
        selected<-val$selected
        choices<- val$choices
        updateSelectizeInput(session, "entryValue",  choices=choices,  selected=selected)
      } 
    }
  })   
  
  
  
  
  #---column---
  observeEvent( c(getColumnName(), getColumnNameChoices()) , {
    if( getTibEditState()==TRUE ){
      cat('ModuleEdTib::...-----Entering---- observeEvent( c(getColumnName(), getColumnNameChoices()) \n')
      cat('ModuleEdTib::...with getColumnName=', getColumnName(), "\n")
      if(!is.null(getColumnName()) && !is.null(getColumnNameChoices())){ 
        updateSelectizeInput(session, "columnName", 
                             choices=as.list(getColumnNameChoices()), 
                             selected=getColumnName() )
      }
      cat('ModuleEdTib::...-----Exiting---- observeEvent( c(getColumnName(), getColumnNameChoices()) \n')
    } 
  })
  
  #---entry---
  observeEvent( c(getColumnName(), getTibEntry(), getTibEntryChoices()) , { 
    if( getTibEditState()==TRUE ){
      cat('ModuleEdTib::...-----Entering---- observeEvent( c(getColumnName(), getTibEntry(), getTibEntryChoices()) \n')
      updateEntry()
      cat('ModuleEdTib::...-----Exiting---- observeEvent( c(getColumnName(), getTibEntry(), getTibEntryChoices()) \n')
    } 
  })
  
  observeEvent( getHandlerValue() ,{
    if( getTibEditState()==TRUE ){
      val<-getHandlerValue()
      if(!is.null(val) && val=='colourable'){
        cat('ModuleEdTib::... Entering getHandlerValue()\n')
        cat('ModuleEdTib::...getHandlerValue()= ', val,"\n")
        cat('ModuleEdTib::...getHandlerValue() calling updateEntry()\n')
        cat('ModuleEdTib::... Exiting getHandlerValue()\n')
        updateEntry()
      }
    }
  })
  
  
  #---matColIndex
  observeEvent( c( matColIndex(), matColIndexChoices(), getValue() ),{
    val=getValue()
    cat("\nModuleEdTib::... Enter ==============c( matColIndex(), matColIndexChoices(), getValue() )================\n")
    #browser()
    cat("val$selected=",val$selected,"\n")
    if( 
        !is.null(val$selected )    && 
        (val$selected=='point')    &&
        !is.null(matColIndex() )   && 
        !is.null(matColIndexChoices())
       )
     {
      mcChoices<-matColIndexChoices()
      # cat('mcChoices=\n')
      # print(mcChoices)
      updateNumericInput(session, "matColIndex", 
                         min=min(mcChoices),
                         max=max(mcChoices),
                         value=matColIndex()
      ) 
    } else {
      cat('ok\n')
    }
    cat("ModuleEdTib::... Exiting ==============c( matColIndex(), matColIndexChoices(), getValue() )================\n")
    
  })
  
                


  #---the next pair of observers are used to return for the entry value---
  observeEvent( input$entryValue ,{
    val<- input$entryValue
    if(!is.null(val) && nchar(val)>0){
        cat("ModuleEdTib::...---------- entering input$entryValue--------\n")
        cat("ModuleEdTib::...----------entryValue=<",val,">\n")
        print(val)
        entry$result<-val
        cat("\nModuleEdTib::...----------returning entry$result=", entry$result,"\n")
        cat("ModuleEdTib::...---------- exiting input$entryValue--------\n")
    }
  })
  
  observeEvent( input$entryColour ,{
    val<-input$entryColour
    if(!is.null(val) && nchar(val)>0){
      cat("ModuleEdTib::...---------- entering input$entryColour--------\n")
      cat("ModuleEdTib::...----------entryValue=<",val,">\n")
      entry$result<-val
      cat("\nModuleEdTib::...----------returning entry$result=", entry$result,"\n")
      cat("ModuleEdTib::...---------- exiting input$entryColour--------\n")
    }
  })
  # observeEvent( getValue(),{
  #   val<-getValue()$selected
  #   if(!is.null(val ) && nchar(val)){
  #     cat("\nModuleEdTib::...---------- entering getValue()$selected",val,">\n")
  #     entry$result<-val
  #     cat("\nModuleEdTib::...----------returning entry$result=", entry$result,"\n")
  #     cat("\nModuleEdTib::...---------- exiting getValue()$selected",val,">\n")
  #   }
  # })
  
  
  #when name, index, attrName valid, and attrVal changes, update the ptDefs and code
  list( 
    name          = reactive({input$name}),
    rowIndex      = reactive({input$rowIndex}),
    columnName    = reactive({input$columnName}),
    entryValue    = reactive(entry$result), 
    matColIndex   = reactive(input$matColIndex),
    transformType = reactive({input$transformType})
  )
}
