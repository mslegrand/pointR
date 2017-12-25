

#currently update triggers value from choice to ace
# propose
# tagValHeadWidth<-c(100,70,120,200)
# tagValHeadLeft<-cumsum(c(10,tagValHeadWidth)+5)
# tagValHeadWidth<-paste0(tagValHeadWidth,"px")
# tagValHeadLeft<-paste0(tagValHeadLeft,"px")

moduleEdTibUI<-function(id, input, output) { 
  ns <- NS(id)
  #useShinyjs()
  tagList(# beginfooter panel
    absolutePanel( "class"="footerPanel", draggable=FALSE,
        conditionalPanel(condition = sprintf("input['%s'] == 'matrix'", ns("entryValue")),
           actionButton(ns("tagClone"),   label = "Clone"   ),
           actionButton(ns("tagDelete"),  label = "Delete"),
           actionButton(ns("tagMoveUp"), label = "Send Up"),
           actionButton(ns("tagMoveDown"), label = "Send  Down")
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'point'", ns("entryValue")),
          actionButton(ns("forwardPt" ), label = "Forward Pt"),
          actionButton(ns("backwardPt"), label = "Backward Pt"),
          actionButton(ns("removePt"), label = "Delete Pt"),
          actionButton(ns("tagPt"), label = "Tag Pt") 
        )
    ),
    #end footer panel
    #begin headerPanel
    absolutePanel( id='header',
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
        ),
        absolutePanel(top=50, left=530,
          conditionalPanel( condition = sprintf("input['%s'] == 'point'", ns("entryValue")),
            numericInput(ns("matColIndex"), label="Mat Col", value=0,
                         min=0, max=0, step=1,
                         width= '80px' 
            )
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

moduleEdTib<-function(input, output, session, 
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
  getTibEntryChoices, 
  headerId
){
  ns<-NS(id2)
  result <- reactiveValues(
    ptDefs=NULL
  ) # note we add name post mortem!!!
  
  updateEntry<-function(){
    cat("\nentering----moduleEdTib----------updateEntry-------------------\n")
    entry=getTibEntry()
    if(is(entry,'matrix')){
      cat("----------updateEntry::matrix-------------------\n")
      entry='matrix'
      cat('---------attn: class(entry)=',class(entry),"\n")
      entryChoices=c('point','matrix')
      if(length(input$entryValue)==0 || !(input$entryValue %in% entryChoices ) ){
        "cat updating entryValue"
        updateSelectizeInput(session, "entryValue", 
                          choices=entryChoices, 
                          selected=entry )
      }
    } else {
      cat("----------updateEntry::else-------------------\n")
      entryChoices=getTibEntryChoices()
      if(length(entry)>0 && length(entryChoices)>0 ){
        #cat("updateEntry::entry=\n")
        #print(entry)
        cat("updateEntry::entryChoices=\n")
        print(entryChoices)
        updateSelectizeInput(session, "entryValue", 
                        choices=entryChoices, 
                        selected=entry )
      }
    }
    cat("\nexiting----moduleEdTib----------updateEntry-------------------\n")
  }
  
  # updateMatCol<-function(){
  #   # if(!is.null(input$entryValue) && input$entryValue=='point'){
  #   #   cat('\n----Entering moduleEdTib: updateMatCol\n')
  #   #   mcChoices<-matColIndexChoices()
  #   #   if(!is.null(mcChoices) ){
  #   #     mcChoices<-matColIndex()
  #   #   }
  #   #   updateNumericInput(session, "matColIndex", 
  #   #                      min=min(mcChoices),
  #   #                      max=max(mcChoices),
  #   #                      value=matColIndex()
  #   #   ) 
  #   #   cat('----Leaving moduleEdTib: updateMatCol\n')
  #   # }
  # }
  
  observeEvent( c(matColIndex(), matColIndexChoices()),{
    # if(!is.null(input$entryValue) && input$entryValue=='point' ){
    #   cat('\\\\\\\\\\\\matColIndex:\n')
    #   print(matColIndex())
    #   cat('matColIndexChoices()=\n')
    #   print(matColIndexChoices())
    # }
    if(!is.null(input$entryValue) && input$entryValue=='point' &&
       !is.null(matColIndex() && !is.null(matColIndexChoices() ))){
          mcChoices<-matColIndexChoices()
          updateNumericInput(session, "matColIndex", 
                             min=min(mcChoices),
                             max=max(mcChoices),
                             value=matColIndex()
          ) 
    }
  })
  
  # updateColumnName<-function(){
  #   #if(!is.null(columnName()) && ( is.null( input$columnName ) || !(input$columnName %in% columnNameChoices()))){
  #   
  #     if(!is.null(columnName()) && !is.null(columnNameChoices())){ 
  #       cat('\n----Entering moduleEdTib: updateColumnName\n')
  #       cat('moduleEdTib: updateColumnName:: columnName=',columnName(),"\n")
  #       cat()
  #       updateSelectizeInput(session, "columnName", 
  #                         choices=as.list(columnNameChoices()), 
  #                         selected=columnName() )
  #       cat('----Leaving moduleEdTib: updateColumnName\n')
  #   }
  # }
  
  #ToDo!!! 
  observeEvent(c(barName(),name(), nameChoices() ), { #update the name 
    if(identical( barName(), 'tibEditor')){
      cat('\n-----Entering----barName initialization for tibEditor (XX) \n')
      if(length(nameChoices())==0){ #name choices
        cat("\n------------------hiding header")
        #hideElement( headerId ) 
      } else {
        #showElement( headerId)
        cat("\n\n*****************************************************\n")
        if(is.null(name() )){
          cat('name is null\n')
        } else {
          cat('name=',name(), "\n\n")
        }
        if(is.null(nameChoices())){
          cat('nameChoices is null')
        } else {
          cat('nameChoices=',paste(nameChoices(), collapse=", "), "\n\n")
        }
        if(!is.null(name())){
          cat("\n\n\n***************************************")
          cat('\n\n-----------updating name\n')
          cat('name=',name(), "\n\n")
          cat('nameChoices=',paste(nameChoices(), collapse=", "), "\n\n")
        }
        updateSelectizeInput(session, "name", 
                          choices=nameChoices(), 
                          selected=name())
        
      #cat('\n-----Exiting----oE 1-123\n')
      cat('\n***********************\n-----Leaving----barName initialization for tibEditor \n')
      }
    }
  }) 
  
  
  
  observeEvent( c(barName(), rowIndex(), rowIndexChoices() ),  { #update index
    if(identical( barName(), 'tibEditor')){
      cat('\n-----Entering----update rowIndex, rowIndexChoices \n')
      cat('\n---------entering--------oE 1-124\n')
      updateNumericInput(
        session, 
        "rowIndex", 
        min=min(rowIndexChoices()),
        max=max(rowIndexChoices()),
        value=rowIndex()
      )
      #updateEntry()
      #updateMatCol()
      cat('\n---------exiting--------oE 1-124\n')
      cat('-----Exiting----update rowIndex, rowIndexChoices \n')
    } 
  }) 
  
  
  
  observeEvent( c(columnName(), columnNameChoices()) , {
    # cat('\n-----Entering----update columnName \n')
    # cat("\n---------entering------oE1-125.1\n")
    if(identical( barName(), 'tibEditor')){
      cat('oE update 1-125.2\n')
      #updateColumnName()
      
      if(!is.null(columnName()) && !is.null(columnNameChoices())){ 
        cat('\n----Entering moduleEdTib: updateColumnName\n')
        cat('moduleEdTib: updateColumnName:: columnName=',columnName(),"\n")
        
        updateSelectizeInput(session, "columnName", 
                             choices=as.list(columnNameChoices()), 
                             selected=columnName() )
        cat('----Leaving moduleEdTib: updateColumnName\n')
      }
      
      
      # updateSelectInput(session, "columnName", 
      #                   choices=columnNameChoices(), 
      #                   selected=columnName() )
      #updateEntry()
      #updateMatCol()
    } 
    # cat("---------exiting------oE1-125.1\n")
    # cat('----Leavomg----update columnName \n')
  })
  
  observeEvent( c(getTibEntry(), getTibEntryChoices()) , { 
    if(identical( barName(), 'tibEditor')){
      cat('\n-----Entering----update tibEntry \n')
      cat('oE 1-126\n')
      # updateSelectInput(session, "columnName", 
      #                   choices=columnNameChoices(), 
      #                   selected=columnName() )
      #updateColumnName()
      updateEntry()
      
      #updateMatCol()
      cat('----Leaving----update tibEntry \n')
    } 
  })
  

  
  #when name, index, attrName valid, and attrVal changes, update the ptDefs and code
  list( 
    name         =reactive({input$name}),
    rowIndex     = reactive({input$rowIndex}),
    columnName    =reactive({input$columnName}),
    entryValue   =reactive(input$entryValue),
    applyTibEdit = reactive({input$applyTibEdit}),
    tagClone    =reactive({input$tagClone}),
    tagDelete   =reactive({input$tagDelete}),
    tagMoveUp   =reactive({input$tagMoveUp}),
    tagMoveDown   =reactive({input$tagMoveDown}),
    matColIndex   = reactive(input$matColIndex),
    backwardPt   = reactive(input$backwardPt),
    forwardPt    = reactive(input$forwardPt),
    removePt     =reactive({input$removePt}),
    tagPt        =reactive({input$tagPt})
  )
}