

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
      conditionalPanel( condition = sprintf("input['%s'] != '%s' && input['%s'] != '%s' ", ns("name"), transformTag, ns("name"), logTag),
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
      )
    ),
    #end footer panel
    #begin headerPanel
    absolutePanel( id=ns('header'),
        top=50, left=0, width="100%", "class"="headerPanel", draggable=FALSE, height="80px"
    ),
        #bottom=0, #height="200px",
        #div(style="display:inline-block",
        # display always
        absolutePanel( top=50, left=15 ,style="display:inline-block",
          selectizeInput( ns("name"), "Data", choices=list(),  selected=NULL, 
            width= '120px' 
          )
        ),
        #display if selected is transform
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
        # display only if input name is a tibble
        conditionalPanel( condition = sprintf("input['%s'] != '%s' && input['%s'] != '%s' ", ns("name"), transformTag, ns("name"), logTag),
          absolutePanel( top=50, left=145 ,
            numericInput( ns("rowIndex"), "Row", 1, min=1, max=10, step=1, 
               width= '70px' 
            )
          ), 
          absolutePanel( top=50, left=220 ,
            selectizeInput(ns("columnName"), label="Column",
              choices=list(),  selected=NULL, 
              width= '120px' 
            )
          ),
          absolutePanel( top=50, left=345 ,
            selectizeInput(ns("entryValue"), "Value", 
              options = list(create = TRUE),
              choices=list(),  selected=NULL, 
              width= '200px' 
            )
          ), 
          # display if state is  point
          absolutePanel(top=50, left=550,
            conditionalPanel( condition = sprintf("input['%s'] == 'point'", ns("entryValue")),
              numericInput(ns("matColIndex"), label="Mat Col", value=0,
                           min=0, max=0, step=1,
                           width= '80px' 
              )
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
  tibEditState,
  headerId
){
  ns<-NS(id2)
  result <- reactiveValues(
    ptDefs=NULL
  ) # note we add name post mortem!!!
  
  
  #updateEntry is called by observer of c(getTibEntry(), getTibEntryChoices())
  updateEntry<-function(){
    #cat("\nentering----moduleEdTib----------updateEntry-------------------\n")
    entry=getTibEntry()
    if(is.null(entry)){return(NULL)}
    if(is(entry,'matrix')){
      #cat("----------updateEntry::matrix-------------------\n")
      #entry='matrix'
      #cat('---------attn: class(entry)=',class(entry),"\n")
      entryChoices=c('point','matrix')
      lastEntry<-input$entryValue 
      if(length(input$entryValue)==0 || !(input$entryValue %in% entryChoices ) ){
        #"cat updating entryValue"
        updateSelectizeInput(session, "entryValue", 
                          choices=entryChoices, 
                          selected='point' )
      }
      if( !is.null(input$entryValue)=='point' && 
         ( is.null(lastEntry) || lastEntry !='point' )){
        matColIndex=ncol(entry)
        if(matColIndex>0){
          minChoice=1
        } else {
          minChoice=0
        }
        updateNumericInput(session, "matColIndex", 
                           min=minChoice,
                           max=matColIndex,
                           value=matColIndex
        ) 
      }
    } else {
      #cat("----------updateEntry::else-------------------\n")
      entryChoices=getTibEntryChoices()
      if(length(entry)>0 && length(entryChoices)>0 ){
        updateSelectizeInput(session, "entryValue", 
                        choices=entryChoices, 
                        selected=entry )
      }
    }
    # cat("\nexiting----moduleEdTib----------updateEntry-------------------\n")
  }
  

  
  observeEvent( c(matColIndex(), matColIndexChoices()),{
    if(!is.null(input$entryValue) && input$entryValue=='point' &&
       !is.null(matColIndex() && !is.null(matColIndexChoices() ))){
          mcChoices<-matColIndexChoices()
          print(mcChoices)
          updateNumericInput(session, "matColIndex", 
                             min=min(mcChoices),
                             max=max(mcChoices),
                             value=matColIndex()
          ) 
    }
  })
   
  
  #ToDo!!! 
  observeEvent(c( name(), nameChoices() ), { #update the name 
    #if(identical( barName(), 'tibEditor')){
      #cat('\n-----Entering----barName initialization for tibEditor (XX) \n')
      if(length(nameChoices())==0){ #name choices
      } else {
        updateSelectizeInput(session, "name", 
                          choices=nameChoices(), 
                          selected=name())
        
      #cat('\n-----Exiting----oE 1-123\n')
      #cat('\n***********************\n-----Leaving----barName initialization for tibEditor \n')
      }
    #}
  }) 
  
  observeEvent( c( rowIndex(), rowIndexChoices() ),  { #update index
    if( tibEditState()==TRUE && 
       !is.null(rowIndex()) && !is.null(rowIndexChoices() )){
      updateNumericInput(
        session, 
        "rowIndex", 
        min=min(rowIndexChoices()),
        max=max(rowIndexChoices()),
        value=rowIndex()
      )
    } 
  }) 
  
  
  
  observeEvent( c(columnName(), columnNameChoices()) , {
     #cat('\n-----Entering----update columnName \n')
    # cat("\n---------entering------oE1-125.1\n")
    if( tibEditState()==TRUE ){

      if(!is.null(columnName()) && !is.null(columnNameChoices())){ 
        updateSelectizeInput(session, "columnName", 
                             choices=as.list(columnNameChoices()), 
                             selected=columnName() )
      }

    } 
  })
  
  observeEvent( c(getTibEntry(), getTibEntryChoices()) , { 
    if( tibEditState()==TRUE ){
      updateEntry()
    } 
  })
  

  
  #when name, index, attrName valid, and attrVal changes, update the ptDefs and code
  list( 
    name         =reactive({input$name}),
    rowIndex     = reactive({input$rowIndex}),
    columnName    =reactive({input$columnName}),
    entryValue   =reactive(input$entryValue),
    tagClone    =reactive({input$tagClone}),
    tagDelete   =reactive({input$tagDelete}),
    tagMoveUp   =reactive({input$tagMoveUp}),
    tagMoveDown   =reactive({input$tagMoveDown}),
    matColIndex   = reactive(input$matColIndex),
    backwardPt   = reactive(input$backwardPt),
    forwardPt    = reactive(input$forwardPt),
    removePt     =reactive({input$removePt}),
    tagPt        =reactive({input$tagPt}),
    transformType =reactive({input$transformType})
  )
}
