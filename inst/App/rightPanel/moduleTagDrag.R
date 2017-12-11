moduleTagDragUI<-function(id, input, output) { 
  ns <- NS(id)  
  tagList(
    absolutePanel( "class"="footerPanel", draggable=FALSE,
      actionButton(ns("tagClone"),   label = "Clone"   ),
      actionButton(ns("tagDelete"),  label = "Delete"),
      actionButton(ns("tagMoveUp"), label = "Send Up"),
      actionButton(ns("tagMoveDown"), label = "Send  Down")
    ),
    absolutePanel( 
      top=50, left=0, width="100%", "class"="headerPanel", draggable=FALSE,
      div(style="display:inline-block",
        selectInput( ns("name"), "Point Matrix",
        multiple=FALSE, size=1, selectize = FALSE,
        choice=list(),  selected=NULL, width="100px"  )
      ),
      div(style="display:inline-block",
          numericInput( ns("rowIndex"), "Row", 1, min=1, max=10, step=1, width="80px" )
      ),
      div(style="display:inline-block",
          numericInput( ns("matColIndex"), "Mat Col", 1, min=1, max=10, step=1, width="80px" )
      )
      # ,
      # div(style="display:inline-block",
      #   selectInput(ns("index"), "Tag-No",
      #   multiple=FALSE, size=1, selectize = FALSE, 
      #   choice=list(), selected=NULL, width="60px"  )
      # )
    ) #absolute panel end
  )
}

moduleTagDrag<-function(input, output, session, 
  barName, 
  name, 
  nameChoices,
  rowIndex,
  rowIndexChoices,
  matColIndex,
  matColIndexChoices
){

  # non-reactive function (not used???)
  # exGetTagColChoice<-function(tagColChoices, currentChoice){
  #   if(length(tagColChoices)>0){
  #     tagColChoice<-currentChoice
  #     tagColChoices<-sort(tagColChoices)
  #     if( length(tagColChoice)==0 || 
  #         !(tagColChoice %in% tagColChoices ) ){
  #       tagColChoice<-tagColChoices[length(tagColChoices)]
  #     }
  #   } else { #hide it
  #     tagColChoice<-NULL
  #   }
  #   tagColChoice
  # }
  
  observeEvent(barName(), { #update the name 
    if(identical( barName(), 'tagDrag')){
      updateSelectInput(session, "name", 
        choices=nameChoices(), 
        selected=name())
    } 
  })  
  
  observeEvent( c(barName(), rowIndex(), rowIndexChoices() ),  { #update index
     
     if(identical( barName(), 'tagDrag')){
      updateNumericInput(
        session, 
        "rowIndex", 
        min=min(rowIndexChoices()),
        max=max(rowIndexChoices()),
        value=rowIndex()
      )
    } 
  })
  
  observeEvent( c(barName(), matColIndex(), matColIndexChoices() ),  { #update index
    if(identical( barName(), 'tagDrag')){
      updateNumericInput(
        session, 
        "matColIndex", 
        min=min(matColIndexChoices()), 
        max=max(matColIndexChoices()), 
        value=matColIndex()
      )
    } 
  })
  
  
  
  list(
    name        =reactive({input$name}),
    rowIndex    =reactive({as.numeric( input$rowIndex )}),
    tagClone    =reactive({input$tagClone}),
    tagDelete   =reactive({input$tagDelete}),
    tagMoveUp   =reactive({input$tagMoveUp}),
    tagMoveDown   =reactive({input$tagMoveDown})
  )
}