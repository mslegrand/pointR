moduleTagDragUI<-function(id, input, output) { 
  ns <- NS(id)  
  tagList(
    absolutePanel( "class"="footerPanel", draggable=FALSE,
      actionButton(ns("tagClone"),   label = "Clone"   ),
      actionButton(ns("tagDelete"),  label = "Delete"),
      actionButton(ns("tagMoveUp"), label = "Send Up"),
      actionButton(ns("tagMoveDown"), label = "Send  Down")
    ),
    absolutePanel( top=50, left=0, width="100%", draggable=TRUE, 
          div(style="display:inline-block",
            selectInput( ns("name"), "Point Matrix",
            multiple=FALSE, size=1, selectize = FALSE,
            choice=list(),  selected=NULL, width="100px"  )
          ),
          div(style="display:inline-block",
            selectInput(ns("index"), "Tag-No",
            multiple=FALSE, size=1, selectize = FALSE, 
            choice=list(), selected=NULL, width="60px"  )
          )
    ) #absolute panel end
  )
}

moduleTagDrag<-function(input, output, session, 
  barName, 
  getTagNameChoices,
  getTagName, 
  getTagIndexChoices,
  getTagIndex
){

  # non-reactive function (not used???)
  exGetTagColChoice<-function(tagColChoices, currentChoice){
    if(length(tagColChoices)>0){
      tagColChoice<-currentChoice
      tagColChoices<-sort(tagColChoices)
      if( length(tagColChoice)==0 || 
          !(tagColChoice %in% tagColChoices ) ){
        tagColChoice<-tagColChoices[length(tagColChoices)]
      }
    } else { #hide it
      tagColChoice<-NULL
    }
    tagColChoice
  }
  
  observe({ #update the name 
    if(identical( barName(), 'tagDrag')){
      tagNameChoices<-getTagNameChoices() 
      tagName<-getTagName()
      updateSelectInput(session, "name", 
        choices=tagNameChoices, 
        selected=tagName)
    } 
  })  
  
  observe({ #update index
     if(identical( barName(), 'tagDrag')){
      tagIndxChoices<-getTagIndexChoices()
      tagIndx<-getTagIndex()
      updateSelectInput(session, "index", 
      choices=tagIndxChoices, selected=tagIndx)
    } 
  }) 
  
  list(
    name        =reactive({input$name}),
    index       =reactive({as.numeric( input$index )}),
    tagClone    =reactive({input$tagClone}),
    tagDelete   =reactive({input$tagDelete}),
    tagMoveUp   =reactive({input$tagMoveUp}),
    tagMoveDown   =reactive({input$tagMoveDown})
  )
}