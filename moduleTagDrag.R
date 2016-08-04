moduleTagDragUI<-function(id, input, output) { 
  ns <- NS(id)
  absolutePanel( top=50, left=0, width=650, draggable=TRUE, 
      style=cstyle$wellPoint,
      fluidRow(
        column(2, 
          selectInput( ns("name"), "Point Matrix",
          multiple=FALSE, size=3, selectize = FALSE,
          list(),  selected=NULL, width="100px"  )
        ),
        column(2, 
          selectInput(ns("index"), "Tag-No",
          multiple=FALSE, size=3, selectize = FALSE, 
          list(), selected=NULL, width="60px"  )
        )
      ) #fluidRow  end
  ) #absolute panel end
}

moduleTagDrag<-function(input, output, session, 
  barName, 
  getTagNameChoices,
  getTagName, 
  getTagIndexChoices,
  getTagIndex
){

  # non-reactive function
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
    index       =reactive({input$index})
  )
}