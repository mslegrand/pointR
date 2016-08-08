
moduleTagValUI<-function(id, input, output) { 
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
        ),
        column(2, 
          selectInput(ns("colName"), "Col-Name",
          multiple=FALSE, size=3, selectize = FALSE, 
          list(),  selected=NULL, width="100px"  )
        ),
        column(3, 
          selectInput(ns("colVal"), "Col-Value", 
          multiple=FALSE, size=3, selectize = FALSE,  
          list(),  selected=NULL, width="100px"  )
        ),
        column(3, 
          textInput(ns("colValEd"), "Alt-Value", value=""),
          actionButton("insertVal2Col", label = "Apply Alternate Val", style=cstyle$buttonSmall)
        )
      ) #fluidRow  end
  ) #absolute panel end
}

moduleTagVal<-function(input, output, session, 
  barName, 
  getTagNameChoices,
  getTagName, 
  getTagIndexChoices,
  getTagIndex,
  getTagColChoices,
  getTagCol,
  getTagValueChoices,
  getTagValue
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
    if(identical( barName(), 'tagValues')){
      tagNameChoices<-getTagNameChoices() 
      tagName<-getTagName()
      updateSelectInput(session, "name", 
        choices=tagNameChoices, 
        selected=tagName)
    } 
  })  
  
  observe({ #update index
     if(identical( barName(), 'tagValues')){
      tagIndxChoices<-getTagIndexChoices()
      tagIndx<-getTagIndex()
      updateSelectInput(session, "index", 
      choices=tagIndxChoices, selected=tagIndx)
    } 
  }) 
  
  observe({ #tab col selection
    if(identical( barName(), 'tagValues')){
      tagColChoices<-getTagColChoices()
      tagCol<-getTagCol()
      updateSelectInput(session, "colName", choices=tagColChoices, selected=tagCol)
    }
  })
  
  observe({ #tag val selection
    if(identical( barName(), 'tagValues')){
      tagValueChoices<-getTagValueChoices()
      tagValChoice<-getTagValue()
      updateSelectInput(session, "colVal", choices=tagValueChoices, selected=tagValChoice)
    }    
  })
  
  list(
    name        =reactive({input$name}),
    index       =reactive({input$index}),    
    colName     =reactive({input$colName}),
    colVal      =reactive({input$colVal})
  )
}