

moduleTagValUI<-function(id, input, output) { 
  ns <- NS(id)
  absolutePanel( top=50, left=0, width=650, draggable=TRUE, 
      style=cstyle$wellPoint,
      #fluidRow(
        div(style="display:inline-block",
          selectInput( ns("name"), "Point Matrix",
          multiple=FALSE, size=1, selectize = FALSE,
          choices=list(),  selected=NULL, width="100px"  )
        ),
        div(style="display:inline-block",
          selectInput(ns("colName"), "Attribute",
          multiple=FALSE, size=1, selectize = FALSE, 
          choices=list(),  selected=NULL, width="100px"  )
        ),
        div(style="display:inline-block",
          selectInput(ns("index"), "Tag-No",
          multiple=FALSE, size=1, selectize = FALSE, 
          choices=list(), selected=NULL, width="60px"  )
        ), 
        div(style="display:inline-block", 
          selectInput(ns("colVal"), "Value Chooser", 
          multiple=FALSE, size=1, selectize = FALSE,  
          choices=list(),  selected=NULL, width="100px"  )
        ),
        div(style="display:inline-block",
          textInput(ns("colValEd"), "Value Choice", value="",width="100px")
        ),
        div(style="display:inline-block",
          actionButton(ns( "insertVal2ColButton" ), label = "Update") #buttonSmall
        )
        
      #) #fluidRow  end
  ) #absolute panel end
}

moduleTagVal<-function(input, output, session, 
  barName, 
  getPtDefs,
  getTagNameChoices,
  getTagName, 
  getTagIndexChoices,
  getTagIndex
){

  local<-reactiveValues( tagRList = NULL)
  # this should be updated whenever we change to this page
  # or we change the code/ptDefs
  observeEvent( getPtDefs(),{
    local$tagRList<-getPtDefs()$df
  } 
  )
  
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
  
  getName<-reactive({input$name})
  getIndex<-reactive({input$index})
  getDF<-reactive({ 
    if(
    !is.null(getName()) && 
    !is.null( getPtDefs()$df ) && 
    getName() %in% names(getPtDefs()$df)  ){
      getPtDefs()$df[[getName()]] 
    } else {
      NULL
    }
  })
  
  
  getTagColChoices<-reactive({
    df<-getDF()
    if(!is.null(df)){
      tagColChoices<-setdiff(names(df),"tag")
    } else {
      NULL
    }
  })
  
  getTagCol<-reactive({ 
  tmp<-getTagColChoices()
    if( length(tmp)>0 &&
        length(input$colName)>0 &&
        input$colName %in% tmp){
      input$colName
    } else {
      tmp[1]
    }
  })
  
  getTagValueVector<-reactive({
    if(length(getTagCol())>0  &&
      length(getDF)>0){
      getDF()[[getTagCol()]]
    } else {
      NULL
    }
  })
  
  getTagValueChoices<-reactive({
    if(length(getTagCol())>0  &&
      length(getDF)>0){
      getDF()[[getTagCol()]]
    } else {
      NULL
    }
  })
  
  getTagValue<-reactive({
    if(length(getTagValueChoices())>0){
      ptIndx<-getIndex()
      tags<-getDF()$tag
      indx<-which(ptIndx==tags)
      getTagValueChoices()[[indx]]
    }
  })
  
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
  
  # want if value selection changes, or insertVal2ColButton pressed
  # to return an updated DF.
  
  observe({ #tag val selection
    if(identical( barName(), 'tagValues')){
      tagValueChoices<-getTagValueChoices()
      tagValue<-getTagValue()
      updateSelectInput(session, "colVal", choices=tagValueChoices, selected=tagValue)
    }    
  })
  
  observe({
    if(identical( barName(), 'tagValues')){
      tagValue<-input$colVal
      updateTextInput(session, "colValEd", value=tagValue)
    }  
  })
  
  observeEvent(
    input$insertVal2ColButton,
    {
      if(identical( barName(), 'tagValues')){
        if(
          !is.null(local$tagRList) && length(input$name)>0 && 
          length(input$colName)>0 && length(input$index)>0 &&
          length(input$colValEd)>0
        )
        {
          tagValueVec<-  local$tagRList[[input$name]][[input$colName]]
          tags<-local$tagRList[[input$name]]$tag
          tmp<-as.integer(input$index)
          indx<-which(tmp==tags)
          tagValueVec[indx]<-input$colValEd
          local$tagRList[[input$name]][[input$colName]]<-tagValueVec
        }
      }
    }
  )
  
  
  #when name, index, colName valid, and colVal changes, update the ptDefs and code
  list( 
    name      =reactive({input$name}),
    index     =reactive({input$index}),
    tagRList  =reactive({local$tagRList}) 
    )
}