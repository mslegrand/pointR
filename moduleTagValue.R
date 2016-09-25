
#currently update triggers value from choice to ace
# propose
moduleTagValUI<-function(id, input, output) { 
  ns <- NS(id)
  useShinyjs()
  absolutePanel( top=50, left=0, width="100%", draggable=TRUE, 
        div(style="display:inline-block",
          selectInput( ns("name"), "Point Matrix",
          multiple=FALSE, size=1, selectize = FALSE,
          choices=list(),  selected=NULL, width="80px"  )
        ),
        div(style="display:inline-block",
            selectInput(ns("index"), "Tag-No",
                        multiple=FALSE, size=1, selectize = FALSE, 
                        choices=list(), selected=NULL, width="60px"  )
        ), 
        div(style="display:inline-block",
          selectInput(ns("colName"), "Attribute",
          multiple=FALSE, size=1, selectize = FALSE, 
          choices=list(),  selected=NULL, width="80px"  )
        ),
        div(style="display:inline-block", 
          selectInput(ns("colVal"), "Value", 
          multiple=FALSE, size=1, selectize = FALSE,  
          choices=list(),  selected=NULL, width="100px"  )
        ),
        div(style="display:inline-block",
            actionButton(ns( "updateVal" ), label = "Update") #buttonSmall
        ),
        div(style="display:inline-block",
          actionButton(ns( "New" ), label = "New") #buttonSmall
        )
  ) # panel end
}

moduleTagVal<-function(input, output, session, 
  id2,
  barName, 
  getPtDefs,
  getTagNameChoices,
  getTagName, 
  getTagIndexChoices,
  getTagIndex
){
  ns<-NS(id2)
  local<-reactiveValues( tagRList = NULL)
  # this should be updated whenever we change to this page
  # or we change the code/ptDefs
  observeEvent( getPtDefs(),{
    local$tagRList<-getPtDefs()$df
  })
  
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
      getName() %in% names(getPtDefs()$df)  
    ){
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
  
  
  observe({ #tag val selection
    if(identical( barName(), 'tagValues')){
      tagValueChoices<-getTagValueChoices()
      tagValue<-getTagValue()
      updateSelectInput(session, "colVal", choices=tagValueChoices, selected=tagValue)
    }    
  })
  
  
  observeEvent(
    input$updateVal,
    {
      if(identical( barName(), 'tagValues')){
        if(
          !is.null(local$tagRList) && nchar(input$name)>0 && 
          length(input$colName)>0 && length(input$index)>0 &&
          length(input$colVal)>0
        )
        {
          tagValueVec<-  local$tagRList[[input$name]][[input$colName]]
          tags<-local$tagRList[[input$name]]$tag
          tmp<-as.integer(input$index)
          indx<-which(tmp==tags)
          tagValueVec[indx]<-input$colVal
          local$tagRList[[input$name]][[input$colName]]<-tagValueVec
        }
      }
    }
  )
  
  
  
  # Return the UI for a modal dialog with data selection input. I
  dataModal <- function(attrName) {
    modalDialog(
      textInput(ns("modalAttrName"), "Attribute Name", value=attrName),
      textInput(ns("modalAttrValue"), "New Attribute Value"), 
      span('Enter new choide for the given named attribute'), 
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("ok"), "OK")
      )
    ) 
  }
  
  observeEvent(input$New,{
    attrName<-input$colName
    showModal( dataModal(attrName ) ) 
  })
  
  observeEvent(input$ok, {
    #print("hello")
    if(identical( barName(), 'tagValues')){
      if( length(input$name)>0 && 
          length(input$index)>0 &&
        !is.null(local$tagRList)  
      ) 
      {
        if(nchar(input$modalAttrName)>0 && 
          nchar (input$modalAttrValue)>0 && 
          input$modalAttrName!="tag"){
        }
        tagAttrNames<-  names(local$tagRList[[input$name]])
        tags<-local$tagRList[[input$name]]$tag
        if(input$modalAttrName %in% tagAttrNames){
          tagValueVec<-  local$tagRList[[input$name]][[input$modalAttrName]]
          tmp<-as.integer(input$index)
          indx<-which(tmp==tags)
          tagValueVec[indx]<-input$modalAttrValue   
        } else {
          tagValueVec<-rep( input$modalAttrValue, length(tags) )  
        }
        local$tagRList[[input$name]][[input$modalAttrName]]<-tagValueVec
        removeModal()
      }
    }
  })
  
  
  
  
  
  #when name, index, colName valid, and colVal changes, update the ptDefs and code
  list( 
    name      =reactive({input$name}),
    index     =reactive({input$index}),
    tagRList  =reactive({local$tagRList}) 
    )
}