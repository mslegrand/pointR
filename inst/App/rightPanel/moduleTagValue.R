
#currently update triggers value from choice to ace
# propose
moduleTagValUI<-function(id, input, output) { 
  ns <- NS(id)
  #useShinyjs()
  absolutePanel( 
    top=50, left=0, width="100%", "class"="headerPanel", draggable=FALSE,
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
      selectInput(ns("attrName"), label="Attribute",
      multiple=FALSE, size=1, selectize = FALSE, 
      choices=list(),  selected=NULL, width="120px"  )
    ),
    div(style="display:inline-block", 
      selectInput(ns("attrVal"), "Value", 
      multiple=FALSE, size=1, selectize = FALSE,  
      choices=list(),  selected=NULL, width="100px"  )
    ),
    div(style="display:inline-block",
      actionButton(ns( "New" ), label = "New") #buttonSmall
    ),
    div(style="display:inline-block",
        actionButton(ns( "updateVal" ), label = "Update") #buttonSmall
    )
  ) # panel end
}

moduleTagVal<-function(input, output, session, 
  id2,
  barName, 
  getCode,
  getPtDefs,
  getTagNameChoices,
  getTagName, 
  getTagIndexChoices,
  getTagIndex
){
  ns<-NS(id2)
  localReactive<-reactiveValues( 
    tagRList = NULL,
    updateTagsNow =0,
    attribute=NULL
  )
  
   
  #===
  # this should be updated whenever 
  # 1) we change to this page
  # or 
  # 2) we change the code/ptDefs, 
  # ???should not use getPtDefs???
  observeEvent( getPtDefs(),{
    #isolate({print("assigning local tagRList")})
    localReactive$tagRList<-getPtDefs()$df
    #isolate({print(localReactive$tagRList)})
    
  })
  
  # a non-reactive function (not used???)
  # exGetTagColChoice<-function(tagColChoices, currentChoice){
  #   if(length(tagColChoices)>0){
  #     tagColChoice<-currentChoice
  #     tagColChoices<-sort(tagColChoices)
  #     if( length(tagColChoice)==0 || 
  #         !(tagColChoice %in% tagColChoices ) ){
  #       tagColChoice<-tail(tagColChoices,1)
  #     }
  #   } else { #hide it
  #     tagColChoice<-NULL
  #   }
  #   tagColChoice
  # }
  
  #===
  getName<-reactive({input$name})
  getIndex<-reactive({input$index})
  
  #===
  getDF<-reactive({ 
    # isolate({print("getDF")})
    # should use localReactive$tagRList instead of getPtDefs()$df ???
    if(
      !is.null(getName()) && 
      !is.null( localReactive$tagRList ) && 
      getName() %in% names(localReactive$tagRList)  
    ){
      localReactive$tagRList[[getName()]] 
    } else {
      NULL
    }
  })
  
  #=== attr choices
  #get attribute choices, uses DF from localReactive$tagRList
  # and name from input$name
  getTagColChoices<-reactive({
    #isolate({print("getTagColChoices")})
    df<-getDF()
    if(!is.null(df)){
      tagColChoices<-setdiff(names(df),"tag")
    } else {
      NULL
    }
  })
  
  
  #=== attr selelection
  # gets attribute selection, input$attribName (ie. input$attrib)
  # trigger by:
  # 1) input$attrName
  # 2) getTagColChoices() 
  getTagCol<-reactive({  
    #isolate({print("getTagCol")})
    choices<-getTagColChoices() #grabs from local df
    if(!is.null(localReactive$attribute)){
        tmp<-localReactive$attribute
        isolate({localReactive$attribute<-NULL})
        return(tmp)
    }
    if(length(choices)>0 && 
       length(input$attrName)>0 &&
       input$attrName %in% choices)
    {
      input$attrName
    } else {
      tail(choices,1)
    }
  })  
  
  #!!! getTagValueVector and getTagValueChoices are identical !!!
  # getTagValueVector<-reactive({ 
  #   # isolate({print("getTagValueVector")}) 
  #   if(length(getTagCol())>0  &&
  #     length(getDF)>0){
  #     getDF()[[getTagCol()]]
  #   } else {
  #     NULL
  #   }
  # })
  
  #!!! getTagValueVector and getTagValueChoices are identical !!!
  getTagValueChoices<-reactive({
    #isolate({print("getTagValuesChoices")})
    if(length(getTagCol())>0  &&
      length(getDF)>0){
      getDF()[[getTagCol()]]
    } else {
      NULL
    }
  })
  
  getTagValue<-reactive({
    #isolate({print("getTagValue")})
    if(length(getTagValueChoices())>0){
      ptIndx<-getIndex()
      tags<-getDF()$tag
      indx<-which(ptIndx==tags)
      getTagValueChoices()[[indx]]
    }
  })
  
  # updates the name selection using data from server.R
  observe({ #update the name 
    if(identical( barName(), 'tagValues')){
      tagNameChoices<-getTagNameChoices() # passed in from server
      tagName<-getTagName() # passed in from server
      updateSelectInput(session, "name", 
        choices=tagNameChoices, 
        selected=tagName)
    } 
  })  
  
  #update tag index using data from server.R 
  observe({ #update index and choices
     if(identical( barName(), 'tagValues')){
      tagIndxChoices<-getTagIndexChoices() #passed in from server
      tagIndx<-getTagIndex() # passed in from server
      updateSelectInput(session, 
        "index", choices=tagIndxChoices, selected=tagIndx
      )
    } 
  }) 
  
  # update attrName (attribute)
  # conditional on: barName(), 
  # triggered by:
  # 1) getTagColChoices (local$name and local$tagList changes)
  # 2) getTagCol
  # sets attributeName
  # note triggering occurs after new attribute
  observe({ #tab col selection
    if(identical( barName(), 'tagValues')){
      #isolate({print("update attrName 1")})
      #isolate({print(input$attrName)})
      tagColChoices<-getTagColChoices()
      tagCol<-getTagCol()
      #print(tagCol)
      #print(tagColChoices)
      #updateSelectInput(session, "attrName", choices=tagColChoices, selected=tagCol)
      updateSelectInput(session, "attrName", choices=tagColChoices)
      updateSelectInput(session, "attrName",  selected=tagCol)
      #isolate({print("end update attrName 1")})
    }
  })
  
  #updates the value using the DF 
  # conditional on: barName
  # triggered by: 
  # 
  observe(
    { #tag val selection
    if(identical( barName(), 'tagValues')){
      #print("inside observe barName")
      tagValueChoices<-getTagValueChoices()
      tagValue<-getTagValue()
      updateSelectInput(session, "attrVal", 
        choices=tagValueChoices, 
        selected=tagValue
      )
    }    
  })
  
  
  # triggered by pressing "Update" button
  observeEvent(
    input$updateVal,
    {
      if(identical( barName(), 'tagValues')){
        if(
          !is.null(localReactive$tagRList) && nchar(input$name)>0 && 
          length(input$attrName)>0 && length(input$index)>0 &&
          length(input$attrVal)>0
        )
        {
          tagValueVec<-  localReactive$tagRList[[input$name]][[input$attrName]]
          tags<-localReactive$tagRList[[input$name]]$tag
          tmp<-as.integer(input$index)
          indx<-which(tmp==tags)
          tagValueVec[indx]<-input$attrVal
          localReactive$tagRList[[input$name]][[input$attrName]]<-tagValueVec
          localReactive$updateTagsNow<-localReactive$updateTagsNow+1
        }
      }
    }
  )
  
  
  
  # Return the UI for a modal dialog with data selection input. I
  attrValueModal <- function(attrName, failedName=FALSE, failedValue=FALSE) {
    modalDialog(
      textInput(ns("modalAttrName"), "Attribute Name", value=attrName),
      textInput(ns("modalAttrValue"), "New Attribute Value"), 
      span('Enter new choice for the given named attribute'), 
      if (failedName)
        div(tags$b("Invalid Attribute Name: must begin with a character", style = "color: red;")),
      if (failedValue)
        div(tags$b("Invalid Attribute Value: must begin with printable character other than space", style = "color: red;")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("ok"), "OK")
      )
    ) 
  }
  
  #invokes modal for new attribute
  observeEvent(input$New,{
    attrName<-input$attrName
    showModal( attrValueModal(attrName ) ) 
  })
  
  observe( {
    localReactive$attribute
    # print("inside observeEvent( localReactive$attribute,")
    if(!is.null(localReactive$attribute)){
      # print("inside !is.null(localReactive$attribute)")
      #choices =localReactive$attribute$attrNames
      choices = getTagColChoices()
      selected=localReactive$attribute
      updateSelectInput(session, "attrName", #fails for some reason
                        choices=choices,
                        selected=selected)
    }
  })
  # 
  
  
  #dialog box ok handler
  observeEvent(input$ok, {
    #print("ok button observed")
    if(identical( barName(), 'tagValues')){
      if( length(input$name)>0 &&  
          length(input$index)>0 &&
          !is.null(localReactive$tagRList)
      ){
        nameOK<-grepl(pattern = "^[[:alpha:]]", input$modalAttrName) && input$modalAttrName!="tag"
        valueOK<-grepl(pattern = "^[[:graph:]]", input$modalAttrValue)
        if( nameOK && valueOK){
            tagAttrNames<-  names(localReactive$tagRList[[input$name]])
            # attrName<-input$modalAttrName
            modalAttrName<-input$modalAttrName
            
            tags<-localReactive$tagRList[[input$name]]$tag
            if(modalAttrName %in% tagAttrNames){
              tagValueVec<-localReactive$tagRList[[input$name]][[modalAttrName]]
              tmp<-as.integer(input$index)
              indx<-which(tmp==tags)
              tagValueVec[indx]<-input$modalAttrValue 
              #isolate({print("update attrName 2")})
              updateSelectInput(session, 
                                inputId="attrName", #fails to update
                                selected=modalAttrName)
              #print(modalAttrName)
              #print(input$attrName)
              #isolate({print("end update attrName 2")})
            } else { #this is a new attributeName
              tagValueVec<-rep( input$modalAttrValue, length(tags) ) 
              tagAttrNames<-c(tagAttrNames,modalAttrName)
              #localReactive$attributeName<-attrName
              #isolate({print("update attrName 3")})
              updateSelectInput(session, 
                                inputId="attrName",  #fails to update
                                choices=tagAttrNames, 
                                selected=modalAttrName) 
              #print(modalAttrName) 
              #print(input$attrName) 
              #isolate({print("end update attrName 3")})
            }
            localReactive$attribute<-modalAttrName
            localReactive$tagRList[[input$name]][[modalAttrName]]<-tagValueVec
            localReactive$updateTagsNow<-localReactive$updateTagsNow+1
            removeModal()
      } else { # invalid modal input
          modalAttrName<-input$modalAttrName
          showModal(attrValueModal(modalAttrName, 
              failedName=!nameOK, failedValue=!valueOK) )
        }
      } #else ignore
    }
  })
  

  #when name, index, attrName valid, and attrVal changes, update the ptDefs and code
  list( 
    name      =reactive({input$name}),
    index     =reactive({input$index}),
    tagRList  =reactive({localReactive$tagRList}),
    updateTagsNow =reactive({localReactive$updateTagsNow})
    )
}