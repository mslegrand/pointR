
#currently update triggers value from choice to ace
# propose
moduleTagValUI<-function(id, input, output) { 
  ns <- NS(id)
  #useShinyjs()
  tagList(
    absolutePanel( "class"="footerPanel", draggable=FALSE,
                   actionButton(ns("newColumn"),   label = "Add Column"   ),
                   actionButton(ns("deleteColumn"),  label = "Delete Column")#,
                   #actionButton(ns("applyTibEdit"), label = "Apply Changes") 
    ), #end footer panel
    absolutePanel( 
      top=50, left=0, width="100%", "class"="headerPanel", draggable=FALSE,
      div(style="display:inline-block",
        selectInput( ns("name"), "Tibble",
        multiple=FALSE, size=1, selectize = FALSE,
        choices=list(),  selected=NULL, width="80px"  )
      ),
      div(style="display:inline-block",
          numericInput( ns("rowIndex"), "Row", 1, min=1, max=10, step=1, width="80px" )
      ), 
      div(style="display:inline-block",
        selectInput(ns("columnName"), label="Column",
        multiple=FALSE, size=1, selectize = FALSE, 
        choices=list(),  selected=NULL, width="120px"  )
      ),
      div(style="display:inline-block", 
          selectizeInput(ns("entryValue"), "Value", 
          #multiple=FALSE, #size=1, 
          options = list(create = TRUE),
        #selectize = TRUE,  
          choices=list(),  selected=NULL, width="100px"  )
      )
  ) # end header panel
  )
}

moduleTagVal<-function(input, output, session, 
  id2,
  barName, 
  name, 
  nameChoices,
  ptIndex,
  ptIndexChoices,
  rowIndex,
  rowIndexChoices,
  matColIndex,
  matColIndexChoices,
  columnName,
  columnNameChoices,
  getTibEntry,
  getTibEntryChoices
){
  ns<-NS(id2)
  result <- reactiveValues(
    ptDefs=NULL
  ) # note we add name post mortem!!!
  
  
  
  observeEvent(barName(), { #update the name 
    if(identical( barName(), 'tagValues')){
      
      updateSelectInput(session, "name", 
                        choices=nameChoices(), 
                        selected=name())
      updateNumericInput(
        session, 
        "rowIndex", 
        min=min(rowIndexChoices()),
        max=max(rowIndexChoices()),
        value=rowIndex()
      )
      updateSelectInput(session, "columnName", 
                        choices=columnNameChoices(), 
                        selected=columnName() )
      entry=getTibEntry()
      if(is(entry,'matrix')){
        entry='matrix'
        entryChoices=c('point','matrix')
      } else {
        entryChoices=getTibEntryChoices()
      }
      updateSelectInput(session, "entryValue", 
                        choices=entryChoices, 
                        selected=entry )
    } 
  })  
  
  observeEvent( c(barName(), rowIndex(), rowIndexChoices() ),  { #update index
    if(identical( barName(), 'tagValues')){
      updateNumericInput(
        session, 
        "rowIndex", 
        min=min(rowIndexChoices()),
        max=max(rowIndexChoices()),
        value=rowIndex()
      )
      entry=getTibEntry()
      
      if(is(entry,'matrix')){
        entry='matrix'
        cat("observeEvent( c(barName(), rowIndex(), rowIndexChoices(): entry=", entry, "\n")
        cat("observeEvent( c(barName(), rowIndex(), rowIndexChoices(): input$entryValue=", input$entryValue, "\n")
        entryChoices=c('point','matrix')
        if( !(input$entryValue %in% entryChoices ) ){
          updateSelectInput(session, "entryValue", 
                            choices=entryChoices, 
                            selected=entry )
        }
      } else {
        entryChoices=getTibEntryChoices()
        updateSelectInput(session, "entryValue", 
                          choices=entryChoices, 
                          selected=entry )
      }
    } 
  }) 
  
  observeEvent( columnName() , { 
    if(identical( barName(), 'tagValues')){
      updateSelectInput(session, "columnName", 
                        choices=columnNameChoices(), 
                        selected=columnName() )
      entry=getTibEntry()
      if(is(entry,'matrix')){
        entry='matrix'
        entryChoices=c('point','matrix')
      } else {
        entryChoices=getTibEntryChoices()
      }
      cat("observeEvent( columnName(): entry=", entry, "\n")
      updateSelectInput(session, "entryValue", 
                        choices=entryChoices, 
                        selected=entry )
    } 
  })

 
  
  # # triggered by pressing "Update" button
  # observeEvent(
  #   input$updateVal,
  #   {
  #     if(identical( barName(), 'tagValues')){
  #       if(
  #         !is.null(localReactive$tagRList) && nchar(input$name)>0 && 
  #         length(input$attrName)>0 && length(input$index)>0 &&
  #         length(input$attrVal)>0
  #       )
  #       {
  #         tagValueVec<-  localReactive$tagRList[[input$name]][[input$attrName]]
  #         tags<-localReactive$tagRList[[input$name]]$tag
  #         tmp<-as.integer(input$index)
  #         indx<-which(tmp==tags)
  #         tagValueVec[indx]<-input$attrVal
  #         localReactive$tagRList[[input$name]][[input$attrName]]<-tagValueVec
  #         localReactive$updateTagsNow<-localReactive$updateTagsNow+1
  #       }
  #     }
  #   }
  # )
  # 
  
  
  # # Return the UI for a modal dialog with data selection input. I
  # attrValueModal <- function(attrName, failedName=FALSE, failedValue=FALSE) {
  #   doOk<-paste0(
  #     'shinyjs.triggerButtonOnEnter(event,"',
  #     ns("ok"), 
  #     '")')
  #   modalDialog(
  #     onkeypress=doOk,
  #     textInput(ns("modalAttrName"), "Attribute Name", value=attrName),
  #     textInput(ns("modalAttrValue"), "New Attribute Value"), 
  #     span('Enter new choice for the given named attribute'), 
  #     if (failedName)
  #       div(tags$b("Invalid Attribute Name: must begin with a character", style = "color: red;")),
  #     if (failedValue)
  #       div(tags$b("Invalid Attribute Value: must begin with printable character other than space", style = "color: red;")),
  #     footer = tagList(
  #       modalButton("Cancel"),
  #       actionButton(ns("ok"), "Commit")
  #     )
  #   ) 
  # }
  # 
  # #invokes modal for new attribute
  # observeEvent(input$New,{
  #   attrName<-input$attrName
  #   showModal( attrValueModal(attrName ) ) 
  # })
  # 
  # observe( {
  #   localReactive$attribute
  #   if(!is.null(localReactive$attribute)){
  #     choices = getTagColChoices()
  #     selected=localReactive$attribute
  #     updateSelectInput(session, "attrName", #fails for some reason
  #                       choices=choices,
  #                       selected=selected)
  #   }
  # })
  # # 
  # 
  # 
  # #dialog box ok handler
  # observeEvent(input$ok, {
  #   if(identical( barName(), 'tagValues')){
  #     if( length(input$name)>0 &&  
  #         length(input$index)>0 &&
  #         !is.null(localReactive$tagRList)
  #     ){
  #       nameOK<-grepl(pattern = "^[[:alpha:]]", input$modalAttrName) && input$modalAttrName!="tag"
  #       valueOK<-grepl(pattern = "^[[:graph:]]", input$modalAttrValue)
  #       if( nameOK && valueOK){
  #           tagAttrNames<-  names(localReactive$tagRList[[input$name]])
  #           modalAttrName<-input$modalAttrName
  #           
  #           tags<-localReactive$tagRList[[input$name]]$tag
  #           if(modalAttrName %in% tagAttrNames){
  #             tagValueVec<-localReactive$tagRList[[input$name]][[modalAttrName]]
  #             tmp<-as.integer(input$index)
  #             indx<-which(tmp==tags)
  #             tagValueVec[indx]<-input$modalAttrValue 
  #             updateSelectInput(session, 
  #                               inputId="attrName", #fails to update
  #                               selected=modalAttrName)
  # 
  #           } else { #this is a new attributeName
  #             tagValueVec<-rep( input$modalAttrValue, length(tags) ) 
  #             tagAttrNames<-c(tagAttrNames,modalAttrName)
  #             updateSelectInput(session, 
  #                               inputId="attrName",  #fails to update
  #                               choices=tagAttrNames, 
  #                               selected=modalAttrName) 
  #           }
  #           localReactive$attribute<-modalAttrName
  #           localReactive$tagRList[[input$name]][[modalAttrName]]<-tagValueVec
  #           localReactive$updateTagsNow<-localReactive$updateTagsNow+1
  #           removeModal()
  #     } else { # invalid modal input
  #         modalAttrName<-input$modalAttrName
  #         showModal(attrValueModal(modalAttrName, 
  #             failedName=!nameOK, failedValue=!valueOK) )
  #       }
  #     } #else ignore
  #   }
  # })
  # 

  #when name, index, attrName valid, and attrVal changes, update the ptDefs and code
  list( 
    name         =reactive({input$name}),
    rowIndex     = reactive({input$rowIndex}),
    columnName    =reactive({input$columnName}),
    entryValue   =reactive(input$entryValue),
    newColumn =    reactive({input$newColumn}),
    deleteColumn = reactive({input$deleteColumn}),
    applyTibEdit = reactive({input$applyTibEdit})
  )
}