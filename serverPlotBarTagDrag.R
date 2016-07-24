

output$TagDragPanel<-renderUI({
  conditionalPanel( "input.plotNavBar=='dragTag'",   
    absolutePanel( top=50, left=0, width=250, draggable=TRUE,
      style=cstyle$wellPoint,
      fluidRow(
        column(8, 
          selectInput( "tagName2", "Tagged Points", list(), 
          selected=NULL,  multiple=FALSE,  selectize = FALSE,  
          width="140px" , size=1 )
        ),
        column(4, 
        selectInput("tagIndx2", "Tag Index", list(),
        selected=NULL,  multiple=FALSE,  selectize = FALSE,  
        width="60px" , size=1 )
        )
      ) #fluid
    )
  ) #dragtag
})


observe({
  user$code
  #input$plotNavBar
  tagName<-selectedPoint$name
  tagIndx<-selectedPoint$point.index
  if(input$plotNavBar=="dragTag"){
    isolate({
      #print("plotBar or user$code change")
      
      tagNameChoices<-getTagNameChoices() #uses getPtDefs()
      
      tagName<-exGetTagName( tagNameChoices, tagName)
      if(length(tagName)>0){
        selectedPoint$name<-tagName #here is where the potential issue arrises
      }
      updateSelectInput(session, "tagName2", choices=tagNameChoices, selected=tagName)
      
      df<-getPtDefs()$df[[tagName]]
      tagIndxChoices<-df$tag
      #print(tagIndx)
      tagIndx<-exGetTagIndx(tagIndxChoices, tagIndx)
      #print(tagIndx)
      if( length(tagName)>0 && length(tagIndx>0)){
        selectedPoint$point.index<-tagIndx 
      }
      updateSelectInput(session, "tagIndx2", choices=tagIndxChoices, selected=tagIndx)
    })
  }
 })
 
 observe({
  if(input$plotNavBar=="dragTag"){
    input$tagName2
    isolate({
      #print("dragTag input$tagName change")
      if(!is.null(input$tagName2)){
        selectedPoint$name<-input$tagName2
      }
    })  
  }
})

observe({
  if(input$plotNavBar=="dragTag"){
    input$tagIndx2
    isolate({
    #print("dragTag input$tagIndx2 change")
      if(!is.null(input$tagIndx2)){
        selectedPoint$point.index<-input$tagIndx2
      }
    })
  }
})


#-------------------------------------------


  showPts.dragTag %<c-% function(ptName, pts, selectedPointIndx, ptDisplayMode,  tags=NULL){
    #cat(file=stderr(),"entering drag.Tag\n")
    #print("entering drag.Tag")
    if( is.null(pts) ) {return(NULL) } 
    if(length(pts)<2)  {return(NULL) }
    tag.indx<-selectedPointIndx #this is the position of the first point of the tagged set 
    semitransparent<-0.3
    colorScheme<-c(default="purple", ending="red", selected="blue")
    color<-colorScheme[1]
    m<-matrix(pts,2)
    if( !is.null(tag.indx) && !is.null(tags)){
      ti<-which(max(tags[tags<=tag.indx])==tags )
      id.nos<-sequence(ncol(m))
      ids<-paste("pd",ptName,id.nos,sep="-")
      tagInterval<-findInterval(id.nos,tags)
      tagIntList<-tapply(id.nos, findInterval(id.nos,tags), list )
      opacity<-rep(semitransparent, length(tags))
      opacity[ti]<-1
      # iterate over tagIntList
      indx<-unique(tagInterval)
      indx<-indx[-ti]
      list(
        g( opacity=opacity[ti], 
           fill='purple',
           transform="matrix(1 0 0 1 0 0)", 
           onmousedown="selectElement(evt)",
           tid=paste0("ptR_Tag_",ti),
           lapply(tagIntList[[ti]], function(j){
             circle(   cxy=m[,j], r=8)
           })
        ),
        lapply(indx, function(i){
          #browser()
          g( opacity=opacity[i], 
             fill='purple',
             transform="matrix(1 0 0 1 0 0)", 
             #onmousedown="selectElement(evt)",
             tid=paste0("ptR_Tag_",i),
             lapply(tagIntList[[i]], function(j){
               circle(   cxy=m[,j], r=8)
             })
          )
        })
      )
      } #end if
  } #end showPts




####################

# change in code or plotNavBar fires the setting of selector pointNamesTagDrag 
# change in selector pointNamesTagDrag fires setting of tagDragIndex
# change in tagDragIndex fires svg dragTag

#sets ptR$select, tagPts, 
#observe({
#  user$code
#  input$plotNavBar
#  tagPtsId<-"tagPts2"
#  isolate({
#    if(input$plotNavBar=="dragTag"){
#      ptRList    <-getPtDefs()$pts
#      tagRList   <-getPtDefs()$df
#      tagNamechoices <- intersect(names(ptRList),names(tagRList))
#      ptChosen<-selectedPoint$name
#      if(length(tagNamechoices)>0){
#        if(ptChosen %in% tagNamechoices){
#          tagName<-ptChosen
#        } else {
#          tagName<-tail(tagNamechoices,1)
#          updateSelectInput(session, "ptRSelect", selected=tagName )
#          selectedPoint$name=tagName
#        }
#        updateSelectInput(session, tagPtsId, choices=tagNamechoices, selected=tagName )
#      } else {
#        updateSelectInput(session, tagPtsId, choices=list(), selected=NULL )
#      }
#    }
#  })
#})


# almost a regular function to compute tagIndx
# df$tag provides tagIndxChoices, 
# note: sets selectedPoint$point.index

#pointIndx2tagIndx<-function( point.index, df){
#  tagIndxChoices<-NULL
#  tagIndx<-NULL
#  if( !is.null(df)){
#    tagIndxChoices<-df[["tag"]]
#  }
#  if(!is.null(tagIndxChoices)){
#    pt.indx<-max(1,selectedPoint$point.index)
#    selectedTagIndx<-max(tagIndxChoices[ tagIndxChoices<= pt.indx])
#    if(selectedPoint$point.index>0){
#      selectedPoint$point.index<-selectedTagIndx
#    }
#    tagIndx<-selectedPoint$point.index
#  } 
#  tagIndx
#}

# -----------SET ACTIVE TAG INDX ------------------------
# observers tagPts
# sets tagIndx
#     
#sets ptR$select, tagIndx, 
#observes: tagPts, code, plotNavBar
#uses values of getPtDefs()
#observe({ 
#  # input$tagPts2
#  # user$code
#  # input$plotNavBar
#  tagName<-selectedPoint$name
#  point.indx<-selectedPoint$point.index
#  isolate({ 
#    if(input$plotNavBar=="dragTag"){
#      tagIndxId<-"tagIndx2"
#      tagIndxChoices<-NULL
#      if(!is.null(tagName)){
#        tagRList<-getPtDefs()$df
#      }
#      if(!is.null(tagRList)){
#        df<-tagRList[[tagName]]
#        if(!is.null(df)){
#          tagIndxChoices<-df$tags
#        }
#      }
#      if(!is.null(tagIndxChoices)){
#        pt.indx<-max(1,selectedPoint$point.index)
#        selectedTagIndx<-max(tagIndxChoices[ tagIndxChoices<= pt.indx])
#        if(selectedPoint$point.index>0){
#          selectedPoint$point.index<-selectedTagIndx
#        }
#        updateSelectInput(session, tagIndxId,
#                          choices=tagIndxChoices,
#                          selected=selectedTagIndx
#        )
#        updateSelectInput(session, "ptRSelect",
#                          choices=names(getPtDefs()$pts),
#                          selected=name
#        )
#      } else {
#        updateSelectInput(session, tagIndxId,
#                          choices=list(),
#                          selected=NULL
#        )
#      }
#    }
#  })
#})



    
#   output$NavBarPlotFloaterPanel<-renderUI({
#     navBarPlotIn<-input$plotNavBar
#     print(navBarPlotIn)
#     if(length(navBarPlotIn)==0 ){ return() }
#   #  top=50; left=0; width=650; draggable=TRUE
#   #  multiple=FALSE; size=1; selectize=FALSE
#     #selector<-function(inputId, label, choices)
#     print(navBarPlotIn)
#     #isolate(
#       switch(navBarPlotIn,
#              Points= absolutePanel( top=50, left=0, width=650, draggable=TRUE,
#                                     style=cstyle$wellPoint,
#                                     fluidRow(
#                                       column(4, 
#                                              selectInput("ptRSelect", "Point Matrix", list("x"), 
#                                                          selected="x", multiple=FALSE,  selectize = FALSE,
#                                                          width="150px",size=1  
#                                              )
#                                       ),
#                                       column(3,
#                                              selectInput("ptDisplayMode", "Display Mode",
#                                                          list("Normal","Labeled","Hidden"), selected="Normal", 
#                                                          multiple=FALSE, selectize = FALSE,
#                                                          width="150px", size=1   
#                                              )
#                                       ),
#                                       column(2,
#                                              selectInput("tagFreq", "Auto Tag",
#                                                          c(list("Off"),1:20), selected="Off", 
#                                                          multiple=FALSE, selectize = FALSE,
#                                                          width="80px", size=1  
#                                              )
#                                       ),
#                                       column(3,
#                                              checkboxInput("insertMode","Insert Mode",value = TRUE, width = "100px"),
#                                              checkboxInput("showGrid", "Show Grid", value = TRUE, width = "100px")
#                                       )
#                                     )), #end Points 
#              dragTag=absolutePanel( top=50, left=0, width=250, draggable=TRUE,
#                                     style=cstyle$wellPoint,
#                                     fluidRow(
#                                       column(8, 
#                                              selectInput( "tagPts2", "Tagged Points", list(), 
#                                                           selected=NULL,  multiple=FALSE,  selectize = FALSE,  
#                                                           width="140px" , size=1 
#                                              )
#                                       ),
#                                       column(4, 
#                                              selectInput("tagIndx2", "Tag Index", list(),
#                                                          selected=NULL,  multiple=FALSE,  selectize = FALSE,  
#                                                          width="60px" , size=1 
#                                              )
#                                       )
#                                     ) #fluid
#              ), #dragtag
#              tagValues=absolutePanel( top=50, left=0, width=650, draggable=TRUE, 
#                                       style=cstyle$wellPoint,
#                                       fluidRow(
#                                         column(2, 
#                                                selectInput( "tagPts", "Point Matrix",
#                                                             multiple=FALSE, size=3, selectize = FALSE,
#                                                             list(),  selected=NULL, width="100px"  
#                                                )
#                                         ),
#                                         column(2, 
#                                                selectInput("tagIndx", "Tag-No",
#                                                            multiple=FALSE, size=3, selectize = FALSE, 
#                                                            list(), selected=NULL, width="60px"  
#                                                )
#                                         ),
#                                         column(2, 
#                                                selectInput("tagCol", "Col-Name",
#                                                            multiple=FALSE, size=3, selectize = FALSE, 
#                                                            list(),  selected=NULL, width="100px"  
#                                                )
#                                         ),
#                                         column(3, 
#                                                selectInput("tagColVal", "Col-Value", 
#                                                            multiple=FALSE, size=3, selectize = FALSE,  
#                                                            list(),  selected=NULL, width="100px"  
#                                                )
#                                         ),
#                                         column(3, 
#                                                textInput("tagValEd", "Alt-Value", value=""),
#                                                actionButton("insertVal2Col", label = "Apply Alternate Val", style=cstyle$buttonSmall)
#                                         )
#                                       )
#              ) #TAGS panel end
#       #)
#     ) 
# })
