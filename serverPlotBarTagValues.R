output$TagValuesPanel<-renderUI({
  conditionalPanel( "input.plotNavBar=='tagValues'", 
    absolutePanel( top=50, left=0, width=650, draggable=TRUE, 
      style=cstyle$wellPoint,
      fluidRow(
        column(2, 
          selectInput( "tagName", "Point Matrix",
          multiple=FALSE, size=3, selectize = FALSE,
          list(),  selected=NULL, width="100px"  )
        ),
        column(2, 
          selectInput("tagIndx", "Tag-No",
          multiple=FALSE, size=3, selectize = FALSE, 
          list(), selected=NULL, width="60px"  )
        ),
        column(2, 
          selectInput("tagCol", "Col-Name",
          multiple=FALSE, size=3, selectize = FALSE, 
          list(),  selected=NULL, width="100px"  )
        ),
        column(3, 
          selectInput("tagColVal", "Col-Value", 
          multiple=FALSE, size=3, selectize = FALSE,  
          list(),  selected=NULL, width="100px"  )
        ),
        column(3, 
          textInput("tagValEd", "Alt-Value", value=""),
          actionButton("insertVal2Col", label = "Apply Alternate Val", style=cstyle$buttonSmall)
        )
      ) #TAGS panel end
    )
  ) 
})



#------------------------------

# 
# # -----------ACTIVE TAG PT------------------------
# #  observes code and plotNavBar
# #  sets active Tag and Tag index
# #       tagPts 
# #       tagIndx 
# #       ptRSelect 
# #       point.index 
# 
# #sets ptR$select, tagPts, 
# observe({
#   user$code
#   input$plotNavBar
#   isolate({
#     if(input$plotNavBar=="tagValues" ||
#        input$plotNavBar=="dragTag"){
#       tagPtsId<-switch( input$plotNavBar,
#         tagValues="tagPts",
#         dragTag="tagPts2"
#       )
#       #point.index<-selectedPoint$point.index
#       #selected   <-input$ptRSelect
#       selected<-selectedPoint$name
#       ptRList    <-getPtDefs()$pts
#       tagRList   <-getPtDefs()$df
#       tagNamechoices    <-intersect(names(ptRList),names(tagRList))
#       if(length(tagNamechoices)>0){
#         # Use selection of ptRSelect if in choices, ow last avail. tagPts2
#         #ptChosen<-input$ptRSelect
#         ptChosen<-selectedPoint$name
#         if(ptChosen %in% tagNamechoices){
#           tagName<-ptChosen
#         } else{
#           tagName<-tail(tagNamechoices,1)
#           updateSelectInput(session, "ptRSelect", selected=tagName )
#         }
#         updateSelectInput(session, tagPtsId, choices=tagNamechoices, selected=tagName )
#       } else {
#         updateSelectInput(session, tagPtsId, choices=list(), selected=NULL )
#       }
#     }
#   })
# })
# 
# 
# # -----------ACTIVE TAG INDX ------------------------
# # observers tagPts
# # sets tagIndx
# #     
# #sets ptR$select, tagIndx, 
# #observes: tagPts, code, plotNavBar
# #uses values of getPtDefs()
# observe({ 
#   input$tagPts
#   input$tagPts2
#   user$code
#   input$plotNavBar
#   isolate({ 
#     if(input$plotNavBar=="tagValues") 
#        #|| input$plotNavBar=="dragTag")
#       {
#       tagIndxId<-"tagIndx"
#       tagName<-input$tagPts
#       # tagIndxId<-switch( input$plotNavBar,
#       #                  tagValues="tagIndx",
#       #                  dragTag="tagIndx2"
#       # )
#       # tagName<-switch( input$plotNavBar,
#       #                    tagValues=input$tagPts,
#       #                    dragTag=input$tagPts2
#       # )
#       tagRList<-NULL
#       df<-NULL
#       tagIndxChoices<-NULL
#       tagName<-input$tagPts
#       if(!is.null(tagName)){
#         tagRList<-getPtDefs()$df
#       }
#       if( !is.null(tagRList)){
#         df<-tagRList[[tagName]]
#       }
#       if( !is.null(df)){
#         tagIndxChoices<-df[["tag"]]
#       }
#       if(!is.null(tagIndxChoices)){
#         pt.indx<-max(1,selectedPoint$point.index)
#         selectedTagIndx<-max(tagIndxChoices[ tagIndxChoices<= pt.indx])
#         if(selectedPoint$point.index>0){
#           selectedPoint$point.index<-selectedTagIndx
#         }
#         updateSelectInput(session, tagIndxId,
#                           choices=tagIndxChoices,
#                           selected=selectedTagIndx
#         )
#         updateSelectInput(session, "ptRSelect",
#                           choices=names(getPtDefs()$pts),
#                           selected=tagName
#         )
#       } else {
#         updateSelectInput(session, tagIndxId,
#                           choices=list(),
#                           selected=NULL
#         )
#       }
#     }
#   })
# })
# 
# # -----------ACTIVE TAG COL------------------------
# # observers tagPts
# # sets tagCol
# 
# observe({
#   input$tagPts
#   #input$tagPts2
#   user$code
#   input$plotNavBar
#   isolate({
#     if(input$plotNavBar=="tagValues"){
#       tagName<-input$tagPts
#       if(!is.null(tagName)){
#         tagRList<-
#         df<-tagRList[[tagName]]
#         tagColChoices<-setdiff(names(df),"tag")
#         tagColChoice<-input$tagCol
#         if(length(tagColChoices)>0){
#           tagColChoices<-sort(tagColChoices)
#           if( length(tagColChoice)==0 || 
#               !(tagColChoice %in% tagColChoices ) ){
#             tagColChoice<-tagColChoices[length(tagColChoices)]
#           }
#           updateSelectInput(session, "tagCol",
#                             choices=tagColChoices, selected=tagColChoices)
#         } else { #hide it
#           updateSelectInput(session, "tagCol",
#                             choices=list(), selected=NULL)
#         }
#       } 
#     }
#   })
# })
# 
# # ----------tagIndx----------------------------------
# # observe({
# #   input$tagIndx2
# #   isolate({
# #     if( input$plotNavBar=="dragTag"){
# #       tagIndx<-as.numeric(input$tagIndx2)
# #     } 
# #   })
# # })
# 
# # -----------ACTIVE TAG VALUE------------------------
# 
# observe({ 
#   tagPtName<-input$tagPts
#   tagIndx<-as.numeric(input$tagIndx)
#   tagCol<- input$tagCol
#   user$code
#   isolate({ 
#     if( input$plotNavBar=="tagValues"){
#       if(length(tagCol)>0){ #or not NULL
#         tagRList<-getPtDefs()$df
#         
#         df<-tagRList[[tagPtName]]
#         choices<-sort(unique(df[[tagCol]]))
#         value<-subset(df,df$tag==tagIndx)[[tagCol]]
#         updateSelectInput(session, "tagColVal",
#                           choices=choices, selected=value )
#       } else {
#         updateSelectInput(session, "tagColVal",
#                           choices=list(), selected=NULL )
#       }
#     }
#   })
# })
# 

#  If the user changes the tag freq selection
#  Need to update reactiveTag$freq

observe({
  input$tagFreq
  isolate({
    if(is.null(getPtDefs()$pts) || is.null(input$ptRSelect)) { return() }
    ptNames<-names(getPtDefs()$pts)
    freq<-reactiveTag$freq
    freq<-lapply( ptNames, function(n)freq[[n]] )
    value<-input$tagFreq
    if( value=="Off"){
      value<-NULL
    } else { 
      #selection<-input$ptRSelect
      selection<-selectedPoint$name

      tagList<-getPtDefs()$df
      if(!is.null(tagList) && !is.null(tagList[[selection]])){
        #get the last tagged element and iterate the tagging
        dn<-as.integer(value)
        df<-tagList[[selection]]
        df1<-tail(df,1)
        n1<-df1$tag
        ptList<-getPtDefs()$pts
        n2<-length(ptList[[selection]])/2
        if(n2>n1){
          s<-seq(from=n1,to=n2,by=dn)
          s<-s[-1]
          if(length(s)>0){
            df2List<-lapply(s, function(tn){ df2<-df1; df2$tag<-tn; df2})
            df3<-do.call(rbind, df2List )
            df4<-rbind(df,df3)
            tagList[[selection]]<-df4
            src<-user$code
            src<-df2Source(src,dfList = tagList)
            user$code<-src 
          }
        }
      }
    }
    freq[[selectedPoint$name]]<-value
    reactiveTag$freq<-freq
  })  
})



#-------------------------------------------------------------------

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



observe({
  user$code
  #input$plotNavBar
  tagName<-selectedPoint$name
  tagIndx<-selectedPoint$point.index
  if(input$plotNavBar=="tagValues"){
    isolate({
      print("plotBar or user$code change")
      
      tagNameChoices<-getTagNameChoices() #uses getPtDefs()
      
      tagName<-exGetTagName( tagNameChoices, tagName)
      if(length(tagName)>0){
        selectedPoint$name<-tagName #here is where the potential issue arrises
      }
      updateSelectInput(session, "tagName", choices=tagNameChoices, selected=tagName)
      
      df<-getPtDefs()$df[[tagName]]
      tagIndxChoices<-df$tag
      #print(tagIndx)
      tagIndx<-exGetTagIndx(tagIndxChoices, tagIndx)
      #print(tagIndx)
      if( length(tagName)>0 && length(tagIndx>0)){
        selectedPoint$point.index<-tagIndx 
      }
      updateSelectInput(session, "tagIndx", choices=tagIndxChoices, selected=tagIndx)
    })
  }
 })
 
observe({
  if(input$plotNavBar=="tagValues"){
    input$tagName
    isolate({
    print("input$tagName change")
      if(!is.null(input$tagName)){
        selectedPoint$name<-input$tagName
      }
    })  
  }
})

observe({
  if(input$plotNavBar=="tagValues"){
    input$tagIndx
    isolate({
      print("input$tagIndx change")
      if(!is.null(input$tagIndx)){
        selectedPoint$point.index<-input$tagIndx
      }
    })
  }
})


observe({
  if(input$plotNavBar=="tagValues"){
    input$tagIndx
    isolate({
      print("tagIndx change")
      #tagColSelection   
      df<-getPtDefs()$df[[selectedPoint$name]]
      tagColChoices<-setdiff(names(df),"tag") #choices
      tagColChoice<-exGetTagColChoice(tagColChoices, input$tagCol)
      updateSelectInput(session, "tagCol", choices=tagColChoices, selected=tagColChoice)
    })
  }
})

observe({
  if(input$plotNavBar=="tagValues"){
    tagColChoice<-input$tagCol
    isolate({
      print("tagCol change")
      tagIndx<-selectedPoint$point.indx #input$tagIndx
      df<-getPtDefs()$df[[selectedPoint$name]]
      #tagValueSelection
      if(!is.null(tagColChoice)){
        tagValueChoices<-df[[tagColChoice]]
      } else {
        tagValueChoices<-NULL
      }
      if(length(tagValueChoices)>0 && !is.null(tagIndx) ){
        #tagIndx<-as.numeric(input$tagIndx)
        tagValueChoice<-subset(df,df$tag==tagIndx)[[tagColChoice]]
      } else {
        tagValueChoice<-NULL
      }
      updateSelectInput(session, "tagColVal", choices=tagValueChoices, selected=tagValueChoices)
    })
  }
})


