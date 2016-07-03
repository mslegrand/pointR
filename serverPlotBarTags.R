
# -----------ACTIVE TAG PT------------------------
#  observes code and plotNavBar
#  sets active Tag and Tag index
#       tagPts
#       tagIndx
#       ptRSelect
#       point.index

observe({
  user$code
  input$plotNavBar
  isolate({
    if(input$plotNavBar=="tagValues"){
      #point.index<-selectedPoint$point.index
      selected   <-input$ptRSelect
      ptRList    <-getPtDefs()$pts
      tagRList   <-getPtDefs()$df
      tagNamechoices    <-intersect(names(ptRList),names(tagRList))
      if(length(tagNamechoices)>0){
        # Use selection of ptRSelect if in choices, ow last avail.
        ptChosen<-input$ptRSelect
        if(ptChosen %in% tagNamechoices){
          tagName<-ptChosen
        } else{
          tagName<-tail(tagNamechoices,1)
          updateSelectInput(session, "ptRSelect", selected=tagName )
        }
        updateSelectInput(session, "tagPts", choices=tagNamechoices, selected=tagName )
      } else {
        updateSelectInput(session, "tagPts", choices=list(), selected=NULL )
      }
    }
  })
})


# -----------ACTIVE TAG INDX ------------------------
# observers tagPts
# sets tagIndx
#     

observe({ 
  input$tagPts
  user$code
  input$plotNavBar
  isolate({ 
    if(input$plotNavBar=="tagValues"){
      tagRList<-NULL
      df<-NULL
      tagIndxChoices<-NULL
      tagName<-input$tagPts
      if(!is.null(tagName)){
        tagRList<-getPtDefs()$df
      }
      if( !is.null(tagRList)){
        df<-tagRList[[tagName]]
      }
      if( !is.null(df)){
        tagIndxChoices<-df[["tag"]]
      }
      if(!is.null(tagIndxChoices)){
        pt.indx<-max(1,selectedPoint$point.index)
        selectedTagIndx<-max(tagIndxChoices[ tagIndxChoices<= pt.indx])
        if(selectedPoint$point.index>0){
          selectedPoint$point.index<-selectedTagIndx
        }
        updateSelectInput(session, "tagIndx",
                          choices=tagIndxChoices,
                          selected=selectedTagIndx
        )
        updateSelectInput(session, "ptRSelect",
                          choices=names(getPtDefs()$pts),
                          selected=tagName
        )
      } else {
        updateSelectInput(session, "tagIndx",
                          choices=list(),
                          selected=NULL
        )
      }
    }
  })
})

# -----------ACTIVE TAG COL------------------------
# observers tagPts
# sets tagCol

observe({
  input$tagPts
  user$code
  input$plotNavBar
  isolate({
    if(input$plotNavBar=="tagValues"){
      tagName<-input$tagPts
      if(!is.null(tagName)){
        tagRList<-getPtDefs()$df
        df<-tagRList[[tagName]]
        tagColChoices<-setdiff(names(df),"tag")
        tagColChoice<-input$tagCol
        if(length(tagColChoices)>0){
          tagColChoices<-sort(tagColChoices)
          if( length(tagColChoice)==0 || 
              !(tagColChoice %in% tagColChoices ) ){
            tagColChoice<-tagColChoices[length(tagColChoices)]
          }
          updateSelectInput(session, "tagCol",
                            choices=tagColChoices, selected=tagColChoices)
        } else { #hide it
          updateSelectInput(session, "tagCol",
                            choices=list(), selected=NULL)
        }
      } 
    }
  })
})



# -----------ACTIVE TAG VALUE------------------------

observe({ 
  tagPtName<-input$tagPts
  tagIndx<-as.numeric(input$tagIndx)
  tagCol<- input$tagCol
  user$code
  isolate({ 
    if( input$plotNavBar=="tagValues"){
      if(length(tagCol)>0){ #or not NULL
        tagRList<-getPtDefs()$df
        
        df<-tagRList[[tagPtName]]
        choices<-sort(unique(df[[tagCol]]))
        value<-subset(df,df$tag==tagIndx)[[tagCol]]
        updateSelectInput(session, "tagColVal",
                          choices=choices, selected=value )
      } else {
        updateSelectInput(session, "tagColVal",
                          choices=list(), selected=NULL )
      }
    }
    
  })
})


#  If the user changes the tag freq selection
#  Need to update reactiveTag$freq

observe({
  input$tagFreq
  isolate({
    ptNames<-names(getPtDefs()$pts)
    freq<-reactiveTag$freq
    freq<-lapply(ptNames, function(n)freq[[n]])
    value<-input$tagFreq
    if(value=="Off"){
      value<-NULL
    } else { 
      selection<-input$ptRSelect
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
    freq[[input$ptRSelect]]<-value
    reactiveTag$freq<-freq
  })  
})

