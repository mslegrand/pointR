#output$TagValuesPanel<-renderUI({
#  conditionalPanel( "input.plotNavBar=='tagValues'", 
#    absolutePanel( top=50, left=0, width=650, draggable=TRUE, 
#      style=cstyle$wellPoint,
#      fluidRow(
#        column(2, 
#          selectInput( "tagName", "Point Matrix",
#          multiple=FALSE, size=3, selectize = FALSE,
#          list(),  selected=NULL, width="100px"  )
#        ),
#        column(2, 
#          selectInput("tagIndx", "Tag-No",
#          multiple=FALSE, size=3, selectize = FALSE, 
#          list(), selected=NULL, width="60px"  )
#        ),
#        column(2, 
#          selectInput("tagCol", "Col-Name",
#          multiple=FALSE, size=3, selectize = FALSE, 
#          list(),  selected=NULL, width="100px"  )
#        ),
#        column(3, 
#          selectInput("tagColVal", "Col-Value", 
#          multiple=FALSE, size=3, selectize = FALSE,  
#          list(),  selected=NULL, width="100px"  )
#        ),
#        column(3, 
#          textInput("tagValEd", "Alt-Value", value=""),
#          actionButton("insertVal2Col", label = "Apply Alternate Val", style=cstyle$buttonSmall)
#        )
#      ) #TAGS panel end
#    )
#  ) 
#})



#------------------------------


#  If the user changes the tag freq selection
#  Need to update reactiveTag$freq

#observe({
#  input$tagFreq
#  isolate({
#    if(is.null(getPtDefs()$pts) || is.null(input$ptRSelect)) { return() }
#    ptNames<-names(getPtDefs()$pts)
#    freq<-reactiveTag$freq
#    freq<-lapply( ptNames, function(n)freq[[n]] )
#    value<-input$tagFreq
#    if( value=="Off"){
#      value<-NULL
#    } else { 
#      #selection<-input$ptRSelect
#      selection<-selectedPoint$name
#
#      tagList<-getPtDefs()$df
#      if(!is.null(tagList) && !is.null(tagList[[selection]])){
#        #get the last tagged element and iterate the tagging
#        dn<-as.integer(value)
#        df<-tagList[[selection]]
#        df1<-tail(df,1)
#        n1<-df1$tag
#        ptList<-getPtDefs()$pts
#        n2<-length(ptList[[selection]])/2
#        if(n2>n1){
#          s<-seq(from=n1,to=n2,by=dn)
#          s<-s[-1]
#          if(length(s)>0){
#            df2List<-lapply(s, function(tn){ df2<-df1; df2$tag<-tn; df2})
#            df3<-do.call(rbind, df2List )
#            df4<-rbind(df,df3)
#            tagList[[selection]]<-df4
#            src<-user$code
#            src<-df2Source(src,dfList = tagList)
#            user$code<-src 
#          }
#        }
#      }
#    }
#    freq[[selectedPoint$name]]<-value
#    reactiveTag$freq<-freq
#  })  
#})



#-------------------------------------------------------------------

#exGetTagColChoice<-function(tagColChoices, currentChoice){
#  if(length(tagColChoices)>0){
#    tagColChoice<-currentChoice
#    tagColChoices<-sort(tagColChoices)
#    if( length(tagColChoice)==0 || 
#        !(tagColChoice %in% tagColChoices ) ){
#      tagColChoice<-tagColChoices[length(tagColChoices)]
#    }
#  } else { #hide it
#    tagColChoice<-NULL
#  }
#  tagColChoice
#}
#
#
#
#observe({
#  user$code
#  #input$plotNavBar
#  tagName<-selectedPoint$name
#  tagIndx<-selectedPoint$point.index
#  if(input$plotNavBar=="tagValues"){
#    isolate({
#      #print("plotBar or user$code change")
#      
#      tagNameChoices<-getTagNameChoices() #uses getPtDefs()
#      
#      tagName<-exGetTagName( tagNameChoices, tagName)
#      if(length(tagName)>0){
#        selectedPoint$name<-tagName #here is where the potential issue arrises
#      }
#      updateSelectInput(session, "tagName", choices=tagNameChoices, selected=tagName)
#      
#      df<-getPtDefs()$df[[tagName]]
#      tagIndxChoices<-df$tag
#      #print(tagIndx)
#      tagIndx<-exGetTagIndx(tagIndxChoices, tagIndx)
#      #print(tagIndx)
#      if( length(tagName)>0 && length(tagIndx>0)){
#        selectedPoint$point.index<-tagIndx 
#      }
#      updateSelectInput(session, "tagIndx", choices=tagIndxChoices, selected=tagIndx)
#    })
#  }
# })
# 
#observe({
#  if(input$plotNavBar=="tagValues"){
#    input$tagName
#    isolate({
#    #print("input$tagName change")
#      if(!is.null(input$tagName)){
#        selectedPoint$name<-input$tagName
#      }
#    })  
#  }
#})
#
#observe({
#  if(input$plotNavBar=="tagValues"){
#    input$tagIndx
#    isolate({
#      #print("input$tagIndx change")
#      if(!is.null(input$tagIndx)){
#        selectedPoint$point.index<-input$tagIndx
#      }
#    })
#  }
#})
#
#
#observe({
#  if(input$plotNavBar=="tagValues"){
#    input$tagIndx
#    isolate({
#      #print("tagIndx change")
#      #tagColSelection   
#      df<-getPtDefs()$df[[selectedPoint$name]]
#      tagColChoices<-setdiff(names(df),"tag") #choices
#      tagColChoice<-exGetTagColChoice(tagColChoices, input$tagCol)
#      updateSelectInput(session, "tagCol", choices=tagColChoices, selected=tagColChoice)
#    })
#  }
#})
#
#observe({
#  if(input$plotNavBar=="tagValues"){
#    tagColChoice<-input$tagCol
#    isolate({
#      #print("tagCol change")
#      tagIndx<-selectedPoint$point.indx #input$tagIndx
#      df<-getPtDefs()$df[[selectedPoint$name]]
#      #tagValueSelection
#      if(!is.null(tagColChoice)){
#        tagValueChoices<-df[[tagColChoice]]
#      } else {
#        tagValueChoices<-NULL
#      }
#      if(length(tagValueChoices)>0 && !is.null(tagIndx) ){
#        #tagIndx<-as.numeric(input$tagIndx)
#        tagValueChoice<-subset(df,df$tag==tagIndx)[[tagColChoice]]
#      } else {
#        tagValueChoice<-NULL
#      }
#      updateSelectInput(session, "tagColVal", choices=tagValueChoices, selected=tagValueChoices)
#    })
#  }
#})



#----------------------------------------------------------------
  showPts.valTag%<c-% function(ptName, pts, selectedPointIndx, ptDisplayMode,  ptTags=NULL){
    #cat(file=stderr(),"entering drag.Tag\n")
    if( is.null(pts) ){return(NULL)} 
    if(length(pts)<2){return(NULL)}
    #print("entering val.Tag\n")
    tag.indx<-selectedPointIndx
    semitransparent<-0.3
    colorScheme<-c(default="green", ending="red", selected="blue")
    
    m<-matrix(pts,2)
    if( is.null(tag.indx) ){ stop("unexpected null for tag.indx") }
    if( is.null(ptTags)   ){ stop("ptTags is null") }

    tags<-ptTags$tag
    ti<-which(tag.indx==tags) 
    tagInterval<-findInterval(sequence(ncol(m)),tags)
    tagInterval<-tagInterval==ti
    tagInterval[tagInterval==0]<-semitransparent
    opac<-tagInterval
      
    # iterate over tagIntList
    lapply(1:ncol(m), function(i){
      id<-paste("pd",ptName,i,sep="-")
      pt<-m[,i]
      color=colorScheme['default']
      if(i==length(pts)/2) { #ncol(m)){
        color=colorScheme['ending']   
      }
      list(
        circle(class="draggable", 
          id=id,  
          cxy=pt, r=8, fill=color, opacity=opac[i],
          transform="matrix(1 0 0 1 0 0)", onmousedown="selectPoint(evt)" 
        ),
        if(ptDisplayMode=="Labeled"){
          text(paste(i), cxy=pt+10*c(1,-1),  stroke='black', font.size=12, opacity=opac[i]) #opac)
        } else {
          NULL
        }
      )
    }) #end lapply
  } #end showPts



