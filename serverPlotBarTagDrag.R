

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