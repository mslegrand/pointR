
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(stringr)

#options(shiny.error = recover)

#----begin external rc------------

source("ptrUtil.R")
source("trUtils.R")


js.scripts<-list(
  points=readFile("pointsIO.js"),
  translate=readFile("transIO.js"),
  rotate=readFile("rotIO.js")
)

ex.getPts<-function(src, selection){
  pts<-NULL
  if(grepl("ptDefs",src)==TRUE & !is.null(selection)){
    try({
      ptDefTxt<-getDef(src, defTag="ptDefs")
      eval(parse(text=ptDefTxt))
      pts<-pts[[selection]]
    })
  }
  pts
}

ex.getPtDefs<-function(src){
  ptDefs<-NULL
  if(grepl("ptDefs",src)==TRUE ){
    try({
      ptDefTxt<-getDef(src, defTag="ptDefs")
      eval(parse(text=ptDefTxt))
    })
  }
  ptDefs
}


pts2Source<-function(txt,ptDefs){
  fPtDefs<-sapply(ptDefs, formatPts)
  tmp<-paste0("   ",names(fPtDefs),"=",fPtDefs,collapse=",\n")
  replacement<-paste0("ptDefs<-list(\n",tmp,"\n)")
  txt<-replaceDef(txt, replacement, defTag="ptDefs") 
}

#----end external rc------------

#---begin server--------------

shinyServer(function(input, output,session) {
 
# Reactive values----------
  
  user <- reactiveValues(
              code=codeTemplate
            ) 
  
  file<-reactiveValues( name="newFile.R")
  panel<-reactiveValues(E.type="ptSet") #can we eliminate this???
  #  index selection of point array
  selectedPoint<-reactiveValues(index=0)
  
 
# Reactive expressions------------- 
  getPtDefs<-reactive({
    ex.getPtDefs(user$code)
  })

  
  getPtArray<-reactive(
    ex.getPts( user$code, input$ptSet )
  )
  
# Event Observers--------------------------------  
  
  # set index on change of point vector selection
  observeEvent( input$ptSet, {
    ptDefs<-getPtDefs()
    tmp<-length(ptDefs) 
      
      if(tmp<1 || is.null(ptDefs[[1]])){
        selectedPoint$index<-0
      } else {
        selectedPoint$index<-length(ptDefs[[input$ptSet]])/2
      }
  })
  
  #---remove last point  button-----
  observeEvent( input$removePt, {
    selection<-input$ptSet
    if(selection!=""){
      ptDefs<-getPtDefs()
      tmp<-ptDefs[[selection]]
      indx=selectedPoint$index 
      if(indx>=1){
        tmp<-tmp[-c(2*indx,2*indx-1)]
        selectedPoint$index<-selectedPoint$index-1
      } else {
        tmp<-NULL
        selectedPoint$index<-0
      }
      
      ptDefs[[selection]]<-tmp
      if(length(ptDefs)==0){
        ptDefs<-list(x=c())
      }   
      src<-user$code
      src<-pts2Source(src,ptDefs)
      user$code<-src
      updateAceEditor( session,"source", value=src)
    }
  })
  
  # forword button
  observeEvent(input$forwardPt,{
    selection<-input$ptSet
    ptDefs<-getPtDefs()
    len<-length(ptDefs[[selection ]])/2
    selectedPoint$index<-min(len, selectedPoint$index+1)
  })
  
  # back button
  observeEvent(input$backwardPt,{
    #decrement selectedPointIndex
    selectedPoint$index<-max(1,selectedPoint$index-1)
  })
  
#observers --------------------------
  
#---fileNavBar ------- (file io)
  observe({
    #input$fileNavBar
    fileBarCmd<-input$fileNavBar
    if(fileBarCmd=="newSource"){
      txt<-codeTemplate
      user$code<-codeTemplate
      # the next  line update the ptDefs; probably should redo with observer
      file$name<-"newSVG.R"

      isolate(
         updateAceEditor( session,"source", value=txt)
      )  
      updateNavbarPage(session, "fileNavBar", selected ="edit")
    }
    if(fileBarCmd=="open"){
      fileName=""
      try(fileName<-file.choose(), silent=TRUE)
      if(fileName!=""){ 
        txt<-paste(readLines(fileName), collapse = "\n")
        file$name<-fileName
        updateAceEditor( session,"source", value=txt)
        #probably should update accordingly
      }
      updateNavbarPage(session, "fileNavBar", selected ="edit")
    }
    if(fileBarCmd=="save"){
      fileName=""
      try(fileName<-file.choose(new=TRUE), silent=TRUE)
      if(fileName!=""){ 
        file$name<-fileName
        txt<-user$code
        writeLines(txt, fileName)
        updateAceEditor( session,"source", value=txt)
      }
      updateNavbarPage(session, "fileNavBar", selected ="edit")
    }
  })

#---svgNavBar------- (svg io)
observe({
  input$svgNavBar
  panel$E.type<-input$svgNavBar
})


#---commit  button----- (update sourceCode with editor contents)
  observe({ 
    input$commit
    #get text from editor
    
    isolate({
      src<-input$source
      if(nchar(src)>0){
        user$code<-src
        ptDefs<-getPtDefs()
        lenptDefs<-length(ptDefs)
        tmp<-length(ptDefs) 
        choices<-names(ptDefs)
        if(tmp<1 || is.null(ptDefs[[1]])){
          selectedPoint$index<-0
        } else {
          spi<-as.numeric(selectedPoint$index)
          if( spi<1 ){
            selectedPoint$index<-length(ptDefs[[input$ptSet]])/2
          }
        }
        updateSelectInput(session, "ptSet", label = "Selected Pt Vec Def", choices=choices )
        }
    })   
  })


#---mouse click--------
  #todo: onmove get the new postion and update
observe({
  input$mydata #may want to rename this
  isolate(
    if(length(input$mydata)>0){
      #get cmd
      #svgMsg<-input$mydata #debug
      #lapply(1:length(svgMsg), function(i){ cat("svgMsg[",i,"]=", svgMsg[i],"\n")})#debug code
      cmd<-input$mydata[1]
      pt<-input$mydata[2]
      src<-user$code
      #todo: error check
      
      pt<-eval(parse(text=pt)) #we assume this is an array??
      
      ptDefs<-getPtDefs()
      if(cmd=='add'){ #add point
        newPt<-pt
        #get selection
        selection<-input$ptSet
        #update local ptDefs
        indx<-selectedPoint$index
        ptDefs[[selection]]<-append(ptDefs[[selection]],newPt,2*indx) 
        #update point values
        selectedPoint$index<-selectedPoint$index+1
        src<-pts2Source(src,ptDefs)
      } 
      if(cmd=='move'){ # move point
        id<-input$mydata[3]
        vid<-strsplit(id,"-")[[1]]
        #get selection
        selection<-vid[2]
        #get point index
        indx<-2*as.numeric(vid[3])-1
        #reassign point
        ptDefs[[selection]][indx:(indx+1)]<-pt
        #update point values
        src<-pts2Source(src,ptDefs)
      }
      #transformations
      if(cmd=='trans'){ # translate
          tid<-input$mydata[3]
          tmp<-input$mydata[2]
          trDefDelta<-formatC(eval(parse(text=tmp)))
          trDefDelta2<-paste0("matrix(",paste0(trDefDelta,collapse=" "),")")
          trDefDelta2<-paste0(trDefDelta,collapse=" ")
          src<-tr2src( src, tid, trDefDelta2 )
      }
      if(cmd=='rotate'){ # rotate
        tid<-input$mydata[3]
        tmp<-input$mydata[2]
        trDefDelta<-formatC(eval(parse(text=tmp)))
        trDefDelta2<-paste0("matrix(",paste0(trDefDelta,collapse=" "),")")
        trDefDelta2<-paste0(trDefDelta,collapse=" ")
        src<-tr2src( src, tid, trDefDelta2 )
      }
                  
      # update internal source
      user$code<-src
      #update editor
      isolate( #no dependency on editor
        updateAceEditor( session,"source", value=src)
      )
    }
  )
})

# Output------------------------------------
#------fileName-------------
  output$fileName <- renderText({ 
    fileName<-file$name
    if(is.null(fileName) ){
      fileName==""
    }
    paste("Editing", basename(fileName))
  })
  
  
#----svg window-------------------
output$svghtml <- renderUI({
  svgBarCmd<-input$svgNavBar
  if(svgBarCmd=="points"){
    ptName<-input$ptSet
    ptDefs<-getPtDefs()
    selectedPointIndx<-selectedPoint$index
  } else {
    ptName<-NULL
  }

  script2<-js.scripts[[ svgBarCmd ]]
  src<-user$code
  src<-usingDraggable(src)
  
  svgX<-function(...){
  
    graphPaper<-function(wh=c(600,600), dxy=c(50, 50), labels=TRUE ){
      seq(0,wh[1],dxy[1])->xs
      seq(0,wh[2],dxy[2])->ys
      grph<-c(
        lapply(xs, function(x)line(xy1=c(x,0),xy2=c(x,wh[2]))),
        lapply(ys, function(y)line(xy1=c(0,y),xy2=c(wh[1],y)))
      )
      if(labels){
        grph<-c(grph, 
                lapply(xs, function(x)text(xy=c(x+2,10),x)),
                lapply(ys, function(y)text(xy=c(2,y),y))
        )
      }
      g( stroke.width=1,
         font.size=10,
         stroke="grey",
         grph
      )       
    }
    
    showPts<-function(ptName){
      ptDefs<-getPtDefs()
      if(is.null(ptName)){
        return(NULL)
      }
      pts<- p <- ptDefs[[ptName]]
      #pts<-getPtArray()
      if(length(pts)<2){
        return(NULL)
      } else{
        m<-matrix(pts,2,)
        lapply(1:ncol(m), function(i){
          id<-paste("pd",ptName,i,sep="-")
          pt<-m[,i]
          color='red'
          if(i==selectedPointIndx){
            color='green'
          } else{
            if(i==length(pts)/2) { #ncol(m)){
              color='orange'
            }
          } 
          circle(class="draggable", 
                 id=id,  
                 cxy=pt, r=8, fill=color, 
                 transform="matrix(1 0 0 1 0 0)", 
                 onmousedown="selectPoint(evt)" )
        })
      }
    }
    newPtLayer<-function(){
      if(svgBarCmd=="points" ){
        rect(xy=c(0,0), wh=WH, fill="#ADADFF", stroke='black', opacity=.0, onmousedown="newPoint(evt)")
      } else {
        NULL
      } 
    }
    
    boundingBox<-function(){
      if(svgBarCmd=="rotate"){
        rect(id='x-bdd-rect', cxy=WH/2, wh=WH/4, stroke='red',fill='none', opacity=.5)
      } else {
        NULL
      }
    }
    
    svgR( style(".draggable {
             cursor: move;
          }"),
          script( script2  ),      
          # background
          use(
            filter=filter( feFlood(flood.color='white') )
          ),
          graphPaper(wh=WH),
          ...,
          newPtLayer(),
          showPts(ptName),
          boundingBox()
    )  
  }
  

  src<-gsub("svgR","svgX",src)
  
  svg<-eval(parse(text=src))
  as.character(svg)->svgOut 
  HTML(svgOut) 
})

 
})
