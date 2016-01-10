
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
#----end external rc------------

getPts<-function(src, selection){
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


#---begin server--------------
shinyServer(function(input, output,session) {
  
  # should these be split up???
  values <- reactiveValues(
              ptDefs=list(x=c()),
              sourceCode=codeTemplate
            ) 
  
  file<-reactiveValues( name="newFile.R")
  panel<-reactiveValues(E.type="ptSet") #can we eliminate this???
  
  source2Pts<-reactive({
    ptDefs<-NULL
    if(grepl("ptDefs",values$sourceCode)==TRUE){
      try({
        ptDefTxt<-getDef(values$sourceCode, defTag="ptDefs")
        eval(parse(text=ptDefTxt))
      })
    } 
  })
  
  getPtArray<-reactive(
    getPts(values$sourceCode, input$ptSet)
  )
  
#  index selection of point array
  selectedPoint<-reactiveValues(index=0)
  
  

  # set index on change of point vector selection
  observeEvent( input$ptSet,{
      ptDefs<-values$ptDefs
      tmp<-length(ptDefs) 
      
      if(tmp<1 || is.null(ptDefs[[1]])){
        selectedPoint$index<-0
      } else {
        selectedPoint$index<-length(ptDefs[[input$ptSet]])
      }
  })

#---fileNavBar -------
  observe({
    #input$fileNavBar
    fileBarCmd<-input$fileNavBar
    if(fileBarCmd=="newSource"){
      txt<-codeTemplate
      values$sourceCode<-codeTemplate
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
        txt<-values$sourceCode
        writeLines(txt, fileName)
        updateAceEditor( session,"source", value=txt)
      }
      updateNavbarPage(session, "fileNavBar", selected ="edit")
    }
  })

#---svgNavBar-------
observe({
  input$svgNavBar
  panel$E.type<-input$svgNavBar
})

#Title above editor
  output$editTitle <- renderText({ 
    fileName<-file$name
    if(is.null(fileName) ){
      fileName==""
    }
    paste("Editing", fileName)
  })

#---remove last point  button-----
observeEvent( input$removePt,{
    selection<-input$ptSet
    if(selection!=""){
      ptDefs<-values$ptDefs
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
      src<-values$sourceCode
      src<-pts2Source(src,ptDefs)
      values$sourceCode<-src
      values$ptDefs<-ptDefs
      updateAceEditor( session,"source", value=src)
    }
})

# forword button
observeEvent(input$forwardPt,{
  selection<-input$ptSet
  ptDefs<-values$ptDefs
  len<-length(ptDefs[[selection ]])/2
  selectedPoint$index<-min(len, selectedPoint$index+1)
})

# back button
observeEvent(input$backwardPt,{
  #decrement selectedPointIndex
    selectedPoint$index<-max(1,selectedPoint$index-1)
})


#---commit  button-----
  observe({ 
    input$commit
    #get text from editor
    
    label<-"Selected Pt Vec Def"
    isolate(src<-input$source)
    #choices<-list("Missing ptDef")
    # if not ptDefs insert?
    
    if(grepl("ptDefs",src)==TRUE){
      try(
        {
        values$sourceCode<-src
        #cat("setting sourceCode\n") #debug
        ptDefTxt<-getDef(src, defTag="ptDefs")
        #cat("\n",ptDefTxt,"\n")
        eval(parse(text=ptDefTxt))
        choices<-names(ptDefs)
        values$ptDefs<-ptDefs
        
        #
        #ptDefs<-values$ptDefs
        tmp<-length(ptDefs) 
#         cat("selectedPoint$index=",selectedPoint$index,"\n")
#         cat("input$ptSet=",input$ptSet,"\n")
#         cat("pointdefs=", paste( ptDefs[[input$ptSet]], collapse=", "), "\nß")
#         cat("length(ptDefs[[input$ptSet]]=",length(ptDefs[[input$ptSet]]),"\n" )
        if(tmp<1 || is.null(ptDefs[[1]])){
          selectedPoint$index<-0
        } else {
          if(selectedPoint$index==0){
            selectedPoint$index<-length(ptDefs[[input$ptSet]])/2
          }
        }
        #
        
        
        #cat("before update\n")
        updateSelectInput(session, "ptSet", label = label, choices=choices )
        #cat("after update\n") #debug
        }
      )
    } 
  })


pts2Source<-function(txt,ptDefs)
{
  # update sourceCode with ptDefs
  # format points to insert
  fPtDefs<-sapply(ptDefs, formatPts)
  tmp<-paste0("   ",names(fPtDefs),"=",fPtDefs,collapse=",\n")
  replacement<-paste0("ptDefs<-list(\n",tmp,"\n)")
  txt<-replaceDef(txt, replacement, defTag="ptDefs") 
}


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
      src<-values$sourceCode
      #todo: error check
      
      pt<-eval(parse(text=pt)) #we assume this is an array??
      
      ptDefs<-values$ptDefs #copy ptDefs
      if(cmd=='add'){ #add point
        newPt<-pt
        #get selection
        selection<-input$ptSet
        #update local ptDefs
        indx<-selectedPoint$index
        ptDefs[[selection]]<-append(ptDefs[[selection]],newPt,2*indx) 
        #update point values
        values$ptDefs<-ptDefs
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
        values$ptDefs<-ptDefs
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
        #cat('cmd=rotate\n')
        tid<-input$mydata[3]
        tmp<-input$mydata[2]
        trDefDelta<-formatC(eval(parse(text=tmp)))
        trDefDelta2<-paste0("matrix(",paste0(trDefDelta,collapse=" "),")")
        trDefDelta2<-paste0(trDefDelta,collapse=" ")
        src<-tr2src( src, tid, trDefDelta2 )
      }
      
      
      #update point values
      #values$ptDefs<-ptDefs
            
      # update internal source
      values$sourceCode<-src
      #update editor
      isolate( #no dependency on editor
        updateAceEditor( session,"source", value=src)
      )
    }
  )
})


#----svg window-------------------
output$svghtml <- renderUI({
  svgBarCmd<-input$svgNavBar
  if(svgBarCmd=="points"){
    ptName<-input$ptSet
    source2Pts() #ptDefs<-values$ptDefs
    selectedPointIndx<-selectedPoint$index
  } else {
    ptName<-NULL
  }

  script2<-js.scripts[[ svgBarCmd ]]
  src<-values$sourceCode
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
      source2Pts() 
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
          cat("length(pts)=",length(pts)/2,"\n")
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
