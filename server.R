
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(stringr)
library(svDialogs)

#options(shiny.error = recover)

#----begin external rc------------
source("format.R")
source("ptrUtil.R")
source("trUtils.R")

defTag<-"ptR"

js.scripts<-list(
  Points=readFile("www/pointsIO.js"),
  Translate=readFile("www/transIO.js"),
  Rotate=readFile("www/rotIO.js"),
  Scale=readFile("www/scaleIO.js")
)


ex.getPtDefs<-function(src){
  ptDefs<-NULL
  #defTag<-"Pts" #ptDefs"
  if(grepl(defTag,src)==TRUE ){
    try({
      ptDefTxt<-getDef(src, defTag=defTag)
      eval(parse(text=ptDefTxt))
      ptDefs<-get(defTag)
    })
  }
  ptDefs
}


pts2Source<-function(txt,ptDefs){
  # fPtDefs<-sapply(ptDefs, formatPts)
  # tmp<-paste0("  ",names(fPtDefs),"=",fPtDefs,collapse=",\n")
  # replacement<-paste0(defTag,"<-list(\n",tmp,"\n)")
  replacement<-formatPtDefs(defTag=defTag, ptDefs=ptDefs)
  txt<-replaceDef(txt, replacement, defTag=defTag) 
}


# called by either a newloaded source
# or upon a commit
ex.getSelectInfo<-function(ptDefs, selected, point.index){
  choices<-names(ptDefs)
  #cat("length(choices)=",length(choices),"\n")
  if(length(choices)==0){
    rtv<-list(selected=NULL, point.index =0 )  
    return(rtv)
  }
  if(!(selected %in% choices)){ # a new choice
    #pick the first choice candidate
    N<-1
    rtv<-list(
      selected=choices[N],
      point.index=length(ptDefs[[N]])/2
    )
    return(rtv)
  }
  #default: an existing choice
  point.index=max(point.index, length( ptDefs[[selected]])/2 )
  rtv<-list(selected=selected, point.index=point.index)
  return(rtv)  
}

#----end external ------------

#---begin server--------------

shinyServer(function(input, output,session) {
 
# Reactive values----------
  
  user <- reactiveValues( code=codeTemplate )   #  internal copy of user code
  file<-reactiveValues( name="newFile.R")       #  file path
  selectedPoint<-reactiveValues(index=0)        #  index selection of point array
  init<-reactiveValues(val=0)                   #  kludge for initialization (shouldn't need this)
# Reactive expressions------------- 
  getPtDefs<-reactive({ ex.getPtDefs(user$code) })  #extract points from user code

#   getPtArray<-reactive(
#     ex.getPts( user$code, input$ptSet ) #not used anymore
#   )
  
 


# Event Observers--------------------------------  
  observeEvent(
    tmp<-init$val,{
      isolate(
        updateAceEditor( session,"source", value=user$code)
      ) 
    }
  )

  # set index on change of point set selection
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
  
  # forward button (selected point forward)
  observeEvent(input$forwardPt,{
    selection<-input$ptSet
    ptDefs<-getPtDefs()
    len<-length(ptDefs[[selection ]])/2
    selectedPoint$index<-min(len, selectedPoint$index+1)
  })
  
  # back button (selected point backward)
  observeEvent(input$backwardPt,{
    #decrement selectedPointIndex
    selection<-input$ptSet
    ptDefs<-getPtDefs()
    len<-length(ptDefs[[selection ]])/2
    if(len>0){
        selectedPoint$index<-max(1,selectedPoint$index-1)
    } else {
        selectedPoint$index<-0
    }
  })
  
#observers --------------------------

#---navbarMenuBar--------
observeEvent( input$fileNavBar, { 
  fileCmd<-input$fileNavBar
  
  if(fileCmd=="New"){ #-----new
    txt<-codeTemplate
    user$code<-codeTemplate
    # the next  line update the ptDefs; probably should redo with observer
    file$name<-"newSVG.R"
    selectedPoint$index<-0
    isolate(
      updateAceEditor( session,"source", value=txt)
    ) 
    updateSelectInput(session, "ptSet", label = "Selected Pt Vec Def", choices=c("x"), selected=NULL ) 
    #updateNavbarPage(session, "fileNavBar", selected =NULL)  
  }
  if(fileCmd=="Open"){ #-----open 
    fileName=""
    try(fileName<-dlgOpen(title = "Select one R file", 
                          filters = dlgFilters[c("R", "All"), ])$res)
    if(length(fileName)>0){ 
      src<-paste(readLines(fileName), collapse = "\n")
      file$name<-fileName
      if(nchar(src)>0){
        user$code<-src 
        point.index<-selectedPoint$index
        selected<-input$ptSet
        ptDefs<-getPtDefs()
        #cat("names(ptDefs)=",paste(names(ptDefs),collapse=", "),"\n")
        res<-ex.getSelectInfo(ptDefs, selected, point.index)
        selectedPoint$index<-res$point.index
        updateSelectInput(session, "ptSet", label = "Selected Pt Vec Def", choices=names(ptDefs), selected=res$selected ) 
        updateAceEditor( session,"source", value=src)
      }
    }
    #updateNavbarPage(session, "fileNavBar", selected =NULL)
  }
  if(fileCmd=="Save"){ #-----save
    fileName=""
    default="newfile.R"
    try(fileName<-dlgSave(title = "Save R script to", 
                          filters = dlgFilters[c("R", "All"), ])$res
    )
    if(fileName!=""){ 
      file$name<-fileName
      txt<-user$code
      writeLines(txt, fileName)
      updateAceEditor( session,"source", value=txt)
    }
    #updateNavbarPage(session, "fileNavBar", selected =NULL)
  }
})


#---commit  button----- (update sourceCode with editor contents)
# alternatively can use observeEvent( input$commit, { ... })
  observe({ 
    input$commit
    #get text from editor
    isolate({
      src<-input$source #------ace editor
      if(nchar(src)>0){
        lines<-strsplit(src,"\n")
        lines<-lines[[1]]
        ptRPos<-grep("^\\s*ptR<-",lines)
        svgRPos<-grep("^\\s*svgR\\(",lines)
        Err<-NULL
        if(length(ptRPos)!=1){
          Err<-"Missing ptR list or multiple  ptR lists"
        }
        if(length(svgRPos)!=1){
          Err<-"Missing svgR call or multiple svgR calls"
        }
        if(is.null(Err) & !(ptRPos[1]<svgRPos[1])){
          Err<-"ptR list must come prior to svgR call"
        }
        if(!is.null(Err)){
          session$sendCustomMessage(type='testmessage', message=Err)
          src<-""
        } 
      }
      if(nchar(src)>0){
        user$code<-src
        point.index<-selectedPoint$index
        selected<-input$ptSet
        ptDefs<-getPtDefs()
        res<-ex.getSelectInfo(ptDefs, selected, point.index)
        selectedPoint$index<-res$point.index
        updateSelectInput(session, "ptSet", label = "Selected Pt Vec Def", choices=names(ptDefs), selected=res$selected )  
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
      cmd<-input$mydata[1]
      pt<- input$mydata[2]
      src<-user$code
      #todo: error check???
      
      pt<-eval(parse(text=pt)) #we assume this is an array??
      ptDefs<-getPtDefs()
      if(cmd=='add'){ #---------add point
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
      if(cmd=='move'){ # --------move point
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
      #-------transformations 
      if(cmd=='trans'){ # -- translate
          tid<-input$mydata[3]
          tmp<-input$mydata[2]
          trDefDelta<-formatC(eval(parse(text=tmp)))
          trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2,)" )
          src<-tr2src( src, tid, trDefDelta2 )
      }
      if(cmd=='rotate'){ # ----rotate
        tid<-input$mydata[3]
        tmp<-input$mydata[2]
        trDefDelta<-formatC(eval(parse(text=tmp)))
        trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2,)" )
        src<-tr2src( src, tid, trDefDelta2 )
      } 
      if(cmd=='scale'){ # ----scale
        tid<-input$mydata[3]
        tmp<-input$mydata[2]
        trDefDelta<-formatC(eval(parse(text=tmp)))
        trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2,)" )
        src<-tr2src( src, tid, trDefDelta2 )
      } 
      # update internal user source
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
  WH<-c(600,620)
  if(svgBarCmd=="Points"){
    ptName<-input$ptSet
    ptDefs<-getPtDefs()
    selectedPointIndx<-selectedPoint$index
    scriptName<-"Points"
    #todo use input$pointOption :
    # pointOpt=c("Insert", "Edit","Tag")
    
  } else {
    ptName<-NULL
    if(svgBarCmd=="Transform"){ #Temp kludge for transform)
      scriptName<-input$transformOption
    } else { # just make it any thing for now
      scriptName<-"Points"
    }
  }
  showGrid<-input$showGrid
  
  script2<-js.scripts[[ scriptName]]
  src<-user$code
  src<-usingDraggable(src)
  
  
  showPts %<c-% function(ptName){
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
  
  newPtLayer %<c-% function(svgBarCmd, wh=c(1200,800)){
    if(svgBarCmd=="Points" ){
      rect(xy=c(0,0), wh=wh, fill="#ADADFF", stroke='black', opacity=.0, onmousedown="newPoint(evt)")
    } else {
      NULL
    } 
  }
    
  boundingBox %<c-% function(){ #not used!!! may consider to use in future
    if(svgBarCmd=="rotate"){
      rect(id='x-bdd-rect', cxy=WH/2, wh=WH/4, stroke='red',fill='none', opacity=.5)
    } else {
      NULL
    }
  }
  
    
  insert.beg<-c( 
    'style(".draggable {','cursor: move;','}"),', 
     gsub('script2', script2, "script('script2'),"),      
    "use(filter=filter(filterUnits=\"userSpaceOnUse\", feFlood(flood.color='white') )),"
  )
  if(showGrid==TRUE){
    insert.beg<-c(insert.beg, "graphPaper( wh=c(1200,1200), dxy=c(50, 50), labels=TRUE ),")
  }
  
  insert.end<-c(
    #paste(',newPtLayer("',svgBarCmd,'"),'),
    ',newPtLayer(svgBarCmd, WH),',
    'showPts(ptName)'
    #boundingBox()
  )    
  
  src<-subSVGX2(src, insert.beg, insert.end)
  svg<-eval(parse(text=src))
  as.character(svg)->svgOut 
  HTML(svgOut)
})

 
})
