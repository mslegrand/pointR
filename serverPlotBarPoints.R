
#ui
#output$PointsPanel<-renderUI({
#  conditionalPanel( "input.plotNavBar=='Points'", 
#    absolutePanel( top=50, left=0, width=650, draggable=TRUE,
#      style=cstyle$wellPoint,
#      fluidRow(
#        column(4, 
#          selectInput("ptRSelect", "Point Matrix", list("x"), 
#            selected="x", multiple=FALSE,  selectize = FALSE,
#            width="150px",size=1  )
#        ),
#        column(3,
#          selectInput("ptDisplayMode", "Display Mode",
#            list("Normal","Labeled","Hidden"), selected="Normal", 
#            multiple=FALSE, selectize = FALSE,
#            width="150px", size=1 )
#        ),
#        column(2,
#          selectInput("tagFreq", "Auto Tag",
#            c(list("Off"),1:20), selected="Off", 
#            multiple=FALSE, selectize = FALSE,
#            width="80px", size=1  )
#        ),
#        column(3,
#          checkboxInput("insertMode","Insert",value = TRUE, width = "100px"),
#          checkboxInput("showGrid", "Grid",   value = TRUE, width = "100px")
#        )
#      )) #end Points 
#    )
#})







#server
# -----------ACTIVE POINT MATRIX------------------------
#  observes code and plotNavBar
#  sets active Point, point selection,  and selectedPoint$point.index
#observe({
#user$code
#  #input$plotNavBar
#  name<-selectedPoint$name
#  point.index<-selectedPoint$point.index  
#  if(input$plotNavBar=='Points'){
#    isolate({
#      ptRList<-getPtDefs()$pts
#      res<-ex.getSelectInfo(ptRList, name, point.index)
#      selectedPoint$point.index<-res$point.index
#      updateSelectInput(session, "ptRSelect",
#                        choices=names(ptRList),
#                        selected= res$selected )
#    })
#  }
#})
#
 #fires when ptRmatrix changes
#observe({
#  input$ptRSelect
#  if(input$plotNavBar=='Points'){
#    isolate({
#      if(!is.null(input$ptRSelect)){
#        selectedPoint$name<-input$ptRSelect
#        #ptRList<-getPtDefs()$pts
#  #      selectedPoint$point.index<-length(ptRList[[input$ptRSelect]])
#  #      selected=reactiveTag$freq[[input$ptRSelect]] 
#  #      if(is.null(selected)){
#  #        selected<-"Off"
#  #      }
#  #      updateSelectInput(session, "tagFreq", selected=selected )
#      }
#    })
#  }
#})
#
#observe({
#  displayOptions$insertMode<-input$insertMode
#  displayOptions$showGrid<-input$showGrid
#  displayOptions$ptMode<-input$ptDisplayMode
#  #showOptions$showGrid<-input$showGrid
#})
#

# --------------input$plotNavBar=="Points"----------------
#modulePointsBarUI

output$PointsPanel<-renderUI({
  conditionalPanel( "input.plotNavBar=='Points'", modulePointsBarUI("pointsBar"))
})

pointInfoList<-callModule( 
  module=modulePointsBar, 
  id="pointsBar", 
  barName=reactive(input$plotNavBar),
  getSelectInfo=getSelectInfo, #not the best way, but ...
  getPtDefs=getPtDefs, 
  name=getPtName, 
  index=getPtIndex
)

observe({
  name<-pointInfoList$name()
#  isolate({
    if(!is.null(name)){
      selectedPoint$name<-pointInfoList$name()
    }
#  })
})

observe({
  value<-pointInfoList$tagFreq()
  isolate({
    name<-getPtName()
    if(is.null(getPtDefs()$pts) || is.null( name )) { return() }
    ptNames<-names(getPtDefs()$pts)
    freq<-reactiveTag$freq
    freq<-lapply( ptNames, function(n)freq[[n]] )
    if( !is.null(value) && value=="Off"){
      value<-NULL
    } else {
      selection<-getPtName()
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
    freq[[name]]<-value
    reactiveTag$freq<-freq
  })  
})


#-----

showGrid<-reactive({pointInfoList$showGrid()})
displayMode<-reactive({pointInfoList$displayMode()})
insertMode<-reactive({pointInfoList$insertMode() })


#---------ShowPts----------------------------------

  # called when we need to show points (in epilog)
  # to do: rewrite to make this work with call for tags
  # where each tag is a group, so that we can edit a tag set 
  # to provide ability for translate, rotate, scale of points
  showPts.PtCmd %<c-% function(ptName, pts=NULL,  selectedPointIndx=NULL, ptDisplayMode="Normal"){
    if(is.null(pts) ){ return(NULL) } 
    if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
    if(length(pts)<2 ){ return(NULL)}
    #print("entering showPts.PtCmd Proper")
    selectedPointIndx<-as.numeric(selectedPointIndx)
    
    colorScheme<-c(default="green", ending="red", selected="blue")
    m<-matrix(pts,2) # is this really necessary????
     
    #form list of  all point renderings
    lapply(1:ncol(m), function(i){
      id<-paste("pd",ptName,i,sep="-")
      pt<-m[,i]
      color=colorScheme['default']
      if(i==length(pts)/2) { #ncol(m)){
          color=colorScheme['ending']   
      }
      list(
        if(identical(selectedPointIndx, as.numeric(i) )){
          circle(class="draggable", 
                 id=id,  
                 cxy=pt, r=9, fill="yellow", 
                 opacity=1,
                 stroke=colorScheme['selected'], stroke.width=3,
                 transform="matrix(1 0 0 1 0 0)", 
                 onmousedown="selectPoint(evt)" )
        } else { #a non-selected point
          circle(class="draggable", 
                 id=id,  
                 cxy=pt, r=8, fill=color, opacity=1,
                 transform="matrix(1 0 0 1 0 0)", 
                 onmousedown="selectPoint(evt)" )
        },
        if(ptDisplayMode=="Labeled"){
            text(paste(i), cxy=pt+10*c(1,-1),  
               stroke='black', font.size=12, opacity=1) 
        } else {
          NULL
        }
      )
    }) #end lapply
  } #end showPts.PtCmd


# output$svgPointsPanel<-renderUI({
#  conditionalPanel( "input.plotNavBar=='Points'",
#  absolutePanel( top=130, left=0, right=0,  draggable=FALSE,
#                     style=cstyle$svg, htmlOutput("svgPointsPlot")
#      )
#  )
# })
# 
# 
# output$svgPointsPlot <- renderUI({
#  WH<-getSVGWH()
#  showGrid<-showGrid()
#  if(is.null(showGrid)){return(NULL)}
# 
#  ptName<-getPtName()
#  ptRList<-getPtDefs()$pts
#  ptDisplayMode<-displayMode()
#  src<-getCode()
# 
#  ptrDisplayScript<-js.scripts[[ "Points"]]
# 
#  showPts<-function(ptName, ptRList, ptDisplayMode){
#    if(!is.null(ptName) && !is.null(ptRList)){
#      pts<- ptRList[[ptName]]
#      selectedPointIndx<-as.numeric( getPtIndex() )
#    } else {
#      pts<-NULL
#      selectedPointIndx<-0
#    }
#    showPts.PtCmd(ptName, pts=pts,  selectedPointIndx=selectedPointIndx, ptDisplayMode=ptDisplayMode)
#  }
# 
#  #Only applies when svgBarCmd=="Points"
#  newPtLayer %<c-% function(svgBarCmd, wh=c(1200,800)){
#    if( displayOptions$insertMode==TRUE){
#      rect(xy=c(0,0), wh=wh, fill="#ADADFF", stroke='black', opacity=.0, onmousedown="newPoint(evt)")
#    } else {
#      NULL
#    }
#  }
# 
#  #defining the prolog
#  insert.beg<-c(
#    'style(".draggable {','cursor: move;','}"),',
#    gsub('ptrDisplayScript', ptrDisplayScript, "script('ptrDisplayScript'),"),
#    "use(filter=filter(filterUnits=\"userSpaceOnUse\", feFlood(flood.color='white') )),",
#    if(showGrid()==TRUE){"graphPaper( wh=c(2000,2000), dxy=c(50, 50), labels=TRUE ),"}
#    else { NULL }
#  )
# 
#  #defining the epilog
#  insert.end<-c(
#    ',newPtLayer(svgBarCmd, WH),',
#    'showPts(ptName, ptRList,  ptDisplayMode)'
#  )
# 
#  #put it together
#  src<-subSVGX2(src, insert.beg, insert.end)
#  res<-""
#    tryCatch({
#        parsedCode<-parse(text=src)
#        svg<-eval(parsedCode)
#        as.character(svg)->svgOut
#        res<-HTML(svgOut)
#        backup$code<-getCode()
#        backup$res<-res
#      },
#      error=function(e){
#        # session$sendCustomMessage(type='testmessage', message=e)
#        mssg$error<-paste(mssg$error, e, collapse="\n", sep="\n")
#        # cat(mssg$error)
#        user$code<-getCodeBackUp()
#        updateNavbarPage(session, "plotNavBar", selected ="Log")
#      }
#    )
#  res
# })
# 


#===============

#all the options for implementation

#svgBarCmd<-input$plotNavBar
#ptrDisplayScript<-switch(svgBarCmd,
#    Points     =js.scripts[[ "Points"]],
#    tagValues  =js.scripts[[ "Points"]],
#    dragTag    =js.scripts[[ "transTag"]],
#    Transforms = js.scripts[[ input$transformOption ]]
#  )

#this is tagDisplay Mode
displayModeTag<-reactive({
  if(ptDisplayMode()=="hidden"){
  } else {
    ptDisplayMode()
  }
})

showPts.transform %<c-% function(){ NULL }

displayModeTransform<-reactive({ "Hidden"}) #or NULL???

getTags<-reactive({
  ptName<-getPtName()
  if(is.null(  getPtName() )){
    return(NULL)
  }
  tagRList<-getPtDefs()$df 
  if(!is.null(tagRList)){
    tagRList[[ptName]]$tag
  } else {
    NULL
  }
})

#use for pts, ow NULL
getPtLayer<-reactive({
  newPtLayer %<c-% function( wh=getWH() ){
    if( insertMode()==TRUE){
      rect(xy=c(0,0), wh=wh, fill="#ADADFF", stroke='black', opacity=.0, onmousedown="newPoint(evt)")
    } else {
      NULL
    } 
  } 
})



  
  
  
  #tagVal
#  showsvgRPoints.tagVal<-reactive({
#    showPts.valTag(ptName=getPtName(),pts=getPtDefs()$pts,  selectedPointIndx=as.numeric( getPtIndex() ), 
#        ptDisplayMode=displayModeTag(),  
#        ptTags=getTags() 
#    )
#  })
#  
#  #tagDrag
#  showsvgRPoints.tagDrag<-reactive({
#    showPts.dragTag(ptName=getPtName(),, pts=pts, 
#        selectedPointIndx=as.numeric( getPtIndex() ), 
#        ptDisplayMode=displayModeTag(),  tags=getTags() 
#    )  
#  })
  
#  showsvgRPoints.transform<-reactive({
#    NULL
#  })
#
#  ---------------
#  
#  getCodeTransform<-reactive({
#    src<-getCode()
#    src<-usingDraggable(src)
#  })
#  
  #if(svgBarCmd=="Transforms"){ #transform}
  
  


#===============
output$PointsPanelPlot<-renderUI({
  conditionalPanel( "input.plotNavBar=='Points'", modulePlotSVGrUI("svgPointsMod"))
})

#points
showsvgRPoints.pts<-reactive({
  showPts.PtCmd2(
    ptName=getPtName(), pts=getPtDefs()$pts,
    selectedPointIndx=as.numeric( getPtIndex() ),
    ptDisplayMode=displayMode()
  )
})



newPtLayer2 %<c-% function(insert, wh=c(1200,800)){
  if(insert==TRUE){
    rect(xy=c(0,0), wh=wh, fill="#ADADFF", stroke='black', opacity=.0, onmousedown="newPoint(evt)")
  } else {
    NULL
  } 
}
  
# show.svgRpoints.compound<-reactive({
#   pts=getPtDefs()$pts
#   list(
#     newPtLayer2( insertMode(), getSVGWH() ),
#     showPts.PtCmd(
#       ptName=getPtName(), pts=pts,
#       selectedPointIndx=as.numeric( getPtIndex() ),
#       ptDisplayMode=displayMode(), insert=insertMode()
#     )
#   )
# })

pointSVGList<-callModule(
  module=modulePlotSVGr,
  id="svgPointsMod",
  showPts.compound=reactive({
    list(
      newPtLayer2( insertMode(), getSVGWH() ),
      showPts.PtCmd(
        ptName=getPtName(), pts=getPtDefs()$pts[[getPtName()]],
        selectedPointIndx=as.numeric( getPtIndex() ),
        ptDisplayMode=displayMode()
      )
    )
  }),
  ptrDisplayScript =js.scripts[[ "Points"]],
  getSVGWH,
  showGrid,
  getCode,
  getCode2 =getCode,  # (or getCodeTransform)
  getCodeBackup,
  getErrorMssg,
  insert.end=",showPts.compound()"
)
