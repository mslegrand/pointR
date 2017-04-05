
# --------------input$plotNavBar=="Points"----------------


# ===============Begin Server PointsBar=======================

# called indirectly by either a new/load source or upon a commit???
# called directly by getSelectInfo, which is called by pointsBar module initialization
ex.getSelectInfo<-function(ptRList, selected, point.index){
  choices<-names(ptRList)
  if(length(choices)==0 ){
    rtv<-list(selected=NULL, point.index =0 )  
    return(rtv)
  }
  
  if(length(selected)<1 || !(selected %in% choices) ){ # a new choice
    #pick the first choice candidate
    selected=choices[1]
    pts<-ptRList[[selected]]
    point.index<-length(pts)/2
    rtv<-list(
      selected=selected,
      point.index=point.index
    )
    return(rtv)
  }
  #default: an existing choice
  point.index<-min(point.index, length( ptRList[[selected]])/2 ) #cannot be longer than the number of points
  rtv<-list(
    selected=selected, 
    point.index=point.index
  )
  return(rtv)  
}

getSelectInfo<-reactive({ #used by pointsBar only??
  name<-getPtName()
  indx<-getPtIndex()
  pts<-getPtDefs()$pts
  ex.getSelectInfo(pts, name, indx)
  #ex.getSelectInfo(getPtDefs()$pts, getPtName(), getPtIndex())
})


#CALL modulePointsBarUI
pointInfoList<-callModule( #auto  input, output, session 
  module=modulePointsBar, 
  id="pointsBar", 
  barName=rightPanel,
  getSelectInfo=getSelectInfo, #not the best way, but ...
  getPtDefs=getPtDefs, 
  name=getPtName, 
  index=getPtIndex, 
  isTaggable=isTaggable
)

#-----REACTIVES   based on modulePointsBar::pointInfoList

showGrid<-reactive({pointInfoList$showGrid()})
displayMode<-reactive({pointInfoList$displayMode()})
insertMode<-reactive({pointInfoList$insertMode() })

#-----OBSERVERS  using  modulePointsBar::pointInfoList

# --SELECTION EVENTS-------------------------------
observe({
  name<-pointInfoList$name()
    if(!is.null(name)){
      selectedPoint$name<-pointInfoList$name()
    }
})

# selected pts index
observe({ selectedPoint$point.index<-pointInfoList$index() })

#-----------BUTTON EVENTS--------------------
#---BUTTON: remove selected point  -----
observeEvent( pointInfoList$removePt(), {
  selection<-selectedPoint$name
  #selection<-input$ptRSelect
  if(selection!=""){
    ptRList<-getPtDefs()$pts
    pts<-ptRList[[selection]]
    if(length(pts)==0){  #if no points, return
      return(NULL)
    }
    indx=selectedPoint$point.index 
    src<-getCode() 
    
    #delete the point from the ptR list
    if(indx>=1){
      pts<-pts[-c(2*indx,2*indx-1)]
      selectedPoint$point.index<-max(1,selectedPoint$point.index-1)
    } 
    if( indx==0 || length(pts)<1 ){
      pts<-list(NULL)
      selectedPoint$point.index<-0 
      ptRList[selection]<-pts
    } else {
      ptRList[[selection]]<-pts
    }
    
    
    src<-pts2Source(src,ptRList)
    
    tagList<-getPtDefs()$df
    if(!is.null(tagList)){ #tagR exists
      df<-tagList[[selection]] 
      if(!is.null(df)){ #df==tagR$x exists
        if(length(ptRList[[selection]])==0){ #no points 
          tagList[selection]<-NULL #remove tagR$x
        } else { # has points
          tags<-df$tag
          freq<-reactiveTag$freq[[selection]]
          # (this is the manual case)
          if(is.null(freq)){
            if(indx==1 && !(2 %in% tags)){ #do nothing
            } else {
              if(indx %in% tags){ #remove the tag row
                df<-subset(df,tags!=indx)
                tags<-df$tag
              }
            }
            #slide tags nos. down 
            tags2move<-tags>indx
            if(length(tags2move)>0){
              tags[tags>indx]<-tags[tags>indx]-1
              df$tag<-tags
              tagList[[selection]]<-df
            }
          } else { # freq is not null, 
            pts<-ptRList[[selection]]
            if( length(pts)/2<tail(tags,1) ){
              # if no of pts == last tag, then remove the last row from df
              n<-length(tags)-1
              df<-head(df, n)
              tagList[[selection]]<-df
            }
          }
        }
        src<-df2Source(src,tagList)
      }
    }
    setCode(src)
  }
})

#----begin for Tagging-------------------------------------

# Return the UI for a modal dialog with data selection input. If 'failed' is
# TRUE, then display a message that the previous value was invalid.
modalFreq <- function(failed = FALSE) {
  doOk<-'shinyjs.triggerButtonOnEnter(event,"okTag")'
  modalDialog(
    onkeydown=doOk,
    selectInput("tagFreq", "Auto Tag",
                c(list("Off"),1:20), selected="Off", 
                multiple=FALSE, selectize = FALSE,
                width="80px", size=1  ), 
    span('Start tagging current point matrix'), 
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okTag", "OK")
    )
  ) 
}



# When OK button is pressed, 

observeEvent(input$okTag, { #move into module???
  #covers the two cases
  # 1. tagR list not there, add tagR list, selection and insert 
  # 2. tagR list there, but selection is not: add selection and insert
  
  # Check that data object exists and is data frame.
  selection<-selectedPoint$name
  if(input$tagFreq=="Off"){
    reactiveTag$freq[[selection]] <- NULL
  } else{
    reactiveTag$freq[[selection]] <- as.numeric(input$tagFreq)
  }
    # set the value here
  removeModal()
  # and complete the processing
  ptDefs<-getPtDefs()
  ptsList<-ptDefs$pts
  dfList<-ptDefs$df
  point.index<-max(1,selectedPoint$point.index) #can change later
  
  if(!is.null(reactiveTag$freq[[selection]])){ #pad tags to the end and exit
    freq<-as.integer(reactiveTag$freq[[selection]])
    tags<-seq.int(from=1,to=ncol(ptsList[[selection]]), by=freq)
  } else { #manual # add the last pt and be cool
    if(point.index>=1){
      tags<-unique(c(1,point.index))
    }
  }
  df<-data.frame(tag=tags) 
  if(is.null(dfList) ){ # 1 if dfList is NULL, create new
    dfList= structure( list( df) , names=selection )
    replacement<-paste0("\n\n", formatDFDefs(dfList),"\n\n")
    src<-getCode() 
    pos<-getDefPos(src, "ptR")     
    src<-paste0( substr(src, 1, pos[2]), replacement,  
                 substr(src, pos[2]+1, nchar(src))) 
    setCode(src)
  } else {
    dfList[[selection]]<-df
    src<-getCode()
    src<-df2Source(src,dfList) #insert points into src
    setCode(src)
  }
})

#---TAG THIS POINT button-----

observeEvent( pointInfoList$tagPt(), {
  #There are 3 distinct cases: 
  # 1. tagR list not there, add tagR list, selection and insert 
  # 2. tagR list there, but selection is not: add selection and insert 
  # 3  Both tagR list and tagR[[selection]] are there, just add tag no.
  if(rightPanel()=="Points"){
    #selection<-input$ptRSelect
    selection<-selectedPoint$name
    ptDefs<-getPtDefs()
    ptsList<-ptDefs$pts
    dfList<-ptDefs$df
    point.index<-max(1,selectedPoint$point.index) #can change later
    #PROPOSED REWRITE:
    ok= !(is.null(dfList)) && !(is.null(dfList[[selection]]) )
    if(!ok) { #this is a first tag
      showModal( modalFreq() ) #observer for showModal must complete the work
    } else { # this is a second tag (and hence manual)
      #if(reactiveTag$freq[[selection]]==0){ #manual case
      #add this tag
      df<-dfList[[selection]]
      if("tag" %in% names(df)){ # if not, then do nothing
        tags<-df$tag
        if(!(point.index %in% tags)){
          row<-max(tags[tags<point.index])
          tmp.df<-subset(df,tag==row)
          tmp.df$tag<-point.index
          df<-rbind(df, tmp.df)
          ordrows<-order(df$tag)
          df<-df[ordrows,,drop=FALSE]
          dfList[[selection]]<-df
          src<-getCode() 
          src<-df2Source(src,dfList)
          setCode(src)
        }
      }
    }
  } #end of if
})
# ===============END SERVER Module PointsBar=======================



# ===============BEGIN SERVER Module svgPointsMod=======================

#---------ShowPts----------------------------------

  showPts.PtCmd %<c-% function(
      ptName, pts=NULL,  
      selectedPointIndx=NULL, ptDisplayMode="Normal"
  ){
    if(is.null(pts) ){ return(NULL) } 
    if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
    if(length(pts)<2 ){ return(NULL)}
    
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
                 #transform="matrix(1 0 0 1 0 0)", 
                 onmousedown="selectPoint(evt)" )
        } else { #a non-selected point
          circle(class="draggable", 
                 id=id,  
                 cxy=pt, r=8, fill=color, opacity=1,
                 #transform="matrix(1 0 0 1 0 0)", 
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





#===============



newPtLayer %<c-% function(insert, wh=c(1200,800)){
  if(insert==TRUE){
    rect(xy=c(0,0), wh=wh, fill="#ADADFF", stroke='black', 
         opacity=.0, onmousedown="newPoint(evt)")
  } else {
    NULL
  } 
}
  
#===============
  
statusPlotPoint<-callModule(
  module=modulePlotSVGr,
  id="svgPointsMod",
  svgID='ptR_SVG_Point',
  showPts.compound=reactive({
    list(
      newPtLayer( insertMode(), getSVGWH() ),
      showPts.PtCmd(
        ptName=getPtName(), pts=getPtDefs()$pts[[getPtName()]],
        selectedPointIndx=as.numeric( getPtIndex() ),
        ptDisplayMode=displayMode()
      )
    )
  }),
  ptrDisplayScript =reactive({ js.scripts[[ "Points"]] }),
  getSVGWH,
  showGrid,
  getCode,
  getCode2 =getCode,  # (or getCodeTransform)
  getErrorMssg,
  insert.end=",showPts.compound()"
)

observeEvent(statusPlotPoint$status(), {
  #status<-statusPlotPoint()
  status<-statusPlotPoint$status()
  if( status$state!="PASS"){
    srcRevert()
    # send mssg to log
    # switch to log 
  }
})



   