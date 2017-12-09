
# --------------input$plotNavBar=="Points"----------------


# ===============Begin Server PointsBar=======================

# called indirectly by either a new/load source or upon a commit???
# called directly by getSelectInfo, which is called by pointsBar module initialization
# ex.getSelectInfo<-function(tibList, selected, point.index){
#   choices<-names(tibList)
#   if(length(tibList)==0 ){
#     rtv<-list(selected=NULL, point.index =0 )  
#     return(rtv)
#   }
#   
#   if(length(selected)<1 || !(selected %in% choices) ){ # a new choice
#     #pick the first choice candidate
#     selected=choices[1]
# #!!! ERROR  to extract points, we need the point column from the tib list. 
# # BUT WE DON'T HAVE ACCESS TO THAT HERE !!!
#     pts<-tibList[[selected]] 
#     point.index<-length(pts)/2
#     rtv<-list(
#       selected=selected,
#       point.index=point.index
#     )
#     return(rtv)
#   }
#   #default: an existing choice
#   point.index<-min(point.index, length( tibList[[selected]])/2 ) #cannot be longer than the number of points
#   rtv<-list(
#     selected=selected, 
#     point.index=point.index
#   )
#   return(rtv)  
# }

# getSelectInfo<-reactive({ #used by pointsBar only??
#   name<-getTibName()
#   indx<-getPtIndex()
#   tibs<-getPtDefs()$tib
#   ex.getSelectInfo(tibs, name, indx)
#   #ex.getSelectInfo(getPtDefs()$pts, getPtName(), getPtIndex())
# })


#CALL modulePointsBarUI

returnValue4ModulePointsBar<-callModule( #auto  input, output, session 
  module=modulePointsBar, 
  id="pointsBar", 
  barName=rightPanel,
  #getTibPtsColEndIndex=getTibPtsColEndIndex,
  #getSelectInfo=getSelectInfo, #not the best way, but ...
  #getPtDefs=getPtDefs, 
  name=getTibName, 
  nameChoices=getTagNameChoices,
  ptIndex=getPtIndex,
  ptIndexChoices=getPtIndexChoices,
  rowIndex=getTibRow,
  rowIndexChoices=getTibRowChoices,
  matColIndex=getTibMatCol,
  matColIndexChoices=getTibMatColChoices,
  #isTaggable=isTaggable,
  headerId=NS("pointsBar", 'header')
)

#-----REACTIVES   based on modulePointsBar::returnValue4ModulePointsBar

observeEvent(returnValue4ModulePointsBar$showGrid() ,{
  if(rightPanel()=="Points"){
    displayOptions$showGrid<-returnValue4ModulePointsBar$showGrid() 
  }
})



displayMode<-reactive({returnValue4ModulePointsBar$displayMode()})
insertMode<-reactive({returnValue4ModulePointsBar$insertMode() })

#-----OBSERVERS  using  modulePointsBar::returnValue4ModulePointsBar

# --SELECTION EVENTS-------------------------------
observeEvent(returnValue4ModulePointsBar$name(), {
  if(rightPanel()=="Points"){
    name<-returnValue4ModulePointsBar$name()
    if(!is.null(name)){
      updateSelected(name=returnValue4ModulePointsBar$name())
    }
  }
})



# Replace this with update of row, column and then tribute to point index
# selected pts index
# observeEvent(returnValue4ModulePointsBar$pointIndex(), { 
#   if(rightPanel()=="Points"){
#     cat("serverPlotBar:: observeEvent- 625\n")
#     cat("returnValue4ModulePointsBar$pointIndex()=\n")
#     indx<-returnValue4ModulePointsBar$pointIndex() 
#     print(returnValue4ModulePointsBar$pointIndex())
#     #rc<-absPtIndx2TibPtPos(indx)
#    
#     updateSelected(row=row, matCol=matCol, point.index=returnValue4ModulePointsBar$pointIndex() )
#   }
# })


# observeEvent(c(returnValue4ModulePointsBar$rowIndex(), returnValue4ModulePointsBar$matColIndex()){ 
#   if(rightPanel()=="Points"){
#     updateSelected(row=returnValue4ModulePointsBar$rowIndex(),  matCol=returnValue4ModulePointsBar$matColIndex() )
#   }
# })





#-----------BUTTON EVENTS--------------------
#---BUTTON: remove selected point  -----
observeEvent( returnValue4ModulePointsBar$removePt(), {
  selection<-getTibName() #selectedPoint$name
cat('Enter removePt\n')  
  if(selection!=""){
    ptDefs<-getPtDefs()

    if(length(ptDefs$tib)==0){return(NULL)}
    #do more checks as necessary
    
    #ptRList<-getPtDefs()$pts
    #pts<-ptRList[[selection]]
    
    # if(length(pts)==0){  #if no points, return
    #   return(NULL)
    # }
    indx=getPtIndex() #selectedPoint$point.index 
    # if(indx==0){ return(NULL)}
    
    cat("indx=",indx,"\n")
    src<-getCode() 
    
    #get row, col
    if(indx>=1){ #should be unnecessary, but...
      rc<-absPtIndx2TibPtPos(indx)
      m<-ptDefs$tib[[selection]][[ rc$row, getTibPtColPos() ]] #!!! probably need some checking here
      print(m)
      cat("rc$matCol=",rc$matCol,"\n")
      if(length(m)>0 && rc$matCol<= ncol(m)){
        if(ncol(m)>0){ # After removal matrix will be empty
          ptDefs$tib[[selection]][[ rc$row, getTibPtColPos() ]]<-m[,-rc$matCol]
          updateSelected(point.index= indx-1) #!!! will need revisit soon
        }
      }
    }
    # get m<-mat[row,col]
    # if ncol(m)<=1, remove row
    # else remove m$matCol from m
    # replace in ptDefs
    # have a nice day
     
    #delete the point from the ptR list
    # if(indx>=1){
    #   pts<-pts[-c(2*indx,2*indx-1)]
    #   selectedPoint$point.index<-max(1,selectedPoint$point.index-1)
    # } 
    # if( indx==0 || length(pts)<1 ){
    #   pts<-list(NULL)
    #   selectedPoint$point.index<-0 
    #   ptRList[selection]<-pts
    # } else {
    #   ptRList[[selection]]<-pts
    # }
     #src<-pts2Source(src,ptRList)
    
    # tagRList<-getPtDefs()$df
    # if(!is.null(tagRList)){ #tagR exists
    #   df<-tagRList[[selection]]
    #   if(!is.null(df)){ #df==tagR$x exists
    #     if(length(ptRList[[selection]])==0){ #no points
    #       tagRList[selection]<-NULL #remove tagR$x
    #     } else { # has points
    #       tags<-df$tag
    #       freq<-reactiveTag$freq[[selection]]
    #       # (this is the manual case)
    #       if(is.null(freq)){
    #         if(indx==1 && !(2 %in% tags)){ #do nothing
    #         } else {
    #           if(indx %in% tags){ #remove the tag row
    #             df<-subset(df,tags!=indx)
    #             tags<-df$tag
    #           }
    #         }
    #         #slide tags nos. down
    #         tags2move<-tags>indx
    #         if(length(tags2move)>0){
    #           tags[tags>indx]<-tags[tags>indx]-1
    #           df$tag<-tags
    #           tagRList[[selection]]<-df
    #         }
    #       } else { # freq is not null,
    #         pts<-ptRList[[selection]]
    #         if( length(pts)/2<tail(tags,1) ){
    #           # if no of pts == last tag, then remove the last row from df
    #           n<-length(tags)-1
    #           df<-head(df, n)
    #           tagRList[[selection]]<-df
    #         }
    #       }
    #     }
    #     
    #     # ptRList
    #     # src<-df2Source(src,tagList)
    #   }
    # }
    newPtDefs<-ptDefs
    sender='points.deletePoint'
    updateAceExtDef(newPtDefs, sender=sender)
    # setCode(src) #!!!
  }
}) #end remove point observer

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

# When okTag button of modalFreq isis pressed, 
# Corresponds to initial point tag
observeEvent(input$okTag, { #move into module???
  #covers the two cases
  # 1. tagR list not there, add tagR list, selection and insert 
  # 2. tagR list there, but selection is not: add selection and insert
  
  # Check that data object exists and is data frame.
  selection<-getTibName() #selectedPoint$name
  if(input$tagFreq=="Off"){
    reactiveTag$freq[[selection]] <- NULL
  } else{
    reactiveTag$freq[[selection]] <- as.numeric(input$tagFreq)
  }
    # set the value here
  removeModal()
  # and complete the processing
  src<-getCode()
  ptDefs<-getPtDefs()
  ptsList<-ptDefs$pts
  dfList<-ptDefs$df
  point.index<-getPtIndex()
  point.index<-max(1,point.index) #!!!TODO: change later
  
  updateSelected(row=getTibRow()+1, matCol=1, point.index=point.index)
  
  
  if(!is.null(reactiveTag$freq[[selection]])){ #pad tags to the end and exit
    freq<-as.integer(reactiveTag$freq[[selection]])
    tags<-seq.int(from=1,to=ncol(ptsList[[selection]]), by=freq)
  } else { #manual # add the last pt and be cool
    if(point.index>=1){
      tags<-unique(c(1,point.index))
    }
  }
  df<-data.frame(tag=tags) #creates a data frame with tags entry
  if(is.null(dfList) ){ # 1 if dfList is NULL, create new
    dfList= structure( list( df) , names=selection )
  } else {
    dfList[[selection]]<-df
  }
  #src<-getCode()
  newPtDef<-list(pts=ptsList, df= dfList )
  
  # !!!TODO Iterate thru columns of newPtDef$pts and convert to list of lists
  pts<-newPtDef$pts
  
 
  newPtDef<-olde2newFmtPtDef2ListOfLists(newPtDef)
  
  
  replacementList<-ptDef2ReplacementList(selection, newPtDef, src)
  
  
  if( length(replacementList)>0 ){
    #!!! TODO can we replace the following with an updateAceExtDef???
    # session$sendCustomMessage(
    #   type = "shinyAceExt",
    #   list(id= "source", replacement=replacementList, sender='tag.pt.button', ok=1) # wtf ok (data.ok in aceExt.js) only revelant when setValue is attr
    # )
    updateAceExtDef( newPtDef, 'tag.pt')
  }
}) #end of okTag button press



# #---TAG THIS POINT button-----
# # note: in 1st tag, calls freqModal to complete the work, which exits in the okTag above
# observeEvent( returnValue4ModulePointsBar$tagPt(), {
#   #There are 3 distinct cases: 
#   # 1. tagR list not there, add tagR list, selection and insert 
#   # 2. tagR list there, but selection is not: add selection and insert 
#   # 3  Both tagR list and tagR[[selection]] are there, just add tag no.
#   if(rightPanel()=="Points"){
#     #selection<-input$ptRSelect
#     selection<-selectedPoint$name
#     ptDefs<-getPtDefs()
#     ptsList<-ptDefs$pts
#     dfList<-ptDefs$df
#     point.index<-max(1,selectedPoint$point.index) #can change later
#     #PROPOSED REWRITE:
#     ok= !(is.null(dfList)) && !(is.null(dfList[[selection]]) )
#     if(!ok) { #this is a first tag
#       showModal( modalFreq() ) #observer for showModal must complete the work
#     } else { # this is a second tag (and hence manual)
#       #if(reactiveTag$freq[[selection]]==0){ #manual case
#       #add this tag
#       df<-dfList[[selection]]
#       if("tag" %in% names(df)){ # if not, then do nothing
#         tags<-df$tag
#         if(!(point.index %in% tags)){
#           row<-max(tags[tags<point.index])
#           tmp.df<-subset(df,tag==row)
#           tmp.df$tag<-point.index
#           df<-rbind(df, tmp.df)
#           ordrows<-order(df$tag)
#           df<-df[ordrows,,drop=FALSE]
#           dfList[[selection]]<-df
#           src<-getCode() 
#           #src<-df2Source(src,dfList)
#           # setCode(src) #!!!
#           
#           newPtDef<-list(pts=ptsList, df= dfList )
#           updateAceExtDef(newPtDef, 'tag.pt')
#           # replacementList<-ptDef2ReplacementList(newPtDef, src)
#           # #src<-df2Source(src,dfList) #insert points into src
#           # 
#           # if( length(replacementList)>0 ){
#           #   session$sendCustomMessage(
#           #     type = "shinyAceExt",
#           #     list(id= "source", replacement=replacementList, sender='tag.pt.button', ok=1)
#           #   )
#           # }
#         }
#       }
#     }
#   } #end of if
# }) #end of point InfoList Tag Point, 


#---TAG THIS POINT button-----
# note: in 1st tag, calls freqModal to complete the work, which exits in the okTag above
observeEvent( returnValue4ModulePointsBar$tagPt(), {
  
  if(rightPanel()=="Points"){
    #selection<-input$ptRSelect
    cat("Enter tagPt\n")
    src<-getCode() 
    selection<-getTibName() #selectedPoint$name
    ptDefs<-getPtDefs()
    #indx<-getPtIndex() #selectedPoint$point.index
    #rc<-absPtIndx2TibPtPos(indx) #point location
    rc<-list( row=getTibRow(), matCol=getTibMatCol() )
    
    m<-ptDefs$tib[[selection]][[ rc$row, getTibPtColPos() ]]
    if(ncol(m)<1){ 
      return(NULL) # bail if matrix of points is empty
    }
    tib<-ptDefs$tib[[selection]] #get the tib 
    tib<-tagTib(tib, getTibPtColPos(), rc$row, rc$matCol)
    ptDefs$tib[[selection]]<-tib 
    sender='tagPt'
    updateAceExtDef(ptDefs, sender=sender)
    #updateSelected(point.index=indx, row=getTibRow()+1, matCol=1)
    updateSelected(row=getTibRow()+1, matCol=1)
  } #end of if
}) #end of point InfoList Tag Point, 


observeEvent( returnValue4ModulePointsBar$forwardPt(), {
  matColIndex<-getTibMatCol()
  if(length( matColIndex)>0){
    cat("observeEvent:: serverPlotBar 99\n")
    matColIndex=min(matColIndex+1, max(getTibMatColChoices()) )
    point.index<-tibPtPos2AbsPtIndx()(getTibRow(), matColIndex )
    #rc<-absPtIndx2TibPtPos(point.index)
    updateSelected(  matCol=matColIndex, point.index=point.index )
    # selectedTibble$row<-rc$row
    # selectedTibble$col<-rc$col
    
  }
})

observeEvent( returnValue4ModulePointsBar$backwardPt(), {
  matColIndex<-getTibMatCol()
  if(length(matColIndex)>0){
    cat("observeEvent:: serverPlotBar 98\n")
    matColIndex=max(matColIndex-1, min(getTibMatColChoices()) )
  #  rc<-absPtIndx2TibPtPos(point.index)
    point.index<-tibPtPos2AbsPtIndx()(getTibRow(), matColIndex )
    updateSelected(  matCol=matColIndex, point.index=point.index  )
    # selectedTibble$row<-rc$row
    # selectedTibble$col<-rc$col
  }
})

# ===============END SERVER Module PointsBar=======================



# ===============BEGIN SERVER Module svgPointsMod=======================

#---------ShowPts----------------------------------

  showPts.PtCmd %<c-% function(
      ptName, 
      pts=NULL,  
      selectedPointIndx=NULL, 
      rowIndex=NULL,
      matColIndex=NULL,
      ptDisplayMode="Normal"
  ){
    onMouseDownTxt='ptRPlotter_ptR_SVG_Point.selectPoint(evt)'
    
    if(is.null(pts) ){ return(NULL) } 
    if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
    
    cat("pts-----------------------------------------------\n")
    print(pts)
    if(length(unlist(pts))<2){ return(NULL)}
    
    selectedPointIndx<-as.numeric(selectedPointIndx)
    cat("selectedPointIndx=",selectedPointIndx,"\n")
    
    colorScheme<-c(default="green", ending="red", selected="blue")
    semitransparent<-0.3
    
    cat("rowIndex",rowIndex,"\n")
    cat("class(rowIndex)",class(rowIndex),"\n")
    
    cat("matColIndex",matColIndex,"\n")
    cat("class(matColIndex)",class(matColIndex),"\n")
    
    # genId<-function(){
    #   nid<-0 # use to generate ids
    #   function(){
    #     nid<<-nid+1
    #     paste("pd",ptName,nid,sep="-")
    #   }
    # } 
    # ptId<-genId()
    
    #nid<-0
    opacity<-rep(semitransparent, length(pts) )
    opacity[rowIndex]<-1
    
    #form list of  all point renderings
    lapply(seq(length(pts)), function(i){
      m<-pts[[i]]
      if(length(m)==0){ # or !is(m,'matrix')
        NULL
      } else {
        lapply(seq(ncol(m)), function(j){ #j is the matCol index
          #nid<<-nid+1
          #cat("nid=",nid,"\n")
          #id<-ptId() #
          #id<-paste("pd",ptName,nid,sep="-")
          id<-paste("pd",ptName,i,j,sep="-")
          cat("id=", id, "\n")
          pt<-m[,j]
          color=colorScheme['default']
          # if(i==length(pts)/2) { #ncol(m)){
          #   color=colorScheme['ending']   
          # } 
          list(
            #if(identical(selectedPointIndx, nid )){
            #if(identical(i,rowIndex) && identical(j, matColIndex) ){
            if(i==rowIndex && j== matColIndex ){
              circle(class="draggable", 
                     id=id,  
                     cxy=pt, r=9, fill="yellow", 
                     opacity=opacity[i],
                     stroke=colorScheme['selected'], stroke.width=3,
                     #transform="matrix(1 0 0 1 0 0)", 
                     onmousedown=onMouseDownTxt
                     #'ptRPlotter_ptR_SVG_Point.selectPoint' #onmousedown 
              )
            } else { #a non-selected point
              circle(class="draggable", 
                     id=id,  
                     cxy=pt, r=8, fill=color, opacity=opacity[i],
                     #transform="matrix(1 0 0 1 0 0)", 
                     onmousedown=onMouseDownTxt
                     #'ptRPlotter_ptR_SVG_Point.selectPoint' #onemousedown 
              )
            },
            if(ptDisplayMode=="Labeled"){
              text(paste0(i,",",j), cxy=pt+10*c(1,-1),  
                   stroke='black', font.size=12, opacity=1) 
            } else {
              NULL
            }
          )
        }) #end lapply of this row
      }
 
    }) #end lapply of points
  } #end showPts.PtCmd





#===============


# !!! todo: recode onmousedown.newPt
newPtLayer %<c-% function(insert, wh=c(1200,800)){
  if(insert==TRUE){
    onmousedownNewPt="ptRPlotter_ptR_SVG_Point.newPoint(evt)"
    rect(xy=c(0,0), wh=wh, fill="#ADADFF", stroke='black', 
         opacity=.0, onmousedown=onmousedownNewPt)
           #"ptRPlotter_ptR_SVG_Point.newPoint(evt)")
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
        ptName=getPtName(), 
        pts=getTibPts(), #getPtDefs()$pts[[getPtName()]],
        selectedPointIndx=as.numeric( getPtIndex() ),
        rowIndex=getTibRow(),
        matColIndex=getTibMatCol(),
        ptDisplayMode=displayMode()
      )
    )
  }),
  ptrDisplayScript = reactive({ svgToolsScript( "Points") }), #ptrDisplayScript =reactive({ js.scripts[[ "Points"]] }),
  getSVGWH,
  showGrid,
  getCode,
  getCode2 =getCode,  # (or getCodeTransform)
  getErrorMssg,
  insert.end=",showPts.compound()"
)

#error handler
observeEvent(statusPlotPoint$status(), {
  status<-statusPlotPoint$status()
  if( status$state!="PASS"){ 
    updateRightPanel('logPanel')
    mssg$err<-status$message
  }
})



   