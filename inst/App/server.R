
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {
  
  source("util/exGetTag.R", local=TRUE) # some ordinary functions :)
  
# Reactive values----------

  #source("util/serverManagerSrc.R", local=TRUE) no longer used
  
  request<-reactiveValues(
    code=NULL,
    sender='startup',
    refresh=NULL # to be used to force a code refresh???
  )
  
  triggerRefresh<-function(sender, rollBack=TRUE, auxValue=FALSE){ # to be used to force a code refresh???
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= "source", sender=sender, getValue=TRUE, rollBack=rollBack, auxValue=auxValue)
    )
  }

  selectedPoint <- reactiveValues(
    name="x", #NULL,       # name of current point array
    point.index=0          #  selected pt.indx (column) in current point array
  )
  

  selectedTibble <- reactiveValues(
    name="x", #NULL,       # name of current point array
    row=1,
    column=1, # default to last col?
    ptColName='pts',
    index=0          #  selected pt.indx (column) in current point array
  )

  
  updateSelected<-function( name, row, column, point.index, ptColName ){
    if(!missing(name)){
      selectedPoint$name=name
      selectedTibble$name=name
    }
    if(!missing(point.index)){
      selectedPoint$point.index=as.numeric(point.index)
    }
    if(!missing(ptColName)){
      selectedTibble$ptColName=ptColName
    }
    if(!missing(row)){ # !!! may want to provide a check here
      selectedTibble$row=row
      point.index<-selectedPoint$point.index
      if( row>0 ){
        endPos<-getTibPtsColEndIndex()
        begPos<-c(0,endPos)+1
        selectedPoint$point.index<-point.index<-min(max(point.index,begPos[row]),endPos[row])
      } 
    }
    if(!missing(column)){
      selectedTibble$column=column
    }
  }
  
  # selectedPoint <- reactiveValues(
  #   tibble.name="x", #NULL,       # name of current point array
  #   point.index=0,          #  selected pt.indx (column) in current point array
  #   point.col.name='pts',
  #   row.num=1
  # )

  
  panels<-reactiveValues(
    left='source',   #to be used as editor name later, for connecting to right graphics
    right="Points"
  )
  
  rightPanel<-reactive({panels$right})
  updateRightPanel<-function(panel){ panels$right<-panel}

  getLeftMenuCmd<-reactive({input$editNavBar$item})
  getRightMenuCmd<-reactive({input$plotNavBar$item})
  
  reactiveTag<-reactiveValues(freq=list())
  
  displayOptions<-reactiveValues(
    insertMode=TRUE,
    showGrid=FALSE,
    ptMode="Normal"
  )
  
  mssg<-reactiveValues(error="") 
  
  
  shinyFileChoose(input, "buttonFileOpenHidden", session=session, roots=c(wd="~") ) #hidden
  shinyFileChoose(input, "buttonSnippetOpen", session=session, roots=c(wd="~"),  filetypes=c('', 'snp') ) #hidden
  shinyFileSave(input, "buttonFileSaveHidden", session=session, roots=c(wd="~") ) #hidden
  
  
  # Reactive expressions------------- 
  showGrid<-reactive({displayOptions$showGrid})
  
  #--- yes unless tagged with freq or no points to tag 
  # isTaggable<-reactive({ 
  #   name<-getPtName()
  #   !is.null(name) && getPtIndex()>0 &&  is.null(reactiveTag$freq[[name]])
  # })
  
  getCode<-reactive({request$code})
  getPtName<-reactive({selectedPoint$name})
  getTibName<-reactive({selectedTibble$name}) #allw to be null only if tib is null
  getPtIndex<-reactive({as.numeric(selectedPoint$point.index)})
  
  getTib<-reactive({ getPtDefs()$tib[[ getTibName() ]] })
  getTibPtColPos<-reactive({ which(names(getTib())==selectedTibble$ptColName )})
  getTibPts<-reactive({ 
    if( !is.null(selectedTibble$ptColName)){
      getTib()[[ selectedTibble$ptColName ]]
    } else {
      NULL
    }
  })
      
    
  
  getTibPtsNCol<-reactive({ sapply(getTibPts(),ncol)}  )
  
  getTibPtsColEndIndex<-reactive({
    cs<-getTibPtsNCol()
    if(length(cs)>0){
      cs<-cumsum(cs)
    }
    cs
  })
  
  
  #!!!TODO THIS WILL FAIL IF WE HAVE MUTLIPLE EMPTY MATRICES, FIX ALGORITHM !!!
  absPtIndx2TibPtPos<-function(indx){
    # cat("Enter: absPtIndx2TibPtPos\n")
    # cat("(point.index) indx=",indx,"\n")
    # cat("length(indx)=",length(indx),"\n")
    rtv<-list(row=1,matCol=0)
    if(length(indx)>0 && indx>0){
      #tib<-ptDefs()$tib
      mlen<-sapply(getTibPts(),ncol)
      if(sum(mlen)<indx){
        #cat("mlen array: c(", paste0(mlen, collapse=", "), ")\n")
        return(NULL)
      }
      #cat("mlen array: c(", paste0(mlen, collapse=", "), ")\n")
      if(length(mlen)>0){
        endpts<-cumsum(mlen)
        #cat("endpts=c(",paste(endpts,collapse=","),")\n")
        begpts<-c(1, (endpts+1)[-length(endpts)])
        #cat("begpts=c(",paste(begpts,collapse=","),")\n")
        r<-sum(indx>=begpts)
        #cat("r=",r,"\n")
        if(r>0){
          matCol<-indx-(begpts[r]-1)
          rtv<-list(row=r,matCol=matCol)
        }
      }
    }
    #cat("rtv=list( row=",rtv$row,", matCol=",rtv$matCol,")\n")
    #cat("Exit: absPtIndx2TibPtPos\n\n")
    rtv
  }
  
  # getTibPtPos<-reactive({ # alternatively use observers and set row, index
  #   #pts<-getTibPts()
  #   cs<-getTibPtsColEndIndex()
  #   
  #   indx<-getPtIndex()
  #   sum(indx<=cs)->r
  #   cs<-c(0,cs)
  #   rindx<-indx-cs[r]
  #   list(row=r,matColPos=rindx)
  # })
  
  # observe({ #An alternative to getTibPtPos, updates tibble index, row whenever point.index changes
  #   cs<-getTibPtsColEndIndex()
  #   indx<-getPtIndex()
  #   isolate({
  #     #browser()
  #     if(length(indx)==0 || indx==0){
  #       selectedTibble$index<-0
  #       selectedTibble$row<-0
  #     } else {
  #       sum(indx<=cs)->r
  #       if(r>0){
  #         cs<-c(0,cs)
  #         selectedTibble$index<-indx-cs[r]
  #         selectedTibble$row<-r
  #       }
  #     }
  # 
  #   })
  # })
  
  # tibptPos can change if 
  #  1. index changes
  #  2. name changes
  #  3. tagging occurs
  #     note: 2 or 3 implies have changed 
  
  getTibRow<-reactive({selectedTibble$row})
  getTibIndex<-reactive({selectedTibble$index})
  
  #inverse function : not used
  tibPtPos2AbsPtIndx<-reactive({
    cs<-getTibPtsNCol()
    if(length(nCols)>0){
      cs<-cumsum(cs)
    }
    function(row, matCol){
      if( row>0 && matCol>0 && length(cs)>0 ){
        sum(cs[1:row])+matCol
      } else {
        0
      }
    }
  })
  
  
  
  #-----------------------
  getErrorMssg<-reactive({ mssg$error })
  getPtDefs<- reactive({ 
    ex.getPtDefs(getCode() ) 
  })  #extract points from user code
  
  #gets the tagged names
  getTagNameChoices<-reactive({
    names(getPtDefs()$tib)
    #intersect(names(getPtDefs()$pts), names(getPtDefs()$df))
  })
  
  # getSelectInfo<-reactive({ #used by pointsBar only??
  #   name<-getPtName()
  #   indx<-getPtIndex()
  #   pts<-getPtDefs()$pts
  #   ex.getSelectInfo(pts, name, indx)
  # })
  
  # getPts<-reactive({
  #   ptdef<-getPtDefs()
  #   ptdef[[getPtName()]]
  # })
  
  #gets a tagged name (=ptName unless ptName is not tagged)
  getTagName<-reactive({
    #exGetTagName( getTagNameChoices(), getPtName() )
    getTibName()
  })
  
  getTagIndexChoices<-reactive({
    #getPtDefs()$df[[getTagName()]]$tag
    nCols<-sapply(getTibPts(),ncol)
    if(length(nCols)>0){
      endPos<-cumsum(nCols)
      begPos<-c(1,1+endPos[-length(endPos)])
    } else {
      begPos<-0
    }
    begPos
  })
  
  getTagIndex<-reactive({ 
    #choices<-getTagIndexChoices()
    indx<-getPtIndex()
    if(length(indx)>0){
      ch<-getTagIndexChoices()
      max(ch[indx>=ch])
    } else
      0

    
  })
  
  
  #this is tagDisplay Mode
  getDisplayModeTag<-reactive({
    if(displayMode()=="hidden"){
    } else {
      displayMode()
    }
  })
  
  getTagDF<-reactive({
    ptName<-getPtName()
    if(is.null(  getPtName() )){
      ptTags<-NULL
    }
    tagRList<-getPtDefs()$df 
    if(!is.null(tagRList)){
      ptTags<-tagRList[[ptName]]
    } else {
      ptTags<-NULL
    }
    ptTags
  })
  
  usingTransformDraggable<-reactive({
    length(getCode()) >0 &&
    nchar(getCode())>0 &&
    ( 
      grepl("class\\s*=\\s*'draggable'",getCode() ) || 
      grepl('class\\s*=\\s*"draggable"',getCode() )
    )
  }) 

# Event Observers-------------------------------- 
  source("leftPanel/serverAce.R", local=TRUE) 
  

  
#help
  source("leftPanel/helpSVG.R", local=TRUE) 
  
#---navbar disable /enabler controls
  observe({
    tagsMissing<-is.null(getPtDefs()$df)
    isolate({
      if(tagsMissing){
        disableDMDM(session, "plotNavBar", "Tags")
      } else {
        enableDMDM(session, "plotNavBar", "Tags")
      }
    })
  })
  
  observe({
    using<-usingTransformDraggable()
    isolate({
      if(using){
        enableDMDM(session, "plotNavBar", "Transforms")
      } else {
        disableDMDM(session, "plotNavBar", "Transforms")
      }
    })
    
  }
  )

#------------------rightPanel--------------------------------

source("rightPanel/serverPlotBarPoints.R", local=TRUE) 
source("rightPanel/serverPlotBarTagValues.R", local=TRUE)  
source("rightPanel/serverPlotBarTagDrag.R", local=TRUE)  
source("rightPanel/serverPlotBarTransform.R", local=TRUE) 

source("rightPanel/serverLog.R", local=TRUE) 
source("rightPanel/serverPlotBar.R", local=TRUE)
source("rightPanel/serverOptions.R", local=TRUE) 
#-----MOUSE CLICKS---------------------------------
source("rightPanel/serverMouseClicks.R", local=TRUE)
source("rightPanel/serverPlotBar.R", local=TRUE)  
  
  
#---------------leftPanel--------------------------
#------buttons
source("leftPanel/serverButtons.R",local = TRUE)
#------menu
source("leftPanel/cmdFileSaveAs.R", local=TRUE)  
source("leftPanel/cmdFileSave.R", local=TRUE)  
source("leftPanel/cmdFileNew.R", local=TRUE)  
source("leftPanel/cmdFileOpen.R", local=TRUE)  
source("leftPanel/cmdFileQuit.R", local=TRUE)  
source("leftPanel/cmdFileExportSvg.R", local=TRUE) 
source("leftPanel/cmdOptionsTheme.R",local=TRUE)
source("leftPanel/cmdOptionsFontSize.R", local=TRUE)  
source("leftPanel/cmdFileSnippet.R",local=TRUE)
source("leftPanel/cmdAbout.R",local=TRUE)
source("leftPanel/serverEditBar.R",local=TRUE)
  

  


 
})
