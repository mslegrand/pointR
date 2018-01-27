
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {
  
  source("util/exGetTag.R", local=TRUE) # some ordinary functions :)
  
# Reactive values----------

  #-- hidden output
  output$handler<-reactive({
    rtv<-getHandler()
    if(is.null(rtv)){
      rtv<-'default'
    }
    rtv
  })
  outputOptions(output, "handler", suspendWhenHidden=FALSE)
  output$handlerValue<-reactive({
    rtv<-getHandlerValue()
    if(is.null(rtv)){
      rtv<-'default'
    }
    rtv
  })
  outputOptions(output, "handlerValue", suspendWhenHidden=FALSE)
  
  
  
  request<-reactiveValues(
    code=NULL,
    name=NULL,
    selector=list( name=NULL, selTib=NULL), # used by rightPanel
    sender='startup',
    refresh=NULL, # to be used to force a code refresh???
    inputHandlers=NULL
  )
  
  # control<-reactiveValues(
  #   selector=list( ptDefs=NULL,  name=NULL, selTib=NULL)
  # )
  
  getCode<-reactive({request$code})
  
  
  
  triggerRefresh<-function(sender, rollBack=TRUE, auxValue=FALSE){ # to be used to force a code refresh???
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= "source", sender=sender, getValue=TRUE, rollBack=rollBack, auxValue=auxValue)
    )
  }
  
  # get CntrlSelector reactive
  # getSelection<- reactive({ control$selector })
 
  # observe request, ptDefs, update cntrl selector
  # potential issue: 
  #      if request comes prior to ptDefs
  #      a row/col request change may be discarded by old existing ptDefs
  # solution:
  #      1. possibly need to add a delay or use a getCode and instead
  #      2.  
  #            a.  have ace do the update when recieving new code+request
  #            b.  Other request go through here, with modifications
  #                  i observer only on request$selector (since ptDefs change only for ace)
  # observeEvent( c( request$selector  ),{  
  #   ptDefs<-getPtDefs()
  #   tibs<-ptDefs$tib
  #   reqSelector<-request$selector
  #   cntrlSelector<-control$selector
  #   cntrlSelector<-selectorUpdate(tibs,  reqSelector, cntrlSelector )
  #   cntrlSelector$ptDefs<-ptDefs
  #   cntrlSelector$selector<-cntrlSelector
  # })
  
  panels<-reactiveValues(
    left='source',   #to be used as editor name later, for connecting to right graphics
    state='point',
    sourceType='svgPanel' #  sourceType = 'svgPanel'  means svgR code
                          #  sourceType = 'logPanel' means plain R code or error
                          #  sourceType is set from processCommit
  )
  
  setSourceType<-function( sourceType ){
    if(!missing(sourceType)){
      cat('setSourceType:: sourceType=',sourceType,"\n")
      panels$sourceType=sourceType
    }
  }
  
  getNameType<-reactive({
    if(!is.null(getTibName())){
      if (getTibName() %in% names(getPtDefs()$tib)){
        'tib'
      } else {
        getTibName()
      }
    } else {
      logTag
    }
  })
  
  
  getColumnType<-reactive({
    colName<-getTibColumnName()
    columnValues<-getTib()[[colName]]
    if(!is.null(columnValues)){
      if( is.list(columnValues) ){
        if(all(sapply(columnValues, function(m){ is.matrix(m) && dim(m)[1]==2}))){
          return('point')
        } else {
          return('list')
        }
      }
      if(is.numeric(columnValues)){
        return('numeric')
      }
      if(is.character(columnValues)){
        if( isColorString(columnValues)){
          cat('column is colourable\n\n')
          return('colourable')
        } else {
          return('character')
        }
      } else {
        return('value')
      }
    }
    return(NULL)
  })
  
  getPlotState<-reactive({
    nameType<-getNameType()
    cat('... getPlotState:  nameType=', nameType,"\n")
    if(nameType=='tib'){
      colType<-getColumnType()
      cat('... getPlotState:  colType=', colType,"\n")
      if(colType=='point'){
        cat( "getSelIndex()=",getSelIndex(),"\n")
        c('point', 'matrix')[ getSelIndex() ]
      } else {
        'value'
      }
    } else {
      nameType
    }
  })
  
  getRightMidPanel2<-reactive({
    if(panels$sourceType=='logPanel' || is.null(getPlotState() )){
      rtv<-"logPanel"
    } else {
      # rtv<-panels$right2
      rtv<-getPlotState()
    }
    cat( "getRightMidPanel2=",rtv,"\n")
    rtv
  })
  
  # getPlotState<-reactive({panels$state})
  
  # if the arg state is anything other  than 'matrix','point', 'transform', 'logPanel' 
  # this results with the state is set to 'value'
  # setPlotState<-function(state){ 
  #   cat('setPlotstate=',state,"\n")
  #   if(!is.null(state) && (state %in% c('matrix','point', 'transform', 'logPanel'))){
  #     if(state=='point' && panels$state!='point'){ 
  #       # get the number of columns of the entry
  #       entry=getTibEntry()
  #       if(!is.null(entry) && is.matrix(entry)){
  #         nc<-ncol(entry)
  #       } else {
  #         nc<-0
  #       }
  #       updateSelected(matCol=nc)
  #     }
  #     cat("setting panels$state=",state,"\n")
  #     panels$state<-state
  #   } else {
  #     cat("setting panels$state=value\n")
  #     panels$state<-'value'
  #   }
  #   # panels$right2<-panels$state
  # }
  
  getRightPanelChoices<-reactive({ # includes names of tibs
    if(panels$sourceType=='logPanel'){
      choices='logPanel'
    } else {
      ptDefs<-getPtDefs()
      choices<-names(getPtDefs()$tib)
      if( usingTransformDraggable() ){
        choices<-c(choices, transformTag)
      }
      choices<-c(choices,logTag)
    }
    choices
  })
  
  getRightPanelName<-reactive({  #used only by editTib
    if(panels$sourceType=='logPanel'){
      return('logPanel')
    } else {
      return(selectedTibble$name)
    }
  })
  
  is.tibName<-function(x){ !is.null(x) || x==logTag || x==transformTag}
  
  getTibEditState<-reactive({
    #cat("panels$state=",panels$state,"\n")
    (panels$sourceType)=='svgPanel' && (panels$state %in% c("point", "value", "matrix"))
  })

  getLeftMenuCmd<-reactive({input$editNavBar$item})
  getRightMenuCmd<-reactive({input$plotNavBar$item})
  
  
  
  
  
  
  getPtDefs<- reactive({ 
    ptDefs<-ex.getPtDefs(getCode(), useTribbleFormat=editOption$useTribbleFormat ) 
    # we kludge here and assign input handlers to ptDefs
    ptDefs
  })  # extract points from user code
  
  mssg<-reactiveValues(error="") 
  getErrorMssg<-reactive({ mssg$error })
  
 
  
  usingTransformDraggable<-reactive({
    length(getCode()) >0 &&
      nchar(getCode())>0 &&
      ( 
        grepl("class\\s*=\\s*'draggable'",getCode() ) || 
          grepl('class\\s*=\\s*"draggable"',getCode() )
      )
  }) 
  
  shinyFileChoose(input, "buttonFileOpenHidden", session=session, roots=c(wd="~") ) #hidden
  shinyFileChoose(input, "buttonSnippetOpen", session=session, roots=c(wd="~"),  filetypes=c('', 'snp') ) #hidden
  shinyFileSave(input, "buttonFileSaveHidden", session=session, roots=c(wd="~") ) #hidden
  
  
  # Reactive expressions------------- 
  showGrid<-reactive({displayOptions$showGrid})

  
  ptrDisplayScript =reactive({ 
    type=getRightMidPanel2()
    cat("\n-------------type1=",type,"\n")
    if(type=='transform'){
      type=  paste0(type,".",getTransformType() )
      cat("\n-------------type2=",type,"\n")
    }
    
    scripts<-list(
      point=    'var ptRPlotter_ptR_SVG_Point = new PtRPanelPoints("ptR_SVG_Point");',
      value=    'var ptRPlotter_ptR_SVG_TagVal = new PtRPanelTagVal("ptR_SVG_TagVal");',
      transform.Translate= 'var ptRPlotter_ptR_SVG_TRANSFORM_TRANSLATE = new PtRPanelTranslate("ptR_SVG_TRANSFORM");',
      transform.Rotate=    'var ptRPlotter_ptR_SVG_TRANSFORM_ROTATE = new PtRPanelRotate("ptR_SVG_TRANSFORM");',
      transform.Scale=     'var ptRPlotter_ptR_SVG_TRANSFORM_SCALE = new PtRPanelScale("ptR_SVG_TRANSFORM");',
      matrix=    'var ptRPlotter_ptR_SVG_TagDrag = new PtRPanelTagDrag("ptR_SVG_TagDrag");'
    )
    scripts[type]
  })
  

# Event Observers-------------------------------- 
  source("leftPanel/serverAce.R", local=TRUE) 
  

  
#help
  source("leftPanel/helpSVG.R", local=TRUE) 
  
#---navbar disable /enabler controls
  # observe({
  #   isTibble<-TRUE # !!!TODO implement simple matrices  
  #   isolate({
  #     if(isTibble){
  #       enableDMDM(session, "plotNavBar", "Tags")
  #     } else {
  #       disableDMDM(session, "plotNavBar", "Tags")
  #     }
  #   })
  # })
  
  # observe({
  #   using<-usingTransformDraggable()
  #   isolate({
  #     if(using){
  #       enableDMDM(session, "plotNavBar", "Transforms")
  #     } else {
  #       disableDMDM(session, "plotNavBar", "Transforms")
  #     }
  #   })}
  # )


#------------------rightPanel--------------------------------
source("rightPanel/serverHandler.R", local=TRUE)
source("rightPanel/serverDisplayOptions.R", local=TRUE)
source("rightPanel/serverSelection.R", local=TRUE) 
source("rightPanel/cmdNewColumn.R", local=TRUE)
source("rightPanel/cmdDeleteColumn.R", local=TRUE)
source("rightPanel/footer/serverFooterRight.R", local=TRUE) 
source("rightPanel/header/serverEdTib.R", local=TRUE) 
source("rightPanel/mid/serverPlotBarPoints.R", local=TRUE) 
source("rightPanel/mid/serverPlotBarTagValues.R", local=TRUE)  
source("rightPanel/mid/serverPlotBarTagDrag.R", local=TRUE)  
source("rightPanel/mid/serverPlotBarTransform.R", local=TRUE) 

source("rightPanel/serverLog.R", local=TRUE) 
source("rightPanel/serverPlotBar.R", local=TRUE)
source("rightPanel/serverOptions.R", local=TRUE) 
#-----MOUSE CLICKS---------------------------------
source("rightPanel/serverMouseClicks.R", local=TRUE)
source("rightPanel/serverPlotBar.R", local=TRUE)  
  
  
#---------------leftPanel--------------------------

#------buttons
source("leftPanel/footer/serverButtons.R",local = TRUE)
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
