
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {
  
source("util/exGetTag.R",               local=TRUE) # some ordinary functions :)
  
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
    sender='startup',
    refresh=NULL, # to be used to force a code refresh???
    inputHandlers=NULL
  )
  
  getCode<-reactive({request$code})
  
  
  triggerRefresh<-function(sender, rollBack=TRUE, auxValue=FALSE){ # to be used to force a code refresh???
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= "source", sender=sender, getValue=TRUE, rollBack=rollBack, auxValue=auxValue)
    )
  }
  
 
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
  

  
#help
  
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

source("leftPanel/mid/serverAce.R",                local=TRUE) 
source("leftPanel/helpSVG.R",                      local=TRUE) 

#------------------rightPanel--------------------------------
source("rightPanel/serverHandler.R",               local=TRUE)
source("rightPanel/serverDisplayOptions.R",        local=TRUE)
source("rightPanel/serverSelection.R",             local=TRUE) 
source("rightPanel/menu/cmdNewColumn.R",           local=TRUE)
source("rightPanel/menu/cmdDeleteColumn.R",        local=TRUE)
source("rightPanel/footer/serverFooterRight.R",    local=TRUE) 
source("rightPanel/header/serverEdTib.R",          local=TRUE) 
source("rightPanel/mid/serverPlotBarPoints.R",     local=TRUE) 
source("rightPanel/mid/serverPlotBarTagValues.R",  local=TRUE)  
source("rightPanel/mid/serverPlotBarTagDrag.R",    local=TRUE)  
source("rightPanel/mid/serverPlotBarTransform.R",  local=TRUE) 
source("rightPanel/mid/serverLog.R",               local=TRUE) 
source("rightPanel/serverPlotBar.R",               local=TRUE)
source("rightPanel/serverOptions.R",               local=TRUE) 
source("rightPanel/mid/serverMouseClicks.R",       local=TRUE)
source("rightPanel/serverPlotBar.R",               local=TRUE)  
  
#---------------leftPanel--------------------------
source("leftPanel/footer/serverButtons.R",        local=TRUE)
source("leftPanel/menu/cmdFileSaveAs.R",          local=TRUE)  
source("leftPanel/menu/cmdFileSave.R",            local=TRUE)  
source("leftPanel/menu/cmdFileNew.R",             local=TRUE)  
source("leftPanel/menu/cmdFileOpen.R",            local=TRUE)  
source("leftPanel/menu/cmdFileQuit.R",            local=TRUE)  
source("leftPanel/menu/cmdFileExportSvg.R",       local=TRUE) 
source("leftPanel/menu/cmdOptionsTheme.R",        local=TRUE)
source("leftPanel/menu/cmdOptionsFontSize.R",     local=TRUE)  
source("leftPanel/menu/cmdFileSnippet.R",         local=TRUE)
source("leftPanel/menu/cmdAbout.R",               local=TRUE)
source("leftPanel/serverEditBar.R",               local=TRUE)
  

})
