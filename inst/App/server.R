
# This is the server logic for a Shiny web application
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#---begin server--------------
shinyServer(function(input, output,session) {
  
  js$disableMenu('plotNavBar li:nth-child(2)') #this disable Tags Menu
  js$disableTab("Transforms")
  
  #ordinary fns
  exGetTagName<-function(tagNameChoices, ptChosen){
    if(length(tagNameChoices)>0){
      if(length(ptChosen)>0 && (ptChosen %in% tagNameChoices)){
        tagChosen<-ptChosen
      } else {
        tagChosen<-tail(tagNameChoices,1)
      }
    } else {
      tagChosen<-NULL
    }
    tagChosen
  }
  
  
  exGetTagIndx<-function(tagIndxChoices, point.indx){
    if(length(tagIndxChoices)<1 ){
     return(NULL)
    }
    if(  length(point.indx)<1){
     point.index<-max(tagIndxChoices)
    } else {
      if( point.indx>0 ){
          t.point.indx<-max(tagIndxChoices[ tagIndxChoices<= point.indx] )
      } else {
          0
      }    
      }
 
  }  
  
# Reactive values----------
  #Eventually we want a stack of code changes so we can do an undo
  user <- reactiveValues( code=codeTemplate) #  internal copy of user code
  backup<-reactiveValues( code=codeTemplate) # last good copy of user code
  #file <-reactiveValues( name="newFile.R")       #  file path
  selectedPoint <- reactiveValues(
    name="x", #NULL,       # name of current point array
    point.index=0    #  selected pt.indx (column) in current point array
  ) 
  #tagVal<-reactiveValues(hasTag=FALSE)
  reactiveTag<-reactiveValues(freq=list())
  displayOptions<-reactiveValues(
    insertMode=TRUE,
    showGrid=TRUE,
    ptMode="Normal"
  )
  mssg<-reactiveValues(error="") 
  
  # Reactive expressions------------- 
   #---
  isTaggable<-reactive({ 
    name<-getPtName()
    !is.null(name) && getPtIndex()>0 &&  is.null(reactiveTag$freq[[name]])
  })
  getCode<-reactive({ user$code })
  getCodeBackup<-reactive({ backup$code })
  
  getPtName<-reactive({selectedPoint$name})
  getPtIndex<-reactive({selectedPoint$point.index})
  #-----------------------
  barName<-reactive({input$plotNavBar})
  getErrorMssg<-reactive({ mssg$error })
  getPtDefs<- reactive({ ex.getPtDefs(user$code) })  #extract points from user code
  getTagNameChoices<-reactive({
    intersect(names(getPtDefs()$pts), names(getPtDefs()$df))
  })
  getSelectInfo<-reactive({ #used by pointsBar only??
    name<-getPtName()
    indx<-getPtIndex()
    pts<-getPtDefs()$pts
    ex.getSelectInfo(pts, name, indx)
    #ex.getSelectInfo(getPtDefs()$pts, getPtName(), getPtIndex())
  })
  
  getPts<-reactive({
    ptdef<-getPtDefs()
    ptdef[[getPtName()]]
  })
  
  getTagName<-reactive({exGetTagName( getTagNameChoices(), getPtName() )})
  getTagIndexChoices<-reactive({getPtDefs()$df[[getTagName()]]$tag})
  getTagIndex<-reactive({ 
    choices<-getTagIndexChoices()
    indx<-getPtIndex()
    exGetTagIndx(choices, indx )
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
      ptTags<NULL
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
    grepl("class='draggable'",user$code) ||
    grepl('class="draggable"',user$code)
  }) 

# Event Observers-------------------------------- 
  
  
  #---- help popup  
  modalHelp <- function( htmlHelp) {
    modalDialog(
      div( 
        HTML(htmlHelp)
      ),
      size = "m",
      title="Help",
      easyClose = TRUE
    ) 
  }
  #----observer that triggers help popup  
  observe({
    query<-input$helpMssg
    if(length(query)>0 && nchar(query)>0){
      queryHelp2Html<-function(query){
        pkg<-"svgR"
        pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 'help', pkg))
        topics = names(pkgRdDB)
        if(!query %in% topics){
          query<-'svgR'
        }
        txtConnection<-textConnection("html","w")
        tools::Rd2HTML(pkgRdDB[[query]],out=txtConnection)
        close(txtConnection)
        html<-paste(html,collapse="\n") 
        html
      } 
      htmlFile<-queryHelp2Html(query)
      showModal( modalHelp(htmlFile) )
      #tmp<-help(helpMssg, package = "svgR", help_type = "html")
      #eval(tmp) #should bring help in browser, but fails
    }    
  })
  
  
  
#---navbar disable /enabler controls
  observe({
    tagsMissing<-is.null(getPtDefs()$df)
    isolate({
      name<-'plotNavBar li:nth-child(2)'
      if(tagsMissing){
        js$disableMenu(name)
      } else {
        js$enableMenu(name)
      }
    })  
  })
  
  
  
  observe({
        using<-usingTransformDraggable()
    if(using){
      js$enableTab("Transforms")
      
    } else {
      js$disableTab("Transforms")
    }
  }
  )
  
  

#--------------------------------------------------

source("rightPanel/serverPlotBarPoints.R", local=TRUE) 
# --------------input$plotNavBar=="Tags"----------------  
source("rightPanel/serverPlotBarTagValues.R", local=TRUE)  
source("rightPanel/serverPlotBarTagDrag.R", local=TRUE)  
source("rightPanel/serverPlotBarTransform.R", local=TRUE)  

#---------------Button handlers--------------------------
source("leftPanel/serverButtons.R",local = TRUE)

  
#--------------------------------navbarMenuBar--------


editOption<-reactiveValues(
  fontSize=defaultOpts ['fontSize'],
  theme=defaultOpts ['theme'],
  tabSize=defaultOpts ['tabSize'],
  currentFile=defaultOpts ['currentFile'],
  currentDirectory=defaultOpts ['currentDirectory'],
  .saved=TRUE
)
  
  getCurrentDir<-reactive({editOption$currentDirectory})
  getCurrentFile<-reactive({editOption$currentFile})
  getFileNameStatus<-reactive({editOption$currentFile!=""})
  getFileSavedStatus<-reactive({editOption$.saved})
  
source("leftPanel/cmdFileSaveAs.R", local=TRUE)  
source("leftPanel/cmdFileSave.R", local=TRUE)  
source("leftPanel/cmdFileNew.R", local=TRUE)  
source("leftPanel/cmdFileOpen.R", local=TRUE)  
source("leftPanel/cmdFileQuit.R", local=TRUE)  
source("leftPanel/cmdFileExportSvg.R", local=TRUE) 
source("leftPanel/cmdOptionsTheme.R",local=TRUE)
source("leftPanel/cmdOptionsFontSize.R", local=TRUE)  
source("leftPanel/serverEditBar.R",local=TRUE)
  
#-----------------------MOUSE CLICKS---------------------------------
source("rightPanel/serverMouseClicks.R", local=TRUE)
  

  
#-----log panel---------------------------
  output$out_log <- renderText({
    mssg$error
  })


 
})
