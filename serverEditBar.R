observeEvent( input$editNavBar, { 
  fileCmd<-input$editNavBar
  if(fileCmd=="New"){ #-----new
    updateNavbarPage(session, "plotNavBar", selected ="Points")
    src<-codeTemplate
    # the next  line update the ptRList; probably should redo with observer
    file$name<-"newSVG.R"
    user$code<-src
    selectedPoint$point.index<-0
    selectedPoint$name<-"x"
    reactiveTag$freq<-list()
    displayOptions$insertMode=TRUE
    displayOptions$showGrid=TRUE
    displayOptions$ptMode="Normal"
    mssg$error<-""
    updateSelectInput(session, "ptRSelect",  choices=c("x"), selected="x" ) 
    updateNavbarPage(session, "editNavBar", selected ="tab1") 
    
    updateNavbarPage(session, "tagFreq", selected ="Off") 
    #session$sendCustomMessage(type = "shinyAceExt", list(id= "source", ptRMode=TRUE))
    updateAceEditor( session,"source", value=user$code)
  }
  
  if(fileCmd=="Open"){ #-----open 
    #mssg$error<-""
    #session$sendCustomMessage(type = "shinyAceExt", list(id= "source", ptRMode=TRUE))
    fileName=""
    try(fileName<-dlgOpen(title = "Select one R file", 
                          filters = dlgFilters[c("R", "All"), ])$res
    )
    if(length(fileName)>0 && nchar(fileName)>0){ 
      src<-paste(readLines(fileName), collapse = "\n")
      file$name<-fileName
      if(nchar(src)>0){
        src<-preProcCode(src)
        user$code<-src
        reactiveTag$freq<-list()
        displayOptions$insertMode=TRUE
        displayOptions$showGrid=TRUE
        displayOptions$ptMode="Normal"
        mssg$error<-""
      }
    }
    updateNavbarPage(session, "editNavBar", selected ="tab1")
    updateNavbarPage(session, "plotNavBar", selected ="Points") 
    updateNavbarPage(session, "tagFreq", selected ="Off") 
  }
  if(fileCmd=="Save"){ #-----save
    fileName=""
    default="newfile.R"
    try(fileName<-dlgSave(title = "Save R script to", 
                          filters = dlgFilters[c("R", "All"), ])$res
    )
    if(length(fileName)>0 && fileName!=""){ 
      file$name<-fileName
      txt<-user$code
      writeLines(txt, fileName)
    }
    updateNavbarPage(session, "editNavBar", selected ="tab1")
  }
  if(fileCmd=="Export as SVG"){ #-----save
    fileName=""
    default="newfile.svg"
    try(fileName<-dlgSave(title = "Save R script to", 
                          filters = dlgFilters[c("R", "All"), ])$res
    )
    if(fileName!=""){ 
      file$name<-fileName
      src<-user$code
      parsedCode<-parse(text=src)
      txt<-as.character(eval(parsedCode))
      writeLines(txt, fileName)
    }
    updateNavbarPage(session, "editNavBar", selected ="tab1")
  }
  if(fileCmd=="Theme"){
    themes<-getAceThemes()
    modalTheme <- function() {
      modalDialog(
        selectInput("selectTheme", "Theme", themes, multiple=FALSE, 
                    selectize = FALSE, width="300px", size=1  ), 
        footer = tagList(actionButton("modalThemeCancel", "Cancel"),actionButton("modalThemeOk", "OK") )
      ) 
    }
    showModal( modalTheme() )
  } 
  if(fileCmd=="Font Size"){
    fontsSizes<-6:36
    modalFontSize <- function() {
      modalDialog(
        selectInput("selectFontSize", "Select Font Size", fontsSizes, multiple=FALSE, 
                    selectize = FALSE, width="90px", size=1  ), 
        footer = tagList(actionButton("modalFontSizeCancel", "Cancel"),actionButton("modalFontSizeOk", "OK") )
      ) 
    }
    showModal( modalFontSize() )
  }
  if(fileCmd=="Indentation"){
    indentSizes<-1:12
    modalIndentSize <- function() {
      modalDialog(
        selectInput("IndentSize", "Select Indent", indentSizes, multiple=FALSE, 
                    selectize = FALSE, width="90px", size=1  ), 
        footer = tagList(actionButton("modalIndentSizeCancel", "Cancel"),actionButton("modalIndentSizeOk", "OK") )
      ) 
    }
    showModal( modalIndentSize() )
  }
  
}) 

#- editor options handlers

observeEvent(input$modalIndentSizeCancel, {
  updateNavbarPage(session, "editNavBar", selected ="tab1")
  removeModal()
}) 

observeEvent(input$modalIndentSizeOk, {
  nextSize<-input$selectIndentSize
  session$sendCustomMessage(type = "shinyAceExt",  
        list(id= "source", tabSize=as.numeric(nextSize))) 
  updateNavbarPage(session, "editNavBar", selected ="tab1")  
  removeModal()
})

observeEvent(input$modalFontSizeCancel, {
  updateNavbarPage(session, "editNavBar", selected ="tab1")
  removeModal()
}) 

observeEvent(input$modalFontSizeOk, {
  #nextSize<-select.list(choices = 6:24, graphics=TRUE)
  nextSize<-input$selectFontSize
  updateAceEditor(session, "source", fontSize=as.numeric(nextSize) )
  updateNavbarPage(session, "editNavBar", selected ="tab1")  
  removeModal()
})

observeEvent(input$modalThemeCancel, {
  updateNavbarPage(session, "editNavBar", selected ="tab1")
  removeModal()
}) 

observeEvent(input$modalThemeOk, {
  nextTheme<-input$selectTheme
  if(nextTheme!=""){
    updateAceEditor(session, "source", theme=nextTheme) 
  } 
  updateNavbarPage(session, "editNavBar", selected ="tab1")
  
  isDark<-function(nt){
    nt %in% c(
      "ambiance","chaos","clouds_midnight","cobalt","idle_fingers",
      "kr_theme","merbivore","merbivore_soft","mono_industrial","monokai",
      "pastel_on_dark","solarized_dark","terminal","tomorrow_night","tomorrow_night_blue",
      "tomorrow_night_bright","tomorrow_night_eighties","twilight","vibrant_ink"
    )
  }
  colorSet<-list(
    ".ace_svgRAN"=c(255,140,0), ".ace_svgRME"=c(112,128,144),
    ".ace_svgRFE" =c(0,255,255), ".ace_svgRGR" =c(255,0,255),
    ".ace_svgRSH" =c(0,0,205), ".ace_svgRCO" =c(172,61,239),
    ".ace_svgRTX" =c(178,34,34), ".ace_svgRFI" =c(0,128,0),
    ".ace_svgRMM" =c(240,82,45) )
  
  colorSet<-lapply(colorSet, function(col){
    if(isDark(nextTheme)){
      col<-pmin(255,col+160)
    }
    paste0("rgb(", paste0(col,collapse=","), ")")
  })

  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", resetElementColor=colorSet)
  ) 
  removeModal()
})


#------fileName-------------
output$fileName <- renderText({ 
  fileName<-file$name
  if(is.null(fileName) ){
    fileName==""
  }
  paste("Editing", basename(fileName))
})

# -----------ACE EDITOR------------------------
observeEvent(
  user$code, {
    if(mssg$error==""){
      updateAceEditor( session,"source", value=user$code)
    }
  }
)

