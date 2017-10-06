#Reimplemntation of the shinyAce ctor customized for ptr


versionCheck<-function(){
# versions with an extra field in them...
    re <- regexpr("^\\d+\\.\\d+(\\.\\d+)?", utils::packageVersion("shiny"))
    shinyVer <- substr(utils::packageVersion("shiny"), 0, attr(re, "match.length"))
    minorVer <- as.integer(substr(utils::packageVersion("shiny"),
                                  attr(re, "match.length")+2,
                                  nchar(utils::packageVersion("shiny"))))
    comp <- utils::compareVersion(shinyVer, "0.9.1")
    if (comp < 0 || (comp == 0 && minorVer < 9004)){
      warning(
        "Shiny version 0.9.1.9004 required to use input debouncing in shinyAce.")
    }
}

initialPtrAceOptions<-function(
  outputId="source", #editor 
  value="",
  theme="katzenmilch",
  fontSize=16,
  mode="ptr",
  autoComplete=c("disabled", "enabled", "live"),
  autoCompleteList=NULL,
  debounce=1000, 
  selectionId=NULL, 
  useTabs="Use Soft Tabs",
  cursorId=NULL, 
  hotkeys=NULL
)
{
  sanitizeId <- function(id){
    gsub("[^[:alnum:]]", "", id)
  }
  id<-sanitizeId(outputId)
  editorVar = paste0("editor__",sanitizeId(outputId))
  acejs = file.path(system.file(package="pointR"), "App/www/Acejs")
  options<-list(
    id=sanitizeId(id),
    mode=mode,
    editorVar=editorVar,
    autoComplete='live',
    acejs=acejs
  )
  rtv<-paste0('ptRaceInit(',toJSON(options),');')
  
  rtv
}

shinyAce4Ptr <- function(
    outputId="source", #editor 
    value="",
    height="990px",
    theme="katzenmilch",
    fontSize=16,
    mode="ptr",
    autoComplete=c("disabled", "enabled", "live"),
    autoCompleteList=NULL,
    debounce=1000, 
    selectionId=NULL, 
    cursorId=NULL, 
    hotkeys=NULL
  ){
    saceList<-aceEditor(
      outputId,
      value,
      'text',
      theme=theme
    )
    
    js2<-initialPtrAceOptions(
      outputId=outputId, value=value, theme=theme, fontSize=fontSize, mode=mode,
      autoComplete=autoComplete, autoCompleteList=autoCompleteList,
      debounce=debounce, selectionId=selectionId, cursorId=cursorId, hotkeys=hotkeys
    )
    
    
    ptRAce<-tagList(
     tags$script(src="Acejs/aceExt.js"),
     tags$script(src='Acejs/snippets/ptr.js'), 
     tags$script(src="Acejs/ptRaceInit.js")
    )
    
    ptrList<-list(
      tags$script(type="text/javascript", HTML(js2))
    )
    #browser()
    saceList[[1]][[3]]<-c(saceList[[1]][[3]], ptRAce)
    #browser()
    rList<-list(saceList, ptrList)
    class(rList)<-c("shiny.tag.list", "list")
    rList
}


