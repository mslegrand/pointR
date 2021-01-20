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
  theme="chrome",
  fontSize=16,
  tabSize=2,
  mode="ptr",
  autoComplete=c("disabled", "enabled", "live"),
  autoCompleteList=NULL,
  debounce=1000, 
  selectionId=NULL, 
  useTabs="Use Soft Tabs",
  cursorId=NULL, 
  hotkeys=NULL,
  docFilePath='?',
  initSaved=FALSE,
  link=NULL
)
{
  sanitizeId <- function(id){
    gsub("[^[:alnum:]]", "", id)
  }
  id<-sanitizeId(outputId)
 
  # cat('sanitizeId(outputId)=', id,'\n')
  editorVar = paste0("editor__",sanitizeId(outputId))
  acejs = file.path(system.file(package="pointR"), "App/www/Acejs")
  options<-list(
    id=outputId,
    mode=mode,
    editorVar=editorVar,
    autoComplete='live',
    autoCompleteList=autoCompleteList,
    acejs=acejs,
    docFilePath=docFilePath,
    initSaved=initSaved,
    link=link
  )
  rtv<-paste0('ptRaceInit(',toJSON(options),');')
  # cat('initialPtrAceOptions:: options')
  # print(options)
  rtv
}

shinyAce4Ptr <- function(
    outputId="source", # assigned aceId editor 
    value="", # assigned text
    height="100%", 
    theme="chrome", #assigned defaultOpts["theme"]
    fontSize=16,# assigned defaultOpts["fontSize"]
    tabSize=2,
    whiteSpace=F,
    mode="ptr", #assigned
    autoComplete= "enabled", #asssigned enabled
    autoCompleteList=NULL, #assigned names(svgR:::eledefs) when mode is 'ptr
    debounce=1000, 
    selectionId=NULL, 
    cursorId=NULL, 
    hotkeys=NULL,
    docFilePath=docFilePath, #assigned
    initSaved=initSaved, #assigned
    link=NULL
  ){
  # cat('shinyAce4Ptr:: outputId=',outputId,"\n")
  
    if(is.null(theme)){
      theme<-"chrome"
    }
  
    saceList<-aceEditor(
      outputId=outputId,
      value=value,
      mode='text',
      autoComplete='disabled',
      fontSize=fontSize,
      showInvisibles=whiteSpace,
      tabSize=tabSize,
      theme=theme,
      height='100%'
    )
    
    js2<-initialPtrAceOptions(
      outputId=outputId, value=value, theme=theme, fontSize=fontSize, mode=mode,
      autoComplete=autoComplete, 
      autoCompleteList=autoCompleteList,
      debounce=debounce, selectionId=selectionId, cursorId=cursorId, hotkeys=hotkeys, 
      docFilePath=docFilePath,
      initSaved=initSaved,
      link=link
    )
    
    
    ptRAce<-tagList(
     tags$script(src="Acejs/aceExt.js"),
     tags$script(src='Acejs/snippets/ptr.js'), 
     tags$script(src='Acejs/snippets/ptrrmd.js'), 
     tags$script(src='Acejs/snippets/dnippets.js'), 
     tags$script(src="Acejs/ptRaceInit.js")
    )
    
    ptrList<-list(
      tags$script(type="text/javascript", HTML(js2))
    )

# saceList[[2]] provided what?
 # 1. drag and drop : moved to ptRaceInit.js
 # 2. set focus
 # 3. set selection
 # 4. insert snippets
 # 5. set some keystroke reponses
 
#     saceList[[2]]<-jqui_droppable(
#         saceList[[2]],
#         operation="enable",
#         #selector ="#source", method='enable',
#         options=list(
#           activeClass= "ui-state-default",
#           hoverClass= "ui-state-hover",
#           accept= ":not(.ui-sortable-helper)",
#           drop= JS('function(event, ui) {
#                    console.log("shinyjqui:  drop occurred");
#                    console.log( "dropped on id="+ $(this).attr("id"));
#                    var theEditoR = $(this).data("aceEditor");
#                    console.log( "dropped on id="+ $(this).attr("id"));
#                    console.log("theEditoR class=" + JSON.stringify(theEditoR.className));
#                    var pos = theEditoR.renderer.screenToTextCoordinates(event.clientX, event.clientY);
#                    console.log("pos=" + JSON.stringify(pos));
#                    var txt =  ui.draggable.attr("data-snippet");
# console.log("data-snippet=" + JSON.stringify(txt));
#                    this.focus();
#                    theEditoR.moveCursorToPosition(pos);
#                    theEditoR.clearSelection();
#                    //editor.session.insert(pos, txt);
#                    //editor.insert(txt);
#                    ui.helper.remove();
#                    var snippetManager = ace.require("ace/snippets").snippetManager;
#                    snippetManager.insertSnippet(theEditoR, txt);
#                    //var tab_press= jQuery.Event("keydown", {which: 88});
#                    //var tab_press= jQuery.Event("keydown", {which: 9});
#                    //theEditor.trigger(tab_press);
#                    //theEditor.simulate("key-combo",{combo: "shift-tab"});
#                    theEditoR.focus();
#                    return true;
# }')
  # )
# )
  
      
    saceList[[1]][[3]]<-c(saceList[[1]][[3]], ptRAce)
    
      
    rList<-list(saceList, ptrList)
    class(rList)<-c("shiny.tag.list", "list")
    rList
}


