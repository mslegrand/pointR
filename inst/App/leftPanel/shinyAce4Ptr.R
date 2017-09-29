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
sanitizeId <- function(id){
  gsub("[^[:alnum:]]", "", id)
}

initialAceOptions<-function(
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
  editorVar = paste0("editor__",sanitizeId(outputId))
  escapedId <- gsub("\\.", "\\\\\\\\.", outputId)
  escapedId <- gsub("\\:", "\\\\\\\\:", escapedId)
  autoComplete <- match.arg(autoComplete)
  
  useTabsTF<-function(x){
    ifelse(x=="Use Soft Tabs", "true","false")
  }
  useTabs<-useTabsTF(useTabs)
  
  mode='ptr'
  js<-c(
    paste0("var ", editorVar," = ace.edit('",outputId,"')"),
    paste0(editorVar,".setTheme('ace/theme/", theme, "')" ),
    #paste0(editorVar,".setKeyboardHandler('ace/keyboard/vim')"),
    "ace.config.set('modePath', './Acejs')",
    "ace.config.set('workerPath', './Acejs')",
    paste0(editorVar, ".getSession().setMode('ace/mode/",mode,"')"),
    paste0(editorVar, ".getSession().setOption('useWorker', true)"),
    paste0(editorVar, ".setValue(", shinyAce:::jsQuote(value), ",-1)"),
    paste0(editorVar, ".renderer.setShowGutter(true);"),
    paste0(editorVar, ".setHighlightActiveLine(true);"),
    paste0(editorVar, ".getSession().setUseSoftTabs(",useTabs,");"),
    #paste0(editorVar, ".setReadOnly(", shinyAce:::jsQuote(readOnly), ")"),
    paste0("document.getElementById('",outputId,"').style.fontSize='", as.numeric(fontSize), "px'"),
    if (!is.null(debounce) && !is.na(as.numeric(debounce))){
    paste0("$('#",outputId,"').data('debounce',",debounce,")")
    },
    if(!is.null(selectionId)){
    paste0(editorVar,'.getSelection().on("changeSelection", ',
    'function(){Shiny.onInputChange("', selectionId, '",', editorVar, ".getCopyText())})")
    },
    if(!is.null(cursorId)){
    paste0(editorVar,'.getSelection().on("changeCursor", ',
    'function(){Shiny.onInputChange("', cursorId, '",', editorVar, ".getCopyText())})")
    },
    paste0(editorVar, ".getSession().setUseWrapMode(false)"),
    paste0("$('#",escapedId,"').data('aceEditor',", editorVar,")"),
    
   
    
    paste0(editorVar, ".setOption('enableSnippets', true)"),
    if(autoComplete != "disabled") {
      paste0(editorVar, ".setOption('enableBasicAutocompletion', true)")
    },
    if(autoComplete != "live") {
      paste0(editorVar, ".setOption('enableLiveAutocompletion', true)")

    },
    paste0(editorVar, ".commands.addCommand({
                name: 'showKeyboardShortcuts',
                bindKey: {win: 'Ctrl-Alt-h', mac: 'Command-Alt-h'},
                exec: function(editor) {
                ace.config.loadModule('ace/ext/keybinding_menu', function(module) {
                module.init(editor);
                editor.showKeyboardShortcuts()
                })
                }
    })"),
    paste0(editorVar, ".commands.addCommand({
      name: 'commitSource',
      bindKey: {win: 'Ctrl-Shift-Enter', mac: 'Command-Shift-Enter'},
      exec: function(editor) {
          var randomString = function(length) {
              var text = '';
              var possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
              for(var i = 0; i < length; i++) {
                  text += possible.charAt(Math.floor(Math.random() * possible.length));
            }
            return text;
          }
          Shiny.onInputChange('commitMssg', randomString(5) );
      }
  })"),
  gsub( "editorVar", editorVar,
  "editorVar.commands.addCommand({
      name: 'helpR',
      bindKey: {win: 'F1', mac: 'F1'},
      exec: function(editor) {
          console.log('helpR!');
          editor.getSession().selection.moveCursorLongWordLeft();
          editor.getSession().selection.selectWordRight();
          var text = editor.getSession().getTextRange(editor.getSelectionRange());
          Shiny.onInputChange('helpMssg', {query:text, num:Math.random(), editorId:'editorVar'} );
      }
  })")
)

 for (i in seq_along(hotkeys)) {
    shortcut = hotkeys[[i]]
    if (is.list(shortcut)) {
      shortcut = paste0(names(shortcut),": '", shortcut,"'", collapse=", ")
    } else {
      shortcut = paste0("win: '",shortcut,"',  mac: '",shortcut,"'")
    }
    
    id = names(hotkeys)[i]
    code = paste0("
      ",editorVar,".commands.addCommand({
      name: '",id,"',
      bindKey: {", shortcut,"},
      exec: function(",editorVar,") {
      Shiny.onInputChange(\"",id,
      "\",{
      editorId : '",outputId,"',
      selection: ", editorVar,".session.getTextRange(",editorVar,".getSelectionRange()), 
      cursor : ", editorVar,".selection.getCursor(),
      randNum : Math.random()
      });            
      },
      readOnly: true // false if this command should not apply in readOnly mode
  });    
                  ")
    js =c(js, code)
  }
  js<-paste(js,collapse=";\n")
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
    js<-initialAceOptions(
        outputId=outputId, value=value, theme=theme, fontSize=fontSize, mode=mode,
        autoComplete=autoComplete, autoCompleteList=autoCompleteList,
        debounce=debounce, selectionId=selectionId, cursorId=cursorId, hotkeys=hotkeys
    )
    #style=paste("height:", validateCssUnit(height)),
    tagList(
      singleton(tags$head(
        shinyAce:::initResourcePaths(),
        tags$script(src = 'shinyAce/ace/ace.js'),
        tags$script(src = 'shinyAce/ace/ext-language_tools.js'),
        tags$script(src = 'shinyAce/shinyAce.js'),
        tags$script(src = 'Acejs/aceExt.js'),
        tags$script(src = 'Acejs/snippets/ptr.js')
      )),
      pre(id=outputId, 
         class="shiny-ace", 
         
        `data-autoCompleteList` = autoCompleteList
      ),
      tags$script(type="text/javascript", HTML(js))
    )
}
