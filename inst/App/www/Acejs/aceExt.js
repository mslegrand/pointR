// ace customizations
var ptr_HighlightedLines=[];

var helpSvgRQuery = function(topic, address){
//var helpSvgRTopic = function(query){
    // alert("help mssg");
        //var mssg="\"" + query + "\"";
        //Shiny.onInputChange("helpSvgRMssg", query );
  console.log("helpSvgRQuery");      
  var mssg= {queryTopic:topic, queryAddress:address };
  Shiny.onInputChange("helpSvgRMssg", mssg );
  return false;
};


        
/*
var loadPtrSnippetFile = function(id) {
    if (!id || snippetManager.files[id])
        return;
    var snippetFilePath = id.replace("mode", "snippets");
    snippetManager.files[id] = {};
    config.loadModule(snippetFilePath, function(m) {
        if (m) {
            snippetManager.files[id] = m;
            if (!m.snippets && m.snippetText)
                m.snippets = snippetManager.parseSnippetFile(m.snippetText);
            snippetManager.register(m.snippets || [], m.scope);
            if (m.includeScopes) {
                snippetManager.snippetMap[m.scope].includeScopes = m.includeScopes;
                m.includeScopes.forEach(function(x) {
                    loadSnippetFile("ace/mode/" + x);
                });
            }
        }
    });
};
*/

/*
$(document).ready(function(){ // not working
    $("#aceContainer").on("resize", function(evt){
      console.log("resizing");
      $("#source").resize() });
})
*/

//Used exclusively for swapping colors of svgR keywords
function getStyleRule(name) {
    var ix, 
    sheet= $('#shinyAceStyle')[0]['sheet']; //revisit this
    for (ix=0; ix<sheet.cssRules.length; ix++) {
      if (sheet.cssRules[ix].selectorText === name){
        return sheet.cssRules[ix].style;
      }        
    }
  return null;
}




Shiny.addCustomMessageHandler(
  "shinyAceExt",
      function(data) { 
        var id = data.id;
        var $el = $('#' + id);
        var editor = $el.data('aceEditor'); 
        var HighlightedLines;
        
        if(data.ptRMode){ 
          editor.getSession().setMode({path: "ace/mode/ptr", v: Date.now()});
          editor.setBehavioursEnabled(true);
        }
        if ( data.addMarker ){
          var pos = data.addMarker;
          var row1 = pos[0]; 
          var col1 = pos[1]; 
          var row2 = row1+1; 
          
          var Range = ace.require("ace/range").Range;
          var mid= editor.getSession().addMarker(
              new Range(row1, 0, row2, 1), 
              'ace_error-marker', 
              'line', true
          );
          ptr_HighlightedLines.push(mid); 
        }
        if(data.removeAllMarkers){
          while(ptr_HighlightedLines.length>0){
            var highlightedLine = ptr_HighlightedLines.pop();
            editor.getSession().removeMarker(highlightedLine);
          }      
        }
        if(data.tabSize){
          //editor.getSession().setUseSoftTabs(true);
          editor.getSession().setTabSize( data.tabSize );
        }
        if(data.resetElementColor){
          $.each(data.resetElementColor, function(key,element){
            var rule=getStyleRule(key);
            rule.color=element;
           });
        }
        if(data.snippets){
          var snippetManager = ace.require("ace/snippets").snippetManager;
          var m = snippetManager.files[editor.session.$mode.$id];
          m.snippetText = data.snippets;
          if (m.snippets){
            snippetManager.unregister(m.snippets);
          }
          m.snippets = snippetManager.parseSnippetFile(m.snippetText, m.scope);
          snippetManager.register(m.snippets);
        }
        if(data.toggleWhiteSpace){
          //console.log(JSON.stringify(data))
          editor.setShowInvisibles(!editor.getShowInvisibles());
        }
        if(data.toggleTabType){
          editor.session.setUseSoftTabs(!editor.session.getUseSoftTabs());
        }
        if(data.setfocus){
          editor.focus();
        }
        
        
        if(data.showKeyboardShortCuts){
          ace.config.loadModule(
            'ace/ext/keybinding_menu', function(module) {
                module.init(editor);
                editor.showKeyboardShortcuts();
          });
        }
        
        if(data.getKeyboardShortcuts){
          //var kb=ace.ext.get_editor_keyboard_shortcuts.getEditorKeybordShortcuts(editor);
          //Shiny.onInputChange("keyBoardHelp",kb);
          ace.config.loadModule('ace/ext/keybinding_menu', function(module) {
            //var require
            var kb=module.getEditorKeybordShortcuts.getEditorKeybordShortcuts(editor);
          });
            
          }
          // update a given Range
          
          
            //'ace/ext/menu_tools/get_editor_keyboard_shortcuts', function(module) {
             //var kb=require("ace.ext.menu_tools.get_editor_keyboard_shortcuts").getEditorKeybordShortcuts(editor); 
              
              //sendCustomMessage to open window and display kb
              //Shiny.onInputChange("keyBoardHelp",kb);
          //});
          
            //'ace/ext/menu_tools/get_editor_keyboard_shortcuts', function(module) 
          //var kb=editor.getEditorKeybordShortcuts();
          //sendCustomMessage to open window and display kb
          //Shiny.onInputChange("keyBoardHelp",kb);
          //});
        //}
        
        // want to set json options
        //todo: add more messaging capablilities
      }
  );
  
  
