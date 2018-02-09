// ace customizations
var ptr_HighlightedLines=[];

var helpSvgRQuery = function(topic, address){
  var mssg= {queryTopic:topic, queryAddress:address };
  Shiny.onInputChange("helpSvgRMssg", mssg );
  return false;
};


function simpleStringify (object){
    var simpleObject = {};
    for (var prop in object ){
        if (!object.hasOwnProperty(prop)){
            continue;
        }
        if (typeof(object[prop]) == 'object'){
            continue;
        }
        if (typeof(object[prop]) == 'function'){
            continue;
        }
        simpleObject[prop] = object[prop];
    }
    return JSON.stringify(simpleObject); // returns cleaned up JSON
}
        
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
        var Range = ace.require("ace/range").Range;
        var ud =  editor.getSession().getUndoManager();
        var sender=data.sender;
        var auxValue="";
        
        
        if(!!data.ptRMode){ 
          editor.getSession().setMode({path: "ace/mode/ptr", v: Date.now()});
          editor.setBehavioursEnabled(true);
        }
        if (!!data.addMarker ){
          var pos = data.addMarker;
          var row1 = pos[0]; 
          var col1 = pos[1]; 
          var row2 = row1+1; 
          
          
          var mid= editor.getSession().addMarker(
              new Range(row1, 0, row2, 1), 
              'ace_error-marker', 
              'line', true
          );
          ptr_HighlightedLines.push(mid); 
        }
        
        if(!!data.toggleComment){
          editor.toggleCommentLines();
          editor.focus();
        }
        if(!!data.indentSelection){
          editor.indent();
          editor.focus();
        }
        
        if(!!data.outdentSelection){
          editor.getSession().outdentRows( editor.getSession().getSelection().getRange() );
          editor.focus();
        }
        
        if(!!data.tbDeleteAllBookMarks){
          editor.getSession().clearBreakpoints();
        }
        
        if(!!data.nextBookMark){
            //check out https://github.com/ajaxorg/ace/issues/3351
              // get the current cursor pos
              var curRow=editor.getCursorPosition().row;// getCursorPosition().row
              var breakpoints=editor.getSession().getBreakpoints().slice(curRow+1);
              // iterate over breakpoints starting at curRow+1 and 
              // stop and process if any
              var nextRow=breakpoints.indexOf('ace_breakpoint');
              if(nextRow>=0){
                var rng=new Range(curRow,0,curRow+1+nextRow,0);
                editor.revealRange(rng, true);
              }
              editor.focus();
        }
        
        if(!!data.previousBookMark){
            //check out https://github.com/ajaxorg/ace/issues/3351
              // get the current cursor pos
              function previousBookMark(){
                var curRow=editor.getCursorPosition().row;// getCursorPosition().row
                if(curRow>0){
                    var breakpoints=editor.getSession().getBreakpoints().slice(0,curRow-1);
                    var nextRow=breakpoints.lastIndexOf('ace_breakpoint');
                    if(nextRow>=0){
                      var rng=new Range(curRow,0, nextRow,0);
                      editor.revealRange(rng, true);
                    }
                }
              }
              previousBookMark();
              editor.focus();
        }
        
        if(!!data.findNext){
          //data.findNext
          // checkout /home/sup/svgRHabitat/ptRAceBldr/TrestleTech/ace/lib/ace/search_test.js:50:
        }
        
        if(!!data.print){
          console.log('starting print');
          function Print(){
            try{
              var printWindow=window.open("","",height=400,width=800);
              printWindow.document.write("<html><head><title>Script Editor Print Window</title>");
              printWindow.document.write("</head><body><span><h4>");
              printWindow.document.write(editor.getSession().getDocument().getValue().split("\n").join("<br/>"));
              printWindow.document.write(editor.getSession().getDocument().getValue().split("\n").join("<br/>"));
              printWindow.document.close();
              printWindow.print();
            }
            catch(ex){
              console.error("Error: " + ex.message);
            }
          }
          Print();
          editor.focus();
        }
        
        if(!!data.removeAllMarkers){
          while(ptr_HighlightedLines.length>0){
            var highlightedLine = ptr_HighlightedLines.pop();
            editor.getSession().removeMarker(highlightedLine);
            editor.focus();
          }      
        }
        if(!!data.tabSize){
          //editor.getSession().setUseSoftTabs(true);
          editor.getSession().setTabSize( data.tabSize );
        }
        if(!!data.resetElementColor){
          $.each(data.resetElementColor, function(key,element){
            var rule=getStyleRule(key);
            rule.color=element;
           });
        }
        if(!!data.snippets){
          var snippetManager = ace.require("ace/snippets").snippetManager;
          var m = snippetManager.files[editor.session.$mode.$id];
          m.snippetText = data.snippets;
          if (m.snippets){
            snippetManager.unregister(m.snippets);
          }
          m.snippets = snippetManager.parseSnippetFile(m.snippetText, m.scope);
          snippetManager.register(m.snippets);
        }
        if(!!data.toggleWhiteSpace){
          //console.log(JSON.stringify(data))
          editor.setShowInvisibles(!editor.getShowInvisibles());
        }
        if(!!data.toggleTabType){
          editor.session.setUseSoftTabs(!editor.session.getUseSoftTabs());
        }
        if(!!data.setfocus){
          editor.focus();
        }
        
        
        if(!!data.showKeyboardShortCuts){
          ace.config.loadModule(
            'ace/ext/keybinding_menu', function(module) {
                module.init(editor);
                editor.showKeyboardShortcuts();
          });
        }
        
        if(!!data.getKeyboardShortcuts){
          //var kb=ace.ext.get_editor_keyboard_shortcuts.getEditorKeybordShortcuts(editor);
          //Shiny.onInputChange("keyBoardHelp",kb);
          ace.config.loadModule('ace/ext/keybinding_menu', function(module) {
            //var require
            var kb=module.getEditorKeybordShortcuts.getEditorKeybordShortcuts(editor);
          });
            
          }
          // update a given Range
       
       if(!!data.getLastOK){
          ud=editor.getSession().getUndoManager();
          if( ud.$ok.length>0 ){ // only replace if we can roll back to a good state
            ud.pop2Ok();
            editor.getSession().setUndoManager(ud); //probably not necessary
          }
          Shiny.onInputChange('messageFromAce', 
          {
             code :  editor.getSession().getValue(),
             dirty: editor.getSession().getUndoManager().dirtyCounter,
             sender : data.sender
          });
       }
       
       if(!!data.setClean){
         editor.getSession().getUndoManager().dirtyCounter=0;
         Shiny.onInputChange('messageFromAce', 
          {
             code :  editor.getSession().getValue(),
             sender : data.sender,
             dirty: editor.getSession().getUndoManager().dirtyCounter,
             rnd : randomString(5)
          });
       }
       
       
       if(!!data.undo){
          editor.getSession().getUndoManager().undo(true);
       }
       if(!!data.redo){
          editor.getSession().getUndoManager().redo(true);
       }
       
       if(!!data.foldAll){
         editor.getSession().foldAll();
       }
       if(!!data.unfoldAll){
         editor.getSession().unfold();
       }
       
       //---------------------------------
        if(!!data.replacement){
          //console.log("\n\nEntering data.replacement");
          var replacement = data.replacement;
          //console.log(JSON.stringify(replacement));
          
          ud=editor.getSession().getUndoManager();
          //console.log('1: undoManager is: ' + simpleStringify( ud ));
          /*
          console.log('1: ud.$ok is :' + JSON.stringify( ud.$ok));
          console.log("1: editor.getSession().getUndoManager().$ok=" + 
                JSON.stringify(editor.getSession().getUndoManager().$ok));
          console.log("Before pop ud.$undoStack.length=" + 
              editor.getSession().getUndoManager().$undoStack.length);
              */
          if( ud.$ok.length>0 ){ // only replace if we can roll back to a good state
            ud.pop2Ok();
            /*
            console.log('2: undoManager is: ' + simpleStringify( ud ));
            //console.log('2: ud.$ok is: ' + simpleStringify( ud.$ok ));
            console.log('2: ud.$ok is: ' + JSON.stringify( ud.$ok ));
            console.log("2: editor.getSession().getUndoManager().$ok=" + 
                JSON.stringify(editor.getSession().getUndoManager().$ok));
            */
            //ud.pop2Ok(); //!!! to do, check if pop.2Ok exists
            /*
            console.log("Before replacement u.$undoStack.length=" + 
              editor.getSession().getUndoManager().$undoStack.length + "\n ok=" +
              JSON.stringify(editor.getSession().getUndoManager().$ok) );
            */
            for(var i=0;  i< replacement.length; i++){
              let rpl = replacement[i];
              //console.log("xx "+ i + ": " + JSON.stringify(rpl));
              var rnge =  new Range(
               rpl.rng.startRow, rpl.rng.startColumn, 
               rpl.rng.endRow, rpl.rng.endColumn);
               editor.getSession().replace(rnge, rpl.txt);
               editor.getSession().getUndoManager().setOk();
            }
           //console.log("After replacement ud.$undoStack.length=" + 
           //  editor.getSession().getUndoManager().$undoStack.length);
           
          setTimeout( function(){
             editor.getSession().getUndoManager().setOk();
             /*
              console.log("replacement: After setOk, ok=" + JSON.stringify(editor.getSession().getUndoManager().$ok));
              console.log('replacement fin: editor.getSession().getUndoManager()$undoStack.length=' + 
                    editor.getSession().getUndoManager().$undoStack.length);
              console.log('replacement fin: editor.getUndoManager()getSession().$ok=' + 
                    JSON.stringify(editor.getSession().getUndoManager().$ok));
              console.log('replacement fin: sender=' + data.sender);     
              */
              Shiny.onInputChange('messageFromAce', 
              {
                 code : editor.getSession().getValue(),
                 sender : data.sender,
                 dirty: editor.getSession().getUndoManager().dirtyCounter,
                 selector: data.selector,
                 rnd : randomString(5)
              } );   
          }, 5 );
          //var lang = ace.require("ace/lib/lang");
          //lang.delayedCall(editor.getSession().getUndoManager().setOk());
          
          
          }        
        }
        //-------------------
        if(!!data.setOk){
          //console.log("\ndata.setOk");
          editor.getSession().getUndoManager().setOk();
          /*
          console.log('setOk fin: editor.getSession().getUndoManager()$undoStack.length=' + 
                editor.getSession().getUndoManager().$undoStack.length);
          console.log('setOk fin: editor.getUndoManager()getSession().$ok=' + 
                JSON.stringify(editor.getSession().getUndoManager().$ok));
          */
          
        }
        //----------------------------
        
        if(!!data.setValue){
          /*
          console.log('getValue fin: editor.getSession().getUndoManager()$undoStack.length=' + 
                editor.getSession().getUndoManager().$undoStack.length);
          console.log('getValue fin: editor.getUndoManager()getSession().$ok=' + 
                editor.getSession().getUndoManager().$ok);
          */
          //console.log('value = ' + JSON.stringify(code));
          editor.getSession().setValue(data.setValue);
          if(!!data.ok){
            //console.log('!!data.ok==TRUE');
            editor.getSession().getUndoManager().setOk();
          }
          /*
          console.log('setValue fin: editor.getSession().getUndoManager()$undoStack.length=' + 
                editor.getSession().getUndoManager().$undoStack.length);
          console.log('setValue fin: editor.getUndoManager()getSession().$ok=' + 
                JSON.stringify(editor.getSession().getUndoManager().$ok));
          //console.log('value set = ' + JSON.stringify(editor.getSession().getValue()));
          console.log('setValue fin: sender=' + data.sender); 
          */
          Shiny.onInputChange('messageFromAce', 
          {
             code : editor.getSession().getValue(),
             sender : data.sender,
             dirty: editor.getSession().getUndoManager().dirtyCounter,
             rnd : randomString(5)
          });
        }
        //----------------------------------------
        
        if(!!data.getValue){
          //console.log('get.Value');
          if(!!data.rollBack){
            ud=editor.getSession().getUndoManager();
            if( ud.$ok.length>0 ){ // only replace if we can roll back to a good state
              ud.pop2Ok();
              editor.getSession().setUndoManager(ud); //probably not necessary
            }
          }
          /*
          console.log('getValue fin: editor.getSession().getUndoManager()$undoStack.length=' + 
                editor.getSession().getUndoManager().$undoStack.length);
          console.log('getValue fin: editor.getUndoManager()getSession().$ok=' + 
                JSON.stringify(editor.getSession().getUndoManager().$ok));
          console.log('getValue fin: sender=' + data.sender);
          */
          
          auxValue='';
          if(!!data.auxValue){
            auxValue=data.auxValue;
          }
          
          Shiny.onInputChange('messageFromAce', 
          {
             code :  editor.getSession().getValue(),
             sender : data.sender,
             dirty: editor.getSession().getUndoManager().dirtyCounter,
             auxValue: auxValue,
             rnd : randomString(5)
          });
        }
        //----------------------
        
        //todo: add more messaging capablilities
      }
  );
  
  
