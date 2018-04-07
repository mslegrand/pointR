// ace customizations
var ptr_HighlightedLines=[]; // todo: integrate back into editoR
                            // using $el.data('highlightedLines')
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
    sheet= $('#shinyAceStyle')[0].sheet; //['sheet']; //revisit this
    for (ix=0; ix<sheet.cssRules.length; ix++) {
      if (sheet.cssRules[ix].selectorText === name){
        return sheet.cssRules[ix].style;
      }        
    }
  return null;
}


function PrintR(editoR){
  ace.require("ace/config").loadModule("ace/ext/static_highlight", function(m) {
    var result = m.renderSync(
        editoR.getValue(), editoR.session.getMode(), editoR.renderer.theme
    );
    document.body.style.display="none";
    var d = document.createElement("div");
    d.innerHTML=result.html;
    document.documentElement.appendChild(d);
    ace.require("ace/lib/dom").importCssString(result.css);

    setTimeout(function() {window.print()}, 10);

    window.addEventListener("focus", function restore() {
       window.removeEventListener("focus", restore, false);
       d.parentNode.removeChild(d);
       document.body.style.display= "";
       editoR.resize(true);
    }, false);
  });
}

function previousBookMark(ed){
                var curRow=ed.getCursorPosition().row;// getCursorPosition().row
                if(curRow>0){
                    var breakpoints=ed.getSession().getBreakpoints().slice(0,curRow-1);
                    var nextRow=breakpoints.lastIndexOf('ace_breakpoint');
                    if(nextRow>=0){
                      //var rng=new Range(curRow,0, nextRow,0);
                      //ed.revealRange(rng, true);
                      ed.gotoLine(nextRow);
                    }
                }
}

function nextBookMark(ed){
    var curRow=ed.getCursorPosition().row;// getCursorPosition().row
              var breakpoints=ed.getSession().getBreakpoints().slice(curRow+1);
              // iterate over breakpoints starting at curRow+1 and 
              // stop and process if any
              var nextRow=breakpoints.indexOf('ace_breakpoint');
              if(nextRow>=0){
                //var rng=new Range(curRow,0,curRow+1+nextRow,0);
                //editor.revealRange(rng, true);
                
                ed.gotoLine(nextRow);
              }
}   


Shiny.addCustomMessageHandler(
  "shinyAceExt",
      function(data) { 
        console.log(
          '------------Entering  aceExt.js customMessageHandler-------------------------------- '
        );
        var id = data.id;
        console.log('ace id =' + JSON.stringify(id));
        
        if(id.length===0){ // nothing to process
          Shiny.onInputChange('messageFromAce', 
          {
             code : "",
             sender : data.sender,
             id : 0,
             //dirty: false,
             isSaved: true,
             docFilePath: "",
             rnd : randomString(5)
          });
          return false;
        }
        
        var $el = $('#' + id);
        var editor = $el.data('aceEditor'); 
        var Range = ace.require("ace/range").Range;
        var ud =  editor.getSession().getUndoManager();
        var sender=data.sender;
        var auxValue="";
        
        var HighlightedLines;
        
        
        
        console.log('id=' + id );
        console.log('sender =' + sender);
        if(!!editor){
          console.log('found editor');
        } else {
          console.log('editor is null');
        }
        
        if(!!data.setDocFilePath){ 
          data.oldFilePath=$el.data('docFilePath');
          console.log('setting  docFilePath=' + data.setDocFilePath);
          $el.data('docFilePath', data.setDocFilePath);
          ud.markClean();
          //$el.data('docFileSaved',-1);
        }
        
        if(!!data.setDocFileSaved){
          ud=editor.getSession().getUndoManager();
         // $el.data('docFileSaved',ud.$undoStack.length);
         ud.markClean()
        }
        
        //console.log( '$el.data(docFilePath)=',JSON.stringify( $el.data('docFilePath') ));
        
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
        
        if(!!data.tbMssg){
          console.log('!!data.tbMssg=' + data.tbMssg);
          if(data.tbMssg==='print'){
             PrintR(editor);
          }else if(data.tbMssg==='deleteAllBookMarks' ){
            editor.getSession().clearBreakpoints();
          }else if(data.tbMssg==='nextBookMark' ){
            nextBookMark(editor);
          }else if(data.tbMssg==='previousBookMark' ){
            previousBookMark(editor);
          } else {
            editor.execCommand(data.tbMssg);
          }
          editor.focus();
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
        
        if(!!data.toggleWhiteSpace){
          editor.setShowInvisibles(!editor.getShowInvisibles());
        }
        
        if(!!data.toggleTabType){
          editor.session.setUseSoftTabs(!editor.session.getUseSoftTabs());
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
             //dirty: editor.getSession().getUndoManager().dirtyCounter,
             isSaved: ud.isClean(),
             sender : data.sender,
             id:id
          });
       }
       
       if(!!data.setClean){
         //editor.getSession().getUndoManager().dirtyCounter=0;
         Shiny.onInputChange('messageFromAce', 
          {
             code :  editor.getSession().getValue(),
             sender : data.sender,
             id : id,
             //dirty: editor.getSession().getUndoManager().dirtyCounter,
             isSaved: ud.isClean(),
             rnd : randomString(5)
          });
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
                 id : id,
                 //dirty: editor.getSession().getUndoManager().dirtyCounter,
                 isSaved: ud.isClean(),
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
          editor.getSession().setValue(data.setValue);
          if(!!data.ok){
            editor.getSession().getUndoManager().setOk();
          }
          if(['cmd.openFileNow','cmd.file.new'].indexOf(sender)>=0){
            editor.getSession().clearBreakpoints();
          }
          Shiny.onInputChange('messageFromAce', 
          {
             code : editor.getSession().getValue(),
             sender : data.sender,
             id : id,
             //dirty: editor.getSession().getUndoManager().dirtyCounter,
             isSaved: ud.isClean(),
             rnd : randomString(5)
          });
        }
        
        //------------getValue----------------------------
        
        if(!!data.getValue){
          
          if(!!data.rollBack){
            ud=editor.getSession().getUndoManager();
            if( ud.$ok.length>0 ){ // only replace if we can roll back to a good state
              ud.pop2Ok();
              editor.getSession().setUndoManager(ud); //probably not necessary
            }
          }
          auxValue='';
          if(!!data.auxValue){
            auxValue=data.auxValue;
          }
          
          Shiny.onInputChange('messageFromAce', 
          {
             code :  editor.getSession().getValue(),
             sender : data.sender,
             id : id,
             //dirty: editor.getSession().getUndoManager().dirtyCounter,
             isSaved: ud.isClean(),
             auxValue: auxValue,
             rnd : randomString(5)
          });
        }
        
        //------------get saved status----------------------------
        // return status of saved
        if(!!data.getDocFileSaved){
          Shiny.onInputChange('messageFromAce', 
          {
             code :  editor.getSession().getValue(),
             sender : data.sender,
             id : id,
             dirty: editor.getSession().getUndoManager().dirtyCounter,
             //saved: $el.data('docFileSaved')==editor.getSession().getUndoManager().$undoStack.length,
             isSaved: ud.isClean(),
             docFilePath: $el.data('docFilePath'),
             rnd : randomString(5)
          });
        }
        
        //------------getDoc-------------------
        if(!!data.getDoc){ 
          //var undoLen=editor.getSession().getUndoManager().$undoStack.length;
          //var saveLen= $el.data('docFileSaved');
          if(! data.oldFilePath){
            data.oldFilePath='?';
          }
          console.log('getDoc:: docFileSaved=' +JSON.stringify(ud.isClean()) );
          Shiny.onInputChange('messageFromAce', 
          {
             code :  editor.getSession().getValue(),
             sender : data.sender,
             id : id,
             //dirty: editor.getSession().getUndoManager().dirtyCounter,
             //saved: undoLen===saveLen,
             isSaved: ud.isClean(),
             docFilePath: $el.data('docFilePath'),
             priorFilePath: data.oldFilePath,
             rnd : randomString(5)
          });
        }
        //----------------------
        
        //todo: add more messaging capablilities
      }
  );
  
  
