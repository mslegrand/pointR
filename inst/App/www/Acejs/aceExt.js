// ace customizations

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


function getAceMode(ed){
  var mode = editor.getSession().$modeId;
  mode = mode.substr(mode.lastIndexOf('/') + 1);
  return mode;
}
/*
function getSaveStatus(ed){
  if(! ud.canUndo() ){
    return ud.startClean;
  } else {
    return ud.isSaved();
  }
}
*/

Shiny.addCustomMessageHandler(
  "shinyAceExt",
      function(data) { 
        //----------- define some useful aux functions--------------
        
        function previousBookMark(editor){
          var curRow=editor.getCursorPosition().row;// getCursorPosition().row
          if(curRow>0){
              var breakpoints=editor.getSession().getBreakpoints().slice(0,curRow);
              var nextRow=breakpoints.lastIndexOf('ace_breakpoint');
              if(nextRow>=0){
                editor.gotoLine(nextRow+1);
              }
          }
        }
  
        function nextBookMark(editor){
              var curRow=editor.getCursorPosition().row;// getCursorPosition().row
              var breakpoints=editor.getSession().getBreakpoints().slice(curRow+1);
              var nextRow=1+breakpoints.indexOf('ace_breakpoint');
              if(nextRow>=1){
                editor.gotoLine(curRow+1+nextRow);
              }
        } 
        
        function clearAllMarkers($el, editor){
          var bmarkers=editor.getSession().$backMarkers;
          var markers=$el.data('errorMarkerArray');
          for(var i=0; i<markers.length;i++){
            mid=markers[i];
            editor.getSession().removeMarker(mid);
          }
          bmarkers=editor.getSession().$backMarkers;
        }
        //-------------- end of aux functions------------------
        
        //
        console.log(
          '------------Entering  aceExt.js customMessageHandler-------------------------------- '
        );
        var id = data.id;
        
        //---------------id check------------------
        if(id.length===0 || id==='bogus'){ // nothing to process
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
        
        //---------------id ok, extract $el and sender------------------
        console.log('ace id =' + JSON.stringify(id));
        var $el = $('#' + id);
        if(!$el){
          console.log('cannot find #id');
        }
        console.log('id=' + JSON.stringify(id) );
        var sender=data.sender;
        console.log('sender =' + JSON.stringify(sender));
        
        //---------------extract editor ---------------
        var editor = $el.data('aceEditor'); 
        
        //---------------editor check------------------
        if(!!editor){
          console.log('found editor');
        } else {
          console.log('editor is null');
          return false;
        }  
        //-------------updateAll handlers---------------
        if(sender=='updateAll'){
              if(!!data.fontSize){
              $('.shiny-ace').each(function(){
                let lid=this.id;
                $('#'+lid).data('aceEditor').setFontSize(data.fontSize);
              });
            }
            if(!!data.theme){
              $('.shiny-ace').each(function(){
                let lid=this.id; 
                console.log('data.aceTheme='+data.theme);
                $('#'+lid).data('aceEditor').setTheme("ace/theme/" + data.theme);
              });
            }
            if(!!data.tabSize){
              $('.shiny-ace').each(function(){
                let lid=this.id; 
                console.log('data.tabSize='+data.tabSize);
                $('#'+lid).data('aceEditor').getSession().setTabSize(data.tabSize);
              });
            }
            if(!!data.whiteSpace){
              var wsCmd=(data.whiteSpace==='show');
              $('.shiny-ace').each(function(){
                let lid=this.id; 
                $('#'+lid).data('aceEditor').setShowInvisibles(wsCmd);
              });
            }
            return null;
        }
        //---------------extract ud check------------------
        var Range = ace.require("ace/range").Range;
        var ud =  editor.getSession().getUndoManager();
        
        var auxValue="";
        
        var HighlightedLines;
        var aceMode = editor.getSession().$modeId;
        aceMode = aceMode.substr(aceMode.lastIndexOf('/') + 1);
        
        

        //---------------setDocFilePath------------------
        if(!!data.setDocFilePath){ 
          data.oldFilePath=$el.data('docFilePath');
          console.log('setting  docFilePath=' + data.setDocFilePath);
          $el.data('docFilePath', data.setDocFilePath);
        }
        
        //---------------setDocFileSaved------------------
        if(!!data.setDocFileSaved){
          ud=editor.getSession().getUndoManager();
          if( data.setDocFileSaved===true){
            ud.setSaved();
          }
        }
        
        if(!!data.setMode){
          if(aceMode!=data.setMode){
                 console.log('setting  mode=' + data.setMode);
                editor.getSession().setMode({path: "ace/mode/" +data.setMode, v: Date.now()});
                editor.setBehavioursEnabled(true);
          }
        } 
        
        //---------------ptRMode------------------
        if(!!data.ptRMode){ 
          editor.getSession().setMode({path: "ace/mode/ptr", v: Date.now()});
          editor.setBehavioursEnabled(true);
        } 
        
        //if(!!data.getMode){
        // var mode = editor.getSession().$modeId;
        //  mode = mode.substr(mode.lastIndexOf('/') + 1);
        //  console.log( 'mode is = ' + JSON.stringify( mode ) );
        //}

        //---------------add Error Marker------------------
        if (!!data.addMarker ){
          let pos = data.addMarker;
          let row1 = pos[0]; 
          let col1 = pos[1]; 
          let row2 = row1+1; 
          let mid= editor.getSession().addMarker(
              new Range(row1, 0, row2, 1), 
              'ace_error-marker', 
              'line', true
          );
          console.log('mid='+JSON.stringify(mid));
          let errorsMark=$el.data('errorMarkerArray');
           console.log('errorsMark1='+JSON.stringify(errorsMark));
          errorsMark.push(mid);
          console.log('errorsMark2='+JSON.stringify(errorsMark));
          $el.data('errorMarkerArray', errorsMark);
          let markers=$el.data('errorMarkerArray');
          console.log('after adding markers='+ JSON.stringify(markers));
        }
        
        //---------------tbMssg------------------
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
        
        
        //---------------tabSize------------------
        if(!!data.tabSize){
          //editor.getSession().setUseSoftTabs(true);
          editor.getSession().setTabSize( data.tabSize );
        }
        
        //---------------resetElementColor------------------
        if(!!data.resetElementColor){
          $.each(data.resetElementColor, function(key,element){
            var rule=getStyleRule(key);
            rule.color=element;
           });
        }
        
        //---------------toggleWhiteSpace------------------
        if(!!data.toggleWhiteSpace){
          editor.setShowInvisibles(!editor.getShowInvisibles());
        }
        
        //---------------toggleTabType------------------
        if(!!data.toggleTabType){
          editor.session.setUseSoftTabs(!editor.session.getUseSoftTabs());
        }
        
        //---------------snippets------------------
        if(!!data.snippets){
          var snippetManager = ace.require("ace/snippets").snippetManager;
          var m = snippetManager.files[editor.session.$mode.$id];
          m.snippetText = data.snippets;
          if (!!m.snippets){
            snippetManager.unregister(m.snippets);
          }
          m.snippets = snippetManager.parseSnippetFile(m.snippetText, m.scope);
          snippetManager.register(m.snippets);
        }
        
        //---------------setfocus------------------
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
        
        //---------------getKeyboardShortcuts------------------
        if(!!data.getKeyboardShortcuts){
          //var kb=ace.ext.get_editor_keyboard_shortcuts.getEditorKeybordShortcuts(editor);
          //Shiny.onInputChange("keyBoardHelp",kb);
          ace.config.loadModule('ace/ext/keybinding_menu', function(module) {
            //var require
            var kb=module.getEditorKeybordShortcuts.getEditorKeybordShortcuts(editor);
          });
            
          }
          // update a given Range
          
       //---------------getLastOK------------------
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
             isSaved: ud.isSaved(),
             sender : data.sender,
             id:id
          });
       }
       
/*       if(!!data.setClean){
         //editor.getSession().getUndoManager().dirtyCounter=0;
         Shiny.onInputChange('messageFromAce', 
          {
             code :  editor.getSession().getValue(),
             sender : data.sender,
             id : id,
             //dirty: editor.getSession().getUndoManager().dirtyCounter,
             isSaved: ud.isSaved(),
             rnd : randomString(5)
          });
       }
*/

       //---------------replacement------------------
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
                 isSaved: ud.isSaved(),
                 selector: data.selector,
                 rnd : randomString(5)
              } );   
          }, 5 );
          //var lang = ace.require("ace/lib/lang");
          //lang.delayedCall(editor.getSession().getUndoManager().setOk());
          
          
          }        
        }
        
        //---------setOk----------
        if(!!data.setOk){
          //console.log("\ndata.setOk");
          editor.getSession().getUndoManager().setOk();
          clearAllMarkers($el, editor);
          /*
          console.log('setOk fin: editor.getSession().getUndoManager()$undoStack.length=' + 
                editor.getSession().getUndoManager().$undoStack.length);
          console.log('setOk fin: editor.getUndoManager()getSession().$ok=' + 
                JSON.stringify(editor.getSession().getUndoManager().$ok));
          */
          
        }
        
        //-------------setValue---------------
        if(!!data.setValue){
          editor.getSession().setValue(data.setValue);
          if(!!data.ok){
            editor.getSession().getUndoManager().setOk();
          }
          //if(['cmd.openFileNow','cmd.file.new'].indexOf(sender)>=0){
          //  editor.getSession().clearBreakpoints();
          //}
          
          Shiny.onInputChange('messageFromAce', 
          {
             code : editor.getSession().getValue(),
             sender : data.sender,
             id : id,
             mode: aceMode,
             //dirty: editor.getSession().getUndoManager().dirtyCounter,
             isSaved: ud.isSaved(),
             rnd : randomString(5)
          });
        }
        
        //------------getValue----------------------------
        if(!!data.getValue){
          /*
          if(['cmd.file.new'].indexOf(sender)>=0){
            //console.log('sender is cmd.file.new, should do editor.find');
            // select NULL
            //if(getAceMode(editor)=='ptr'){
              editor.find('NULL');
            //}
          }
          */
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
             mode: aceMode,
             isSaved: ud.isSaved(),
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
             isSaved: ud.isSaved(),
             docFilePath: $el.data('docFilePath'),
             rnd : randomString(5)
          });
        }
        
        //------------getDoc-------------------
        if(!!data.getDoc){ 
          //var undoLen=editor.getSession().getUndoManager().$undoStack.length;
          //var saveLen= $el.data('docFileSaved');
          //if(! data.oldFilePath){
          //  data.oldFilePath='?';
          //}
          //console.log('getDoc:: docFileSaved=' +JSON.stringify(ud.isSaved()) );
          Shiny.onInputChange('messageFromAce', 
          {
             code :  editor.getSession().getValue(),
             sender : data.sender,
             id : id,
             //dirty: editor.getSession().getUndoManager().dirtyCounter,
             //saved: undoLen===saveLen,
             isSaved: ud.isSaved(),
             docFilePath: $el.data('docFilePath'),
             //priorFilePath: data.oldFilePath,
             rnd : randomString(5)
          });
        }
        //----------------------
        
        //todo: add more messaging capablilities
      }
  );
  
  
