
      function UMD(){
        ace.UndoManager.call(this);
        this.$ok=[];
        this.$initiallySaved=false;
      }
      //ud.$ok=[];
      UMD.prototype=Object.create(ace.UndoManager.prototype);
      UMD.prototype.constructor= UMD;
      
      UMD.prototype.setOk = function(){ 
        // TODO donot push if ok is on stack
        if(this.$ok.indexOf(this.$undoStack.length) == -1){
          this.$ok.push( this.$undoStack.length); 
          //return(this);
        }
      };
      //ud.setOk=function(){ this.$ok.push( this.$undoStack.length); };
      //ud.pop2Ok=function(){
      
      // kludge to fix initial save state
      UMD.prototype.isSaved = function(){ 
        if(this.$undoStack.length === 0){
          return this.$initiallySaved;
        } else {
          return this.isClean();
        }
      };
     UMD.prototype.setSaved = function(){ 
        if(this.$undoStack.length === 0){
          this.$initiallySaved=true;
        } else {
          this.markClean();
        }
      };      
      
      
      
      UMD.prototype.pop2Ok = function(){
        var udlen= this.$undoStack.length;
        //console.log("popstart: this.$ok=" + JSON.stringify(this.$ok));
        //console.log("popstart: this.dirtyCounter.length=" + this.dirtyCounter);
        //console.log("popstart: this.$undoStack.length=" + this.$undoStack.length);
        if(udlen>0){
          if(this.$ok.length>0){
            this.$ok = this.$ok.filter(function(i){ return i<= udlen; });
            var lastOkLen = 0;
            if(this.$ok.length>0){
              lastOkLen = this.$ok[this.$ok.length-1];
            } 
            // need to invoke the undo until 
            // this.$undoStack.length==-lastOkLen;
            //console.log("lastOkLen" + lastOkLen);
            //console.log("Prior to pop loop: this.$undoStack.length=" + this.$undoStack.length);
            while( this.$undoStack.length>lastOkLen ){
              //console.log("POPPING");
              this.undo(true);
            }
            //console.log("this.$undoStack.length=" + this.$undoStack.length);
            this.$redoStack = [];
          }
        }
        //console.log("popfin: this.$ok=" + JSON.stringify(this.$ok));
        //console.log("popfin: this.$undoStack.length=" + this.$undoStack.length);
        //console.log("popfin: this.dirtyCounter.length=" + JSON.stringify(this.dirtyCounter));
        return(this);
      };
      
      randomString = function(length) {
                var text = '';
                var possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
                for(var i = 0; i < length; i++) {
                    text += possible.charAt(Math.floor(Math.random() * possible.length));
                return(text);
        }
      };
      
      

function ptRaceInit(data){
  console.log("---------------------ptRaceInit(data)-------------------------------");
  console.log(typeof(data));
  ace.require("UndoManager");
  var id = data.id;
  //console.log("ptRaceInit:: data.id=" + data.id + ":");
  //console.log("ptRaceInit:: data.id[0]=" +data.id[0] +":");
  //console.log("ptRaceInit:: data.initSaved=" +JSON.stringify(data.initSaved)); 
  var mode = data.mode;
  var $el = $('#' + id);
  //console.log($el.attr());
  //console.log($el.attr());
  //console.log('JSON.stringify($el.data)')
  var theEditor = $el.data('aceEditor'); 
  var autoComplete = data.autoComplete[0];
  var autoCompleteList=data.autoCompleteList;
  //console.log('autoCompleteList=', JSON.stringify(autoCompleteList));
  
  if(!!theEditor){
    console.log('found theEditor');
  } else {
    console.log('theEditor is null');
  }
  
  if(!!mode){
    console.log('mode=' + mode);
  } else {
    console.log('mode is null');
  }
  
  $el.data('errorMarkerArray', []);
  
  if(!!data.docFilePath){
    //console.log('setting  docFilePath=' + data.docFilePath);
    $el.data('docFilePath', data.docFilePath);
  }
  
    if(mode == 'ptr' || mode=='ptrrmd' || mode=='dnippets'){ 
      data.acejs= data.acejs[0];
      ace.config.set('workerPath', "./Acejs");
      ace.config.set('modePath', "./Acejs");
    }  
      theEditor.getSession().setMode('ace/mode/' + mode);  // shinyAce init
      theEditor.getSession().setOption('useWorker', true);
      theEditor.getSession().setUseSoftTabs(true);
      theEditor.getSession().setUseWrapMode(false);
      theEditor.renderer.setShowGutter(true);
      theEditor.setHighlightActiveLine(true);
      
      if(autoComplete == "live") {
        theEditor.setOption('enableLiveAutocompletion', true);
      }
      if(autoComplete != "disabled") {
        theEditor.setOption('enableBasicAutocompletion', true);
        theEditor.setOption('enableSnippets', true);
      }
      if(autoComplete!="disabled") {
        theEditor.completers.splice(1, 1); // to get rid of the ducpliate local matches 
        //console.log(JSON.stringify(theEditor.completers.length));
        //console.log(JSON.stringify(theEditor.completers));
      }
      if (data.hasOwnProperty('autoCompleteList')){
          $el.data('autoCompleteList', data.autoCompleteList);
      }
      //Next a custom history manager
      
      
      var ud = new UMD(); 
      if(!!data.initSaved){
          ud.$initiallySaved=data.initSaved;
          console.log('---->undo manager .$initiallySaved=' + ud.$initiallySaved);
      }      
      //console.log('initial undo :' + JSON.stringify(ud));
      theEditor.getSession().setUndoManager(ud);
      
      theEditor.commands.addCommand({
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
            };
            Shiny.onInputChange('commitMssg', randomString(5) );
        }
      });
      
      theEditor.commands.addCommand({
        name: 'closeWindow',
        bindKey: {win: 'Ctrl-x', mac: 'Command-x'},
        exec: function(editor) {
            event.stopPropagation();
            var randomString = function(length) {
                var text = '';
                var possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
                for(var i = 0; i < length; i++) {
                    text += possible.charAt(Math.floor(Math.random() * possible.length));
              }
              return text;
            };
            Shiny.onInputChange('closeTab',  { id :id,  type:'aceId', rnd: randomString(5) });
        }
      });
      
      
  
      theEditor.commands.addCommand({
        name: 'helpR',
        bindKey: {win: 'F1', mac: 'F1'},
        exec: function(editor) {
          console.log('--------------helpR!----------------------');
          editor.getSession().selection.moveCursorLongWordLeft();
          editor.getSession().selection.selectWordRight();
          var text = editor.getSession().getTextRange(editor.getSelectionRange());
          Shiny.onInputChange('helpMssg', {query:text, num:Math.random(), editorId: id} );
        } //TODO: remove editorVar; appears not to be used
      });
      
      theEditor.on("guttermousedown", function(e) {
       // derived from the following link
       //https://stackoverflow.com/questions/16864236/how-to-mark-line-numbers-in-javascript-ace-editor
        var target = e.domEvent.target; 
        if (target.className.indexOf("ace_gutter-cell") == -1)
            return; 
        if (!theEditor.isFocused()) 
            return; 
        if (e.clientX > 25 + target.getBoundingClientRect().left) 
            return; 
        var breakpoints = e.editor.session.getBreakpoints(row, 0);
        var row = e.getDocumentPosition().row;
        if(typeof breakpoints[row] === typeof undefined)
            e.editor.session.setBreakpoint(row);
        else
            e.editor.session.clearBreakpoint(row);
        e.stop();
     });
 
    
    $el.droppable({

    activeClass: "ui-state-default",
    hoverClass: "ui-state-hover",
    accept: ":not(.ui-sortable-helper)",

    drop: function(event, ui) {
      var pos = $el.data('aceEditor').renderer.screenToTextCoordinates(event.clientX, event.clientY);
      
      console.log("pos=" + JSON.stringify(pos));
      console.log("ui =\n" + JSON.stringify(ui));
      var txt =  ui.draggable.attr("data-snippet");
      console.log(
        'txt=' + JSON.stringify(txt)
      );
      $el.data('aceEditor').moveCursorToPosition(pos);
      $el.data('aceEditor').focus();
	    var tab_press= jQuery.Event('keydown', {which: 88});
       
      console.log('drop occurred');
 	    $el.trigger(tab_press);
       var snippetManager = ace.require("ace/snippets").snippetManager;
      snippetManager.insertSnippet($el.data('aceEditor'), txt);

      return true;
    }
  });
  
  var geRandomString = function(length) {
                var text = '';
                var possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
                for(var i = 0; i < length; i++) {
                    text += possible.charAt(Math.floor(Math.random() * possible.length));
              }
              return text;
            };
            
/*  Shiny.onInputChange('messageFromAce', 
          {
             code :  theEditor.getSession().getValue(),
             sender : 'cmd.file.new',
             id : data.id,
             //dirty: editor.getSession().getUndoManager().dirtyCounter,
             mode: data.mode,
             isSaved: ud.isSaved(),
             rnd : geRandomString(5)
          }
    );
*/

}




