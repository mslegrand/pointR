if(!!window.sendToElectron){
  window.ipcRenderer.on( 'appRunner', function(event, arg1, arg2){
    Shiny.onInputChange('appStatus', 
      {
          mssg:  arg1, 
          tabId: arg2,
          rnd:Math.random().toString(36).replace(/[^a-z]+/g, '').substr(0, 5)
      });
  });
  
    window.ipcRenderer.on( 'appRunnerLog', function(event, arg1, arg2){
    //alert('appRunnerLog=' + JSON.stringify( arg1));
    //console.log('appRunnerLog=' + JSON.stringify( arg1));
    //alert('appRunnerLog=' + JSON.stringify( arg2));
    Shiny.onInputChange('appLog', 
      {
          mssg:  arg1, 
          tabId: arg2,
          rnd:Math.random().toString(36).replace(/[^a-z]+/g, '').substr(0, 5)
      });
  });
  
  window.ipcRenderer.on( 'appCloseCmd', function(event, arg){
    // console.log('about to close');
    $('#ptRQuit').trigger('click');
  });
  
  window.ipcRenderer.on( 'fileChanged', function(event, arg){
    // console.log('fileChanged');
    //alert('fileChanged '+JSON.stringify(arg));
    Shiny.onInputChange('fileChanged', 
      {
          mssg:  arg, 
          rnd:Math.random().toString(36).replace(/[^a-z]+/g, '').substr(0, 5)
      });
      
    //$('#ptRQuit').trigger('click');
  });
  
  window.ipcRenderer.on( 'fileDeleted', function(event, arg){
    // console.log('fileDeleted');
    //alert('fileDeleted '+JSON.stringify(arg));
    //$('#ptRQuit').trigger('click');
  });
  
  
}



Shiny.addCustomMessageHandler(
  "ptRManager",
  function(data){
    // console.log('-----------Entering ptRManager------------\n');
    // console.log(JSON.stringify(data));
    if(data.sender==="cmd.electron"){
      if(!!data.app2RunPath){
        if(!!window.sendToElectron){
          window.sendToElectron('cmdAppRun',data.app2RunPath, data.tabId);
        }
      }
      if(!!data.resetWatcher){
        if(!!window.sendToElectron){
          // console.log('resetWatcher sendToElectron');
          window.sendToElectron('resetWatcher',data.resetWatcher);
        }
      }
      if(!!data.app2stop){
        if(!!window.sendToElectron){
          window.sendToElectron('cmdStopAppRunner', data.app2stop, "");
        }
      }
      if(!!data.version){
        if(!!window.sendToElectron){
          window.sendToElectron('cmdSetTitle','pointR --Version:', data.version);
        }
      }
      if(!!data.openLink){
        // console.log('data.openLink' + JSON.stringify(data.openLink));
        if(!!window.sendToElectron){
          window.sendToElectron('cmdOpenLink',data.openLink, '');
        }
      }
      if(!!data.openWindow){
        // console.log('data.openWindow' + JSON.stringify(data.openWindow));
        if(!!window.sendToElectron){
          window.sendToElectron('cmdOpenWindow',data.openWindow, '');
        }
      }
    } //end of electron handlers
    if(data.sender==="closePtRWindowNow"){
      // console.log('inside data.closePtRWindowNow');
      if(!!window.sendToElectron ){ 
        // console.log('about to send confirmation');
        var confirmation=window.sendExitConfirmation();
        //window.ipcRenderer.sendSync('confirmExit', true);
        // console.log('confirmation recieved '+ JSON.stringify(confirmation));
      }
      // console.log('invoking window.close');
      window.close();
    }
    if(!!data.openFile){
      //console.log('about to trigger open\n');
      if( data.sender==='cmd.openFileNow'){
        $('#buttonFileOpen').trigger('click');
      }
      if( data.sender==='cmd.snippet.file.import'){
        $('#buttonSnippetImport').trigger('click');
      }
      if( data.sender==='cmd.dnippet.file.import'){
        $('#buttonDnippetImport').trigger('click');
      }
    } //end data.openFile
    if(!!data.saveFile){
      var sender=data.sender;
      // console.log('data.saveFile:: sender=' + sender);
      var tabId = data.tabId;
      // console.log('data.saveFile:: tabId=' + tabId);
      //get title from tabId and stabs
      var title=stabs.getTitleGivendataValue(tabId);
      // console.log('ptRManager:: title= ' + JSON.stringify(title));
      
      if(!!data.target){
        // console.log('save target=' + data.target); 
      } 
      
      $('#' + data.target).trigger('click');
      $("h4.sF-title.modal-title").text( 'Save '+ title +' as ...');
      
      if(sender==='fileCmd.close' || sender==='fileCmd.quit'){
        // console.log("sender is close or quit\n");
        // console.log('data.saveFile:: sender=' + sender);
        $("#sF-cancelButton").text( "Close Without Saving");
        $('#' + data.target).on('cancel', function(event){
            Shiny.onInputChange(data.target, { 
              sender:sender, 
              cancel: 'close', 
              rnd: Math.random().toString(36).substring(7)});
        });
      } else {
        // console.log("sender is neither close or quit\n");
        // console.log('data.saveFile:: sender=' + sender);
        $("#sF-cancelButton").text( "Cancel Save");
        $('#' + data.target).on('cancel', function(event){
          Shiny.onInputChange(data.target, {
            sender:sender, 
            cancel: 'cancel',
            rnd: Math.random().toString(36).substring(7)
          });
        });
      }
    } //endof data.saveFile
    if(!!data.setFocus){ // I don't if this is still being called???
      setTimeout(function() {$('#' + data.setFocus).focus()}, 10);
    }
     if(!!data.hide){ // I don't if this is still being called???
      setTimeout(function() {$('#' + data.hide).hide()}, 10);
    }
    if(!!data.addClass){ // I don't if this is still being called???
      setTimeout(function() {$('#' + data.addClass.id).addClass(klass)}, 10);
    }
    if(!!data.removeDrippets){
      $('#dndSnippetList').empty();
    }
    if(!!data.insertDrippets){
      var drippets = data.insertDrippets;
      $('#dndSnippetList').empty();
      drippets.forEach( function(dripItem){
        /*
          console.log('dripItem =' + JSON.stringify(dripItem)+"\n");
          console.log('logo =' + JSON.stringify(dripItem.logo)+"\n");
          console.log('snip =' + JSON.stringify(dripItem.snip)+"\n");
          console.log('hint =' + JSON.stringify(dripItem.hint)+"\n");
      	*/
        $('#dndSnippetList').append(
           $('<li/>')
              .attr("id",dripItem.id)
              .addClass('snippetButton')
              .attr('data-snippet', dripItem.snip)
              .attr('title', dripItem.hint)
              .prop('data-toggle','tooltip')
              .prop('data-placement','top')
              .append(
                  $(dripItem.logo)
              )
              .draggable({
              		appendTo: 'body',
              		revert: 'invalid',
              		helper: function() {
              			//drag selected items
              			var selected = $(this);
              			var container = $('<div/>');
              			container.append(selected.clone());
              			return container;
              		},
              		stop: function () {
              			$(this).draggable('option', 'revert', 'invalid');
              		}
              	})
          );
      });
    } //endof insertDrippets
    
    
    if(!!data.rowCountChange){
      $(window).resize();
    }
    // console.log('-----------Exiting ptRManager------------\n');
  }
);



$('document').ready(function()
{
  $('.hiddenButton').hide();
  


  //this is  ctrl+w key fails in browser, but works
  document.addEventListener('keydown', function(event) {
    if (event.code == 'KeyS' && (event.ctrlKey || event.metaKey)) {
      //alert('saving!');
      let $el=$('#editNavBar');
      if(event.shiftKey){
        $el.attr('value',"saveAll");
      } else {
        $el.attr('value',"Save");
      }
  		$el.trigger("change");
      event.stopPropagation();
    }
   /* if (event.code == 'KeyW' && (event.ctrlKey || event.metaKey)) {
      //alert('closing!');
      let $el=$('#editNavBar');
      if(event.shiftKey){
        $el.attr('value',"closeAll");
      } else {
        $el.attr('value',"close");
      }
  		$el.trigger("change");
      event.stopPropagation();
    }
    */
    if (event.code == 'KeyT' && (event.ctrlKey || event.metaKey)) {
      //alert('closing!');
      let $el=$('#editNavBar');
      $el.attr('value',"closeProject");
  		$el.trigger("change");
      event.stopPropagation();
    }
  });
  
  
  $( "#svgOutPanel" ).keydown(function(event) {
    $( "#svgOutPanel" ).data("keycode", event.keyCode );
   // alert( "Handler for .keydown() called." );
  });
  $( "#svgOutPanel" ).keyup(function(event) {
    $( "#svgOutPanel" ).data("keycode", null);
   // alert( "Handler for .keyup() called." );
  });
  
  /*
  $("#left-component").on('resize', function(e){
    console.log('resize #left-component');
  });

  $(".split-pane-component").on('resize', function(e){
    console.log('resize #split-pane-component');
  });
  */
  
  
  global_snbt = new SnippetToolBaR( "snippetToolBarContainer", "dndSnippetList", "snippetScrollDown", "snippetScrollUp", 32);
  global_snbt.reAdjustPos();
  $(global_snbt.downId).click(function(){global_snbt.onDownClick();});
  $(global_snbt.upId).click(function(){global_snbt.onUpClick();});
  $(window).on('resize',function(e){  
    	global_snbt.reAdjustPos();
  });
  
  var rsb = new rowScrollBaR("rowOutPanel",  "rowDND-rowPanel", "rowScrollDown","rowScrollUp", 32 );
  //"rowDND-rowPanel" does not have position(), actually no attributes
  
  //var rsb = new rowScrollBaR("rowOutPanel",  "rowDND-rowIndex", "rowScrollDown","rowScrollUp", 32 );
  
  
  rsb.reAdjustPos();
  $(rsb.downId).click(function(){rsb.onDownClick();});
  $(rsb.upId).click(function(){rsb.onUpClick();});
  $(window).on('resize',function(e){  
      //console.log('resize window');
    	rsb.reAdjustPos();
  });
  
  
  /*
  $(window).on('resize',function(e){  
  	alert('resize window');
  });
  */
  
    
/*  
  $('.cAceContainer').droppable({
    
    //activeClass: "ui-state-default",
    //hoverClass: "ui-state-hover",
    accept: ":not(.ui-sortable-helper)",

    drop: function(event, ui) {
      console.log('0:  drop occurred');
      var theEditor = $(this).data('aceEditor'); 
      console.log( JSON.stringify( this.data ))
      var pos = theEditor.renderer.screenToTextCoordinates(event.clientX, event.clientY);
      
      console.log("pos=" + JSON.stringify(pos));
      var txt =  ui.draggable.attr("data-snippet");
      $(this).focus();
      theEditor.moveCursorToPosition(pos);
      //editor.session.insert(pos, txt);
      //editor.insert(txt);
      ui.helper.remove();
      
	    var tab_press= jQuery.Event('keydown', {which: 88});
       var snippetManager = ace.require("ace/snippets").snippetManager;
       snippetManager.insertSnippet(theEditor, txt);
       
       setTimeout( function(){
       $(this).focus();
       console.log('drop occurred');
 	     $(this).trigger(tab_press);
       }, 5);
      return true;
    }


  });
*/  

  //$('body').on('keypress', 'enter')
});




