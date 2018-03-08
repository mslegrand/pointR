
Shiny.addCustomMessageHandler(
  "ptRManager",
  function(data){
    //console.log('hello from ptRManager\n');
    //console.log(JSON.stringify(data));
    if(!!data.openFile){
      console.log('about to trigger open\n');
      if( data.sender==='cmd.openFileNow'){
        $('#buttonFileOpenHidden').trigger('click');
      }
      if( data.sender==='cmd.snippet.file.open'){
        $('#buttonSnippetOpen').trigger('click');
      }
    }
    if(!!data.importSnippet){
      console.log('about to trigger load snippet\n');
      $('#buttonSnippetOpen').trigger('click');
    }
    if(!!data.saveFile){
      console.log('about to trigger save\n');
      $('#buttonFileSaveHidden').trigger('click');
    }
    if(!!data.exportSVG){
      console.log('about to trigger svg export\n');
      $('#buttonExportSVGHidden').trigger('click');
    }
    if(!!data.setFocus){
      //console.log('#' + data.setFocus +'\n' );
      setTimeout(function() {$('#' + data.setFocus).focus()}, 10);
      //$('#' + data.setFocus).focus();
    }
     if(!!data.hide){
      console.log('#' + data.hide +'\n' );
      setTimeout(function() {$('#' + data.hide).hide()}, 10);
    }
    if(!!data.addClass){
      //var klass=data.addClass.klass;
      setTimeout(function() {$('#' + data.addClass.id).addClass(klass)}, 10);
    }
    if(!!data.snippetButtonActivate){
      $(".snippetButton").each( function(){
        $(this).draggable({
    		//revert: true
    		opacity: 0.5,
    		stroke: "#FFFFFF",
    		helper: 'clone',
        revert: 'invalid',
        appendTo: 'body'
      }); 
    });
    }
  }
); 

$('#buttonFileOpenHidden').on('selection', function(event, path){
  console.log('selection');
});



$('document').ready(function()
{
  $('.hiddenButton').hide();
  
  /*
  $("#left-component").on('resize', function(e){
    console.log('resize #left-component');
  });

  $(".split-pane-component").on('resize', function(e){
    console.log('resize #split-pane-component');
  });
  */
  
  
  var sntb = new SnippetToolBaR( "snippetToolBarContainer", "dndSnippetList", "snippetScrollDown", "snippetScrollUp", 32);
  sntb.reAdjustPos();
  $(sntb.downId).click(function(){sntb.onDownClick();});
  $(sntb.upId).click(function(){sntb.onUpClick();});
  $(window).on('resize',function(e){  
    console.log('resize window');
    	sntb.reAdjustPos();
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




