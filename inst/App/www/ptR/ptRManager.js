
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
  }
); 

$('#buttonFileOpenHidden').on('selection', function(event, path){
  console.log('selection');
});



$('document').ready(function()
{
  $('.hiddenButton').hide();
  //$('body').on('keypress', 'enter')
});




