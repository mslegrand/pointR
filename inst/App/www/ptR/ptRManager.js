
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
  }
);

$('#buttonFileOpenHidden').on('selection', function(event, path){
  console.log('selection');
});

$('document').ready(function()
{
  $('.hiddenButton').hide();
                
});


