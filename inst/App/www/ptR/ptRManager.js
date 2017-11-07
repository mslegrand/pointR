
Shiny.addCustomMessageHandler(
  "ptRManager",
  function(data){
    console.log('hello from ptRManager\n');
    //console.log(JSON.stringify(data));
    if(!!data.openFile){
      console.log('about to trigger open\n');
      $('#buttonFileOpenHidden').trigger('click');
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


