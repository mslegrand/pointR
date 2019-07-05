

shinyjs.closeWindow = function() { 
  console.log('inside shinyjs.closeWindow');
/*  if(!!window.sendToElectron ){ 
    console.log('about to send confirmation');
    var confirmation='no'; //window.sendExitConfirmation();
    //window.ipcRenderer.sendSync('confirmExit', true);
    console.log('confirmation recieved '+ JSON.stringify(confirmation));
  }
*/  
  console.log('invoking window.close');
  window.close();
};



shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
};

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
};


shinyjs.ignoreClick=function(e) {
  e.preventDefault();
  return false;
};


shinyjs.triggerButtonOnEnter=function(event,buttonId){ //cmdNewColumn.R; cmdNewAsset.R
  if(event.keyCode==13){
    $('#' + buttonId).trigger("click");
  }
};

shinyjs.disableMenu = function(navBarId) {
$('#'+navBarId).addClass('disabled');
$('#'+navBarId).bind('click', shinyjs.ignoreClick);
};

shinyjs.enableMenu = function(navBarId) {
  $('#'+navBarId).removeClass('disabled');
  $('#'+navBarId).unbind('click', shinyjs.ignoreClick );
};



