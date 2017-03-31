shinyjs.closeWindow = function() { window.close(); };

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


shinyjs.triggerButtonOnEnter=function(event,buttonId){
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

