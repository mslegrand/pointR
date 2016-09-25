
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

//shinyjs.disableMenu = function(name) {
//var men =$(name );
//men.addClass('disabled');
//var tmp = name+' a';
//$(tmp ).css('color','grey');
//men.bind('click.dropdown-toggle', function(e) {
//e.preventDefault();
//return false;
//});
//};

//shinyjs.enableMenu = function(name) {
//var men =$(name);
//var tmp = name+' a';
//$(tmp ).css('color','#333388');
//men.unbind('click.dropdown-menu');
//men.removeClass('disabled');
//};

shinyjs.ignoreClick=function(e) {
  e.preventDefault();
  return false;
};

shinyjs.disableMenu = function(navBarId) {
$('#'+navBarId).addClass('disabled');
$('#'+navBarId).bind('click', shinyjs.ignoreClick)
}

shinyjs.enableMenu = function(navBarId) {
  $('#'+navBarId).removeClass('disabled');
  $('#'+navBarId).unbind('click', shinyjs.ignoreClick );
}

