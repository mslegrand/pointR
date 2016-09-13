
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

shinyjs.disableMenu = function(navBarId, pos) {
//var men =$('#' + navBarId +' li:nth-child(2)' );
var men =$('#plotNavBar li:nth-child(2)' );
men.addClass('disabled');
$('#plotNavBar li:nth-child(2) a' ).css('color','grey');
men.bind('click.dropdown-menu', function(e) {
e.preventDefault();
return false;
});
};

shinyjs.enableMenu = function(navBarId, pos) {
var men =$('#plotNavBar li:nth-child(2)' );
$('#plotNavBar li:nth-child(2) a' ).css('color', '#333388');
men.unbind('click.dropdown-menu');
men.removeClass('disabled');
};

