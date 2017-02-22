$(function() {
  $('div.split-pane').splitPane();
  $('button:first').on('click', function() {
    $('div.split-pane').splitPane('lastComponentSize', 0);
  });
  $('button:last').on('click', function() {
    $('div.split-pane').splitPane('firstComponentSize', 0);
  });
});