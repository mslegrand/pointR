  var resizeStuff=function(){
   $("#mySplitter").css("height", parseInt($(parent.window).height()));
   $("#mySplitter").css("width", parseInt($(parent.window).width()));
   $("#mySplitter").trigger("resize"); 
  };


$().ready(function(){
  $("#mySplitter").splitter();
  $("#mySplitter").css("height", $(window).innerHeight()-5).trigger("resize");

  $(window).resize(function() {
      resizeStuff();
  }).resize();
       
  $(window).trigger("resize");
  
});

//$().splitter({splitbarKey: "I", anchorToWindow: true, resizeToWidth: true});


$().splitter({splitbarKey: "I"});

