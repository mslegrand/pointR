
(function(old) {
  $.fn.attr = function() {
    if(arguments.length === 0) {
      if(this.length === 0) {
        return null;
      }

      var obj = {};
      $.each(this[0].attributes, function() {
        if(this.specified) {
          obj[this.name] = this.value;
        }
      });
      return obj;
    }

    return old.apply(this, arguments);
  };
})($.fn.attr);

function rowScrollBaR(containerId, listId, buttonDownId, buttonUpId, itemHeight){
  this.containerId =   '#' + containerId;
  this.listId      =   '#' + listId;
  this.downId      =   '#' + buttonDownId;
  this.upId        =   '#' + buttonUpId;
  this.itemHeight  =  itemHeight;
  
  this.gHiddenHeight=0;
}

rowScrollBaR.prototype.getTopPos=function(){
  if(!!($(this.listId).position()) ){
    return $(this.listId).position().top;
  } else {
    return 0;
  }
  
};

rowScrollBaR.prototype.heightOfList=function(){
  if(!!$(this.listId)){
    return $(this.listId).outerHeight();
  } else {
    return 0;
  }
  
};
rowScrollBaR.prototype.heightOfHidden = function(){
  var rtv =  this.heightOfList()-$(this.containerId).outerHeight();
  if(rtv<0){
    rtv=0;
  }
  return rtv;
};

rowScrollBaR.prototype.reAdjustPos = function(){
  if(!$(this.containerId)){
    return false;
  }
  var deltaHidden = this.heightOfHidden() - this.gHiddenHeight;
  //if(deltaHidden!==0){
    if( deltaHidden > 0 ){ // container got smaller, hidden increased
      // keep current position
      // show down
      $(this.downId).show();
    } else if( deltaHidden < 0 ){ // deltaHidden<=0; container grew, hidden decreased
      // slide list down by the amount -deltaHidden
      if(deltaHidden>-this.itemHeight){
        $(this.listId).animate({top:0},'fast');
      } else{
       $(this.listId).animate({top:"-="+ deltaHidden +"px"},'fast');
      }
    }
    if( this.getTopPos()>=0){
      $(this.upId).hide();
    }
    
    if(this.heightOfHidden()<=0){
      $(this.downId).hide();
    }
    this.gHiddenHeight=this.heightOfHidden();
    
  //}
};


rowScrollBaR.prototype.onDownClick = function(){
  //console.log("rowScrollBaR::onDownClick");
  var m1 = $(this.containerId).outerHeight()-2*this.itemHeight;
  var m2 = (this.heightOfHidden()+ this.getTopPos() ); 
  //console.log("m1=" + m1);
  //console.log("m2=" + m2);
  var delta = Math.min(m1,m2);
  //console.log("delta=" + delta);
  if(m2<=m1){
    $(this.downId).fadeOut('slow');
    delta=delta+8;
  }
  $(this.listId).animate({top:"-=" + delta + "px"},'slow',function(){});
  $(this.upId).fadeIn('slow');
};



rowScrollBaR.prototype.onUpClick = function() {
  //console.log("rowScrollBaR:: onUpClick");
  $(this.downId).fadeIn('slow');
  var delta = Math.min(-this.getTopPos(), $(this.containerId).outerHeight()-2*this.itemHeight );
  if(-this.getTopPos()<= $(this.containerId).outerHeight()-2*this.itemHeight ){
    $(this.upId).fadeOut('slow');
  }
  $(this.listId).animate({top:"+=" + delta +"px"},'slow',function(){});
};


