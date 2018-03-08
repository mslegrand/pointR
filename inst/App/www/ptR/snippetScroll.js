
function SnippetToolBaR(containerId, listId, buttonDownId, buttonUpId, itemHeight){
  this.containerId =  "#" +  containerId;
  this.listId      =  "#" +  listId;
  this.downId      =  "#" +  buttonDownId;
  this.upId        =  "#" +  buttonUpId;
  this.itemHeight  =  itemHeight;
  
  this.gHiddenHeight=0;
}

  SnippetToolBaR.prototype.getTopPos=function(){
    return $(this.listId).position().top;
  };
  
  SnippetToolBaR.prototype.heightOfList=function(){
    return this.itemHeight*($(this.listId).children().length);
  };
  SnippetToolBaR.prototype.heightOfHidden = function(){
    var rtv =  this.heightOfList()-$(this.containerId).outerHeight();
    if(rtv<0){
      rtv=0;
    }
    return rtv;
  };
  SnippetToolBaR.prototype.reAdjustPos = function(){
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
     
   // }
  };
  

  SnippetToolBaR.prototype.onDownClick = function(){
    var m1 = $(this.containerId).outerHeight()-2*this.itemHeight;
    var m2 = (this.heightOfHidden()+ this.getTopPos()); 
    var delta = Math.min(m1,m2);
    if(m2<=m1){
      $(this.downId).fadeOut('slow');
      delta=delta+8;
    }
    $(this.listId).animate({top:"-=" + delta + "px"},'slow',function(){$(this.upId).fadeIn('slow');});
    $(this.upId).fadeIn('slow');
  };
    

    
  SnippetToolBaR.prototype.onUpClick = function() {
    console.log("tbUp click");
  	$(this.downId).fadeIn('slow');
    var delta = Math.min(-this.getTopPos(), $(this.containerId).outerHeight()-2*this.itemHeight );
    if(-this.getTopPos()<= $(this.containerId).outerHeight()-2*this.itemHeight ){
      $(this.upId).fadeOut('slow');
    }
    $(this.listId).animate({top:"+=" + delta +"px"},'slow',function(){});
  };
    



