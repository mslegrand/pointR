var global_snbt=null;

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
      if(this.heightOfHidden()<=0){
         $(this.listId).animate({top:0},'fast');
         $(this.upId).hide();
         $(this.downId).hide();
      } else if( this.getTopPos()<= -this.heightOfHidden()-6){
         $(this.listId).animate({top:-this.heightOfHidden()-6},'fast');
         $(this.downId).hide();
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
    var delta  = 0.5*$(this.containerId).outerHeight();
    var tp = this.getTopPos() - delta;
    if( ( tp + this.heightOfHidden() )<=0 ){
        tp = -this.heightOfHidden()-6;
        $(this.downId).fadeOut('slow');
    }
    $(this.listId).animate({top: tp },'slow',function(){$(this.upId).fadeIn('slow');});
    $(this.upId).fadeIn('slow');
  };
    

    
  SnippetToolBaR.prototype.onUpClick = function() {
    //console.log("tbUp click");
  	$(this.downId).fadeIn('slow');
    var delta = Math.min(-this.getTopPos(), $(this.containerId).outerHeight()-2*this.itemHeight );
    if(-this.getTopPos()<= $(this.containerId).outerHeight()-2*this.itemHeight ){
      $(this.upId).fadeOut('slow');
    }
    $(this.listId).animate({top:"+=" + delta +"px"},'slow',function(){});
  };
  

    



