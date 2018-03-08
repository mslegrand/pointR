
// cSnippetToolBarContainer > cSnippetToolBarList > snippetButton
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
    //console.log('entering heightOfList');
    //console.log('heightOfList:: num of kids=' + $(this.listId).children().length);
    //console.log('heightOfList:: itemHeight='+ this.itemHeight);
    //console.log('rtv=' + this.itemHeight*($(this.listId).children().length));
    return this.itemHeight*($(this.listId).children().length);
  };
  SnippetToolBaR.prototype.heightOfHidden = function(){
   // console.log('entering heightOfHidden');
    //console.log('heightOfHidden:: this.heightOfList()' + this.heightOfList());
    //console.log('heightOfHidden:: $(this.containerId).outerHeight()' + $(this.containerId).outerHeight() );
    var rtv =  this.heightOfList()-$(this.containerId).outerHeight();
    //console.log('heightOfHidden:: 1:: rtv=',rtv );
    if(rtv<0){
      rtv=0;
    }
    console.log('heightOfHidden:: 2:: rtv=',rtv );
    return rtv;
  };
  SnippetToolBaR.prototype.reAdjustPos = function(){
    console.log('re1: entering reAdjustPos');
    console.log('re2: top= ' + $(this.listId).position().top);
    //console.log('re3: num of kids=' + $(this.listId).children().length);
    //console.log('re4: itemHeight='+ this.itemHeight);
    //console.log('re5: this.heightOfHidden()='+ this.heightOfHidden());
    console.log('re6: this.gHiddenHeight='+ this.gHiddenHeight);
    var deltaHidden = this.heightOfHidden() - this.gHiddenHeight;
    console.log('re7: deltaHidden=' + deltaHidden);
    if(deltaHidden!=0){
     console.log('re8: deltaHidden=' + deltaHidden);
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
    console.log('getTopPos()=' + this.getTopPos() );
    if( this.getTopPos()>=0){
    //if( this.getTopPos()>=-4){
      console.log('getTopPos positive aa:: hiding up');
      $(this.upId).hide();
    }
    //if(this.heightOfHidden()<=4){
   if(this.heightOfHidden()<=-8){
      //$(this.downId).hide();
      console.log('heightOfHidden is negative bb:: hiding down');
    }
    console.log('seting this.gHiddenHeight=', this.heightOfHidden());
	  this.gHiddenHeight=this.heightOfHidden();
     
    }
  };
  

  SnippetToolBaR.prototype.onDownClick = function(){
    console.log('down click 0--');
    var m1 = $(this.containerId).outerHeight()-2*this.itemHeight;
    var m2 = (this.heightOfHidden()+ this.getTopPos()); 
    var delta = Math.min(m1,m2);
    if(m2<=m1){
      //$(this.downId).fadeOut('slow');
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
    



