var hidWidth;


function ScollableTabs( containerId ){
	this.scrollBarWidths = 40;
	
	this.lastHiddenWidth=0;
	this.containerId =  "#" +  containerId;
	this.container = $(this.containerId);
	//var containerSB = $('<div class=scroll-container> <div class="scroller scroller-right"> <i class="glyphicon glyphicon-chevron-right"></i> </div> <div class="scroller scroller-left"> <i class="glyphicon glyphicon-chevron-left"></i> </div> </div>' );
	//this.container.append(containerSB);
	//containerSB.insertAfter(this.container.find('ul'));
	var rightSB = $('<div class="scroller scroller-right"><i class="glyphicon glyphicon-chevron-right"></i></div>');
	//this.container.prepend(rightSB);
	rightSB.insertAfter(this.container.find('ul'));
	var leftSB = $('<div class="scroller scroller-left"><i class="glyphicon glyphicon-chevron-left"></i></div>');
	//this.container.prepend(leftSB);
	leftSB.insertAfter(this.container.find('ul'));
	
	this.rightSB =   this.container.find('.scroller-right');
	this.leftSB =  this.container.find('.scroller-left');
	//this.container.find('.scroller-left').onclick=function(){console.log('leftClick');};
	//this.container.find('.scroller-right').onclick=function(){console.log('rightClick');}
	//this.container.find('.scroller-right').click( function(){ this.rightClick(); } );
	return this;
}

ScollableTabs.prototype.widthOfList = function(){
   var itemsWidth = 0;
   this.container.find('ul li').each(function(){ //li elements 
   // $('.list li').each(function(){ //li elements 
    var itemWidth = $(this).outerWidth();
    itemsWidth+=itemWidth;
  });
  //console.log('no of items=' + this.container.find('ul li').length );
  return itemsWidth;
};





ScollableTabs.prototype.widthOfHidden = function(){
  console.log('this.container.outerWidth()=' + this.container.outerWidth());
  console.log('this.widthOfList()=' + this.widthOfList());
  console.log('this.getLeftPos()=' + this.getLeftPos());
  
  //return ((this.container.outerWidth())- this.widthOfList()-this.getLeftPosi())-this.scrollBarWidths;
  var rtv= this.widthOfList()- this.container.outerWidth();
  if(rtv<0){
    rtv=0;
  }
  return rtv;
};


ScollableTabs.prototype.getLeftPos = function(){
	//return this.container.find('.list').position().left;
	if(!!(this.container.find('ul')) && !!(this.container.find('ul').position()) ){
		return this.container.find('ul').position().left;
	} else {
		return 0;
	}
};

ScollableTabs.prototype.itemPositionArray = function(){
  var targetWidths=$.map(this.container.find('ul li'),( function(e){ return $(e).outerWidth();})) || [];
  //console.log("targetWidths=" + JSON.stringify(targetWidths));
  //console.log(JSON.stringify(this.getLeftPos()));
  targetWidths.unshift(this.getLeftPos());
  targetWidths.push(0);
  return targetWidths.reduce((a,x,i)=>[...a, x + (a[i-1] || 0)], []);
};

ScollableTabs.prototype.dataValueToIndex = function(dataValue){
  //console.log('dataValue=' + dataValue);
  //console.log('search String=');
  //console.log(this.containerId +" div ul li a[data-value='" + dataValue + "']" );
  //var targetItem = $.find(this.containerId +" div ul li a[data-value='" + dataValue + "']");
  var targetItem = $("a[data-value='" + dataValue + "']");
  //console.log( "targetItem=" + JSON.stringify(targetItem) );
  //console.log('index=' + $(this.containerId +' div ul li a').index(targetItem) );
  return $(this.containerId +' div ul li a').index(targetItem) || 0;
};

ScollableTabs.prototype.scrollIntoView = function(dataValue){
  console.log('scrollIntoView');
  var targetIndex= 1+this.dataValueToIndex(dataValue);
  //console.log('targetIndex=' + targetIndex);
  var posArry = this.itemPositionArray();
  //console.log('posArry=' + posArry);
  var posPrevious = posArry[targetIndex-3] || posArry[targetIndex-2] || posArry[targetIndex-1] || 0;
  //console.log('posPrevious=' + posPrevious);
  var posNext = posArry[targetIndex+2] || posArry[targetIndex+1] || 0;
  //console.log('posNext=' + posNext);
  var delta =0;
  if(this.widthOfHidden()>0){
    if(posPrevious<=0){ // scrolling left, so leftPos increases
      if(-this.getLeftPos()<1*this.scrollBarWidths){
        //this.container.find('ul').animate({left:0},'slow');
        delta=-this.getLeftPos(); 
        this.leftSB.hide();
      } else {
         delta=-posPrevious;
         this.leftSB.show();
      }
      //delta should be positive here
      delta=Math.max(delta,0);
      if(0<this.widthOfHidden() - delta+this.getLeftPos()){ 
        this.rightSB.show();
      }
      this.container.find('ul').animate({left:"+=" + delta +"px"},'slow'); //left increases
    } else if ( posNext >= this.container.outerWidth() ){ //scrolling right, so leftPos decreases
      if(this.widthOfHidden() + this.getLeftPos() < 1*this.scrollBarWidths){
        delta= this.widthOfHidden() + this.getLeftPos();
        this.rightSB.hide();
      } else {
        delta=(posNext-this.container.outerWidth());
        this.rightSB.show();
      }
      //delta should be positive here
      delta=Math.max(delta,0);
      if( delta + this.getLeftPos() <0  ){
        this.leftSB.show();
      }
      if((this.widthOfHidden()-this.container.outerWidth()+this.getLeftPos()-delta)<this.scrollBarWidths){
        this.rightSB.hide();
      }
      this.container.find('ul').animate({left:"-=" + delta +"px"},'slow');
    }
  } else {
      this.rightSB.hide();
      this.leftSB.hide();
  }
};


ScollableTabs.prototype.reAdjust = function(){
  console.log('reAdjust');
  if( this.widthOfHidden()<=0){
    this.container.find('ul').animate({left:0},'fast');
    this.lastHiddenWidth=0;
    this.rightSB.hide();
    this.leftSB.hide();
    this.lastHiddenWidth=this.widthOfHidden();
  } else if(this.getLeftPos()>0) {
    this.container.find('ul').animate({left:0},'fast');
    this.leftSB.hide();
    this.rightSB.show();
    this.lastHiddenWidth=this.widthOfHidden();
  } else { // widthOfHidden>0 and leftPos<=0
    var deltaHidden = this.widthOfHidden() - this.lastHiddenWidth;
    this.lastHiddenWidth=this.widthOfHidden();
    if(deltaHidden>0){ // hidden increased: window got smaller
      this.rightSB.show();
    } else if (deltaHidden<0 ){ // hidden decreased: window got bigger
      // keep same pos unless we last tab comes into view
      // if last tab comes into view, scroll to end and hide right
      // last tab in container is -con
      // if this.widthOfHidden()-this.container.w
      var leftpos = this.container.outerWidth() - this.widthOfList();
      if( leftPos < this.getLeftPos() ){ // do nothing
      } else {
        if(leftPos> -this.scrollBarWidths){
          leftPos=0;
        }
        if(leftPos<0){
          this.leftSB.show();
        } else {
          this.leftSB.hide();
        }
        if(leftPos+this.widthOfList()<this.container.outerWidth()){
          this.rightSB.hide();
        } else {
          this.rightSB.show();
        }
        this.container.find('ul').animate({left:"=" + lftPos +"px"},'fast');
      }
      /*
      var delta = this.widthOfHidden()-this.container.outerWidth();
      var offset =-Math.max(0,this.widthOfHidden()-this.container.outerWidth());
      this.container.find('ul').animate({left:"=" + offset +"px"},'fast');
      if(offset<0){
        this.leftSB.show();
      } else {
        this.leftSB.hide();
      }
      this.rightSB.hide();
      */
    }
  }
};

//$('.scroller-right').click(function() 
ScollableTabs.prototype.rightClick =function(){
  console.log('rightClick');
  var m1 = this.container.outerWidth()-2*this.scrollBarWidths;
  //console.log('m1=' + m1);
  var m2 = (this.widthOfHidden()+ this.getLeftPos()); 
  //console.log('m2=' + m2);
    var delta = Math.min(m1,m2);
    //console.log('delta=' + delta);
    if(m2<=m1){
      this.rightSB.fadeOut('slow');
      delta=delta+8;
    }
    //console.log('again delta=' + delta);
    this.container.find('ul').animate({left:"-=" + delta + "px"},'slow',function(){});
    //$(this.upId).fadeIn('slow');
    
  this.leftSB.fadeIn('slow');
  //this.rightSB.fadeOut('slow');
  
   //this.container.find('.shiny-tab-input').animate({left:"+="+this.widthOfHidden()+"px"},'slow',function(){

};

//$('.scroller-left').click(function() {
 ScollableTabs.prototype.leftClick =function(){
   console.log('left click');
   this.rightSB.fadeIn('slow');
    var delta = Math.min(-this.getLeftPos(), this.container.outerWidth()-2*this.scrollBarWidths );
    if(-this.getLeftPos()<= this.container.outerWidth()-2*this.scrollBarWidths ){
      this.leftSB.fadeOut('slow');
    }
    this.container.find('ul').animate({left:"+=" + delta +"px"},'slow',function(){});
   
};


ScollableTabs.prototype.gotoEnd =function(){ 
  if(this.widthOfHidden()>=0 ){
    this.leftSB.fadeIn('slow');
    this.container.find('ul').animate({left:"=" + this.widthOfHidden() +"px"},'slow',function(){});
  }
};


   
ScollableTabs.prototype.init=function(){
	var ltabs=this;
	ltabs.container.find('.scroller-right').click( function(){ 
		  console.log('rightClick');
		  //console.log('ltabs.widthOfHidden()=' + ltabs.widthOfHidden());
		  ltabs.rightClick();
		  //ltabs.leftSB.fadeIn('slow');
		  //ltabs.rightSB.fadeOut('slow');
		   //ltabs.container.find('ul').animate({left:"+="+ltabs.widthOfHidden()+"px"},'slow',function(){

		  //});
	} );
	ltabs.container.find('.scroller-left').click( function(){
		console.log('leftClick');
		//console.log('ltabs.getLeftPos()=' + ltabs.getLeftPos());
		ltabs.leftClick();
		 //ltabs.rightSB.fadeIn('slow');
		 //ltabs.leftSB.fadeOut('slow');
		 //ltabs.container.find('ul').animate({left:"-="+ltabs.getLeftPosi()+"px"},'slow',function(){
		
		//});
	} );
};


$(document).ready(function(){
  stabs=new ScollableTabs( 'aceTabSet');
  stabs.init();
  
  stabs.reAdjust();
  $(window).on('resize',function(e){  
    	stabs.reAdjust();
  });
  $(window).on('splittermove', function(e){ 
    stabs.reAdjust();
  });
});

Shiny.addCustomMessageHandler(
  "scrollManager",
  function(data){
    if(!!data.resize){
      console.log('resize here');
      $(window).resize();
    }
    if(!!data.selected){
      stabs.scrollIntoView(data.selected);
    }
  }
);
