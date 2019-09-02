var hidWidth;


function ScollableTabs( containerId ){
	this.scrollBarWidths = 40;
	
	this.lastHiddenWidth=0;
	this.containerId =  "#" +  containerId;
	this.container = $(this.containerId);
	
	var rightSB = 
	$('<div class="scroller scroller-right"><i class="glyphicon glyphicon-chevron-right"></i></div>');
	
	rightSB.insertAfter(this.container.find('ul'));
	var leftSB = 
	$('<div class="scroller scroller-left"><i class="glyphicon glyphicon-chevron-left"></i></div>');
	
	leftSB.insertAfter(this.container.find('ul'));
	
	this.rightSB =   this.container.find('.scroller-right');
	this.leftSB =  this.container.find('.scroller-left');
	
	return this;
}

ScollableTabs.prototype.widthOfList = function(){
   var itemsWidth = 0;
   this.container.find('ul li').each(function(){ //li elements 
   
    var itemWidth = $(this).outerWidth();
    itemsWidth+=itemWidth;
  });
  
  return itemsWidth;
};





ScollableTabs.prototype.widthOfHidden = function(){

  var rtv= this.widthOfList()- this.container.outerWidth();
  if(rtv<0){
    rtv=0;
  }
  return rtv;
};


ScollableTabs.prototype.getLeftPos = function(){
	
	if(!!(this.container.find('ul')) && !!(this.container.find('ul').position()) ){
		return this.container.find('ul').position().left;
	} else {
		return 0;
	}
};

ScollableTabs.prototype.itemPositionArray = function(){
  var targetWidths=$.map(this.container.find('ul li'),( function(e){ return $(e).outerWidth();})) || [];

  targetWidths.unshift(this.getLeftPos());
  targetWidths.push(0);
  return targetWidths.reduce((a,x,i)=>[...a, x + (a[i-1] || 0)], []);
};

ScollableTabs.prototype.dataValueToIndex = function(dataValue){
  
  var targetItem = $("a[data-value='" + dataValue + "']");

  return $(this.containerId +' div ul li a').index(targetItem) || 0;
};


ScollableTabs.prototype.getAllTabValues = function(){
  return $.map( this.container.find('div ul li a[data-toggle="tab"]'), function(e){ return $(e).data('value')});  
};

ScollableTabs.prototype.getAllDocPaths = function(){
  return $.map( this.container.find('.shiny-ace'), function(e){ return $(e).data('docFilePath')});
};


ScollableTabs.prototype.resetTitleGivendataValue = function( newTitle, dataValue){
  $(this.containerId +" div ul li a[data-value='" + dataValue + "'] span").replaceWith(newTitle);
};

ScollableTabs.prototype.getTitleGivendataValue = function( dataValue){
  var innText=$(this.containerId +" div ul li a[data-value='" + dataValue + "'] span span.tabTitle").text().trim();
  return innText;
};

ScollableTabs.prototype.toggleSaveState = function( tabId, state){
  // console.log('title='+ $(this.containerId +" div ul li a[data-value='" + tabId + "'] span span.tabTitle").text().trim());
  $(this.containerId +" div ul li a[data-value='" + tabId + "'] span span.tabTitle").toggleClass('star', state) ;
  return true;
};

ScollableTabs.prototype.scrollIntoView = function(dataValue){
  //console.log('scrollIntoView');
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
  //console.log('reAdjust');
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
      var leftPos = this.container.outerWidth() - this.widthOfList();
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
        this.container.find('ul').animate({left:"=" + leftPos +"px"},'fast');
      }
      
    }
  }
};


ScollableTabs.prototype.rightClick =function(){
  //console.log('rightClick');
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


 ScollableTabs.prototype.leftClick =function(){
   //console.log('left click');
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
		  ltabs.rightClick();
	} );
	ltabs.container.find('.scroller-left').click( function(){
		ltabs.leftClick();
	} );
	
	$(function(){ // make the tabs reorderable
	  $(ltabs.containerId).find('ul').sortable();
	  $(ltabs.containerId).find('ul').disableSelection();

	});
	
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
  
  $('#editNavBar').mousedown(function(){
    var paths=stabs.getAllDocPaths();
  });
});

Shiny.addCustomMessageHandler(
  "scrollManager",
  function(data){
    console.log("----------entering  scrollManager------------");
    // console.log("data is" + JSON.stringify(data));
    if(!!data.resize){
      $(window).resize();
    }
    if(!!data.selected){
      stabs.scrollIntoView(data.selected);
    }
    if(!!data.tabId){ //aka tabId
      if(!!data.title){
        stabs.resetTitleGivendataValue( data.title, data.tabId);
        stabs.reAdjust();
      }
      if(!!data.savedStatus){
        stabs.toggleSaveState(data.tabId, data.savedStatus!=='saved');
      }
    }
    
    if(!!data.getAllTabIds){
      var tabIds = stabs.getAllTabValues();
      Shiny.onInputChange('tabManager', {
        tabs:tabIds, 
        sender: data.sender, 
        rnd: Math.random().toString(36).substring(7)
      } );
    }
    console.log("----------exiting  scrollManager------------");
  }
);
