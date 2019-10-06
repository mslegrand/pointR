// Javascript
//BEGIN: INPUT BINDING
var widgetCntrlBinding = new Shiny.InputBinding();
$.extend(widgetCntrlBinding, {
    find: function(scope) {
        console.log('find');
        return $(scope).find(".widgetCntrl");
    },
    initialize: function(el){
        //  Initialize any data values here
        $(el).data("theta", $(el).attr("data-theta")); 
        $(el).data("R", $(el).attr("data-r")); 
        $(el).data('CXY',{
          x:Number($(el).attr("data-x")),
          y:Number($(el).attr("data-y"))
        });
    },
    getValue: function(el) {// used to return the value of the input control
        return $(el).data('theta'); //we return only theta (in degrees)
    },
    setValue: function(el, value) { // used for updating input control
       //  theta value is assumed to be in degrees
       $(el).data('theta',value);
       $(el).trigger("change");
    },
    subscribe: function(el, callback) {
        // notify server whenever change 
        $(el).on("change.widgetCntrlBinding", function(e) {
            callback();
        });
    },
    unsubscribe: function(el) {
        $(el).off(".widgetCntrlBinding");                              
    },
    receiveMessage: function(el, data) { //called by server when updating 
        if(!!data.value){
          var id=$(el).attr('id');
          var theta=data.value; //assumed to be in degrees
          this.updateNeedle(id,  (Math.PI*theta)/180); //adjust image
          this.setValue($(el), data.value); //record value
        }
    },
    mouse2pt: function(id, x, y){ //method to convert mouse coord to svg coord
      var thisSVG=document.querySelector("#" + id +" svg"); 
      var pt= thisSVG.createSVGPoint();
      pt.x = x;
      pt.y = y;
      return pt.matrixTransform(thisSVG.getScreenCTM().inverse());
    },   
    getCXY: function(id){
    	return $("#"+id).data('CXY');
    },
    pts2theta:function(q, p){ //compute theta 
    	return 0.5*Math.PI + Math.atan( (p.x-q.x)/(p.y-q.y) );
    },
    mouse2theta: function(id, x, y){
      var p=this.mouse2pt(id, x, y);
      var q= $("#"+id).data('CXY');
      return this.pts2theta(q, p);
    },
    polar2xy: function(q, theta, r){
    	return { x:q.x +  r*Math.cos(theta), y:q.y -  r*Math.sin(theta)};
    },
    updateNeedle:function(id, theta){ // assumes theta in radians
      var el='#'+id;
      var r=$(el).data('R');
      var q= $(el).data('CXY');
      var p=this.polar2xy(q,theta,r);
      $(el + " svg line").attr({"x2":p.x, "y2":p.y});
    	$(el + " svg circle:first").attr({"cx":p.x, "cy":p.y});
    },
    clicked: function(ctrlId, evt ){
      // compute theta (in radians)
      var theta = this.mouse2theta(ctrlId, evt.clientX, evt.clientY);
      this.updateNeedle(ctrlId,theta );
      //update value
      theta=Math.round((180 *theta) / Math.PI );
      this.setValue("#" + ctrlId, theta);
    }
});

// register input binding
Shiny.inputBindings.register(widgetCntrlBinding);

