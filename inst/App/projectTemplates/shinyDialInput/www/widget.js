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
        $(el).data('dXY',{
          x:Number($(el).attr("data-dx")),
          y:Number($(el).attr("data-dy"))
        });
        $(el).data('CXY',{
          x:Number($(el).attr("data-x")),
          y:Number($(el).attr("data-y"))
        });
    },
    getValue: function(el) {// used to return the value of the input control
        //return $(el).data('dXY'); //we return mouse position relative to svg
        return {
          dXY:$(el).data('dXY'),
          CXY:$(el).data('CXY')
        };
    },
    setValue: function(el, value) { // used for updating input control
       //  theta value is assumed to be in degrees
       $(el).data('dXY',value);
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
          //var id=$(el).attr('id');
          var htm=data.value; //assumed to be in degrees
          var node=jQuery.parseHTML( htm );
          $(el).empty().append(node);
          //this.setValue($(el), data.value); //record value
        }
    },
    mouse2pt: function(id, x, y){ //method to convert mouse coord to svg coord
      var thisSVG=document.querySelector("#" + id +" svg"); 
      var pt= thisSVG.createSVGPoint();
      pt.x = x;
      pt.y = y;
      return pt.matrixTransform(thisSVG.getScreenCTM().inverse());
    },
    clicked: function(ctrlId, evt ){
      var pt = this.mouse2pt(ctrlId, evt.clientX, evt.clientY);
        var el = "#" + ctrlId;
      this.setValue("#" + ctrlId, 
        {
          x: pt.x-$(el).data("CXY").x, 
          y: pt.y-$(el).data("CXY").y
        } 
      );
    },
    getType: function(el){ 
      return "widgetCntrlBinding";
    }
});

// register input binding
Shiny.inputBindings.register(widgetCntrlBinding);

