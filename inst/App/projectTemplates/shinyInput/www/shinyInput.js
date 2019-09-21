// JAVASCRIPT

//INPUT BINDING
var shinyInputCntrlBinding = new Shiny.InputBinding();
$.extend(shinyInputCntrlBinding, {
    find: function(scope) {
        console.log('find');
        return $(scope).find(".shinyInputCntrl");
    },
    initialize: function(el){
        //  Initialize any data values here
        // here we initial Z and the current Index
        let iniZ=$(el).attr(`data-Z`); //extract attribute
        $(el).data("Z", JSON.parse(iniZ)); //convert to an object and attach
        $(el).data('Index',0); // set index
    },
    getValue: function(el) {
    // used to return the value of this input control
    // here we Z and which index was changed
        return {
            Z: JSON.stringify($(el).data('Z')),
            Index: $(el).data('Index')
        };
    },
    setValue: function(el, index, value) { 
      // used for updating input control
      let Z=$(el).data("Z");
      $(el).index=index;
      Z[index]={
          re:value[0],
          im:value[1]
      }
      $(el).data('Z',Z);
      $(el).data('Index',index);
      $(el).trigger("change");
    },
    subscribe: function(el, callback) {
        // notify server whenever change 
        $(el).on("change.shinyInputCntrlBinding", function(e) {
            callback();
        });
    },
    unsubscribe: function(el) {
        $(el).off(".shinyInputCntrlBinding");                              
    },
    receiveMessage: function(el, data) { //called by server when updating 
        if(!!data.value){ 
          var htm=data.value; //htm is string represented a node
          var node=jQuery.parseHTML( htm );
          $(el).empty().append(node);
          
          $(el).data("Z", data.Z);
           // alternatively, set value but be careful about index
        }
    },
    mouse2pt: function(id, x, y){ //method to convert mouse coord to svg coord
      var thisSVG=document.querySelector("#" + id ); 
      thisSVG=document.querySelector("svg#" + id ); 
      var pt= thisSVG.createSVGPoint();
      pt.x = x;
      pt.y = y;
      return pt.matrixTransform(thisSVG.getScreenCTM().inverse());
    },
    clicked: function(ctrlId, index, evt){
       let svgId=ctrlId + index; 
       let pt = this.mouse2pt(svgId, evt.clientX, evt.clientY);
       let el =  "#" + ctrlId;
       let value =[pt.x, - pt.y] ;
       this.setValue(el, index, value);
    },
    getType: function(el){ 
      return "shinyInputCntrlBinding";
    }
});

// REGISTER INPUT BINDING
Shiny.inputBindings.register(shinyInputCntrlBinding);

