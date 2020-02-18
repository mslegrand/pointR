// JAVASCRIPT

//INPUT BINDING
var shinyInputControlBinding = new Shiny.InputBinding();
$.extend(shinyInputControlBinding, {
    find: function(scope) {
        return $(scope).find(".shinyInputControl");
    },
    initialize: function(el){
        //  STEP 3.2 Initialize element data here:
        // may use the dnds: 
        //  1. get element data, 'from string dnd' 
    },
    getValue: function(el) {
      // Used for returning the value(s) of this input control
      // Typically,  held as element data, ie. $(el).data('value')
      
      var value = $(el).data('value');
      // if value is an object, may want to use JSON.stringify
      return value ;
    },
    setValue: function(el,  value) { 
      // used for updating input control
      // Typically
      //  1. set element data value
          $(el).data('value', value)
      //  2. then trigger element change
          $(el).trigger("change");
    },
    subscribe: function(el, callback) {
        // notify server whenever change 
        $(el).on("change.shinyInputControlBinding", function(e) {
            callback();
        });
    },
    unsubscribe: function(el) {
        $(el).off(".shinyInputControlBinding");                              
    },
    receiveMessage: function(el, data) { //called when server sends update message
        if(!!data.value){
          // handle update here
          // Step 6.3
          // Typically:
          // 6.3.1. extract value(s) from data: Xval dnd
          // 6.3.2. Possibly convert to object: may use 'fromString' dnd
          // 6.3.3. set element with new data: may use Sval dnd
          // 6.3.4. update svg rendering (if necessary :) (svg-tree dnd)
        }
    },
    // STEP 4.1 add handler clicked
    
    getType: function(el){ 
      return "shinyInputControlBinding";
    }
});

// REGISTER INPUT BINDING
Shiny.inputBindings.register(shinyInputControlBinding);

