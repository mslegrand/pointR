// ace customizations

Shiny.addCustomMessageHandler(
  "shinyAceExt",
      function(data) { 
        var id = data.id;
        var $el = $('#' + id);
        var editor = $el.data('aceEditor');
        
        alert(el);
        if(data.ptRMode){ 
          editor.session.setMode({path: "ace/mode/ptr", v: Date.now()});
          editor.setBehavioursEnabled(true);
          
        }
      }
  );
