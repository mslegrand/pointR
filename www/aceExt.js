// ace customizations

Shiny.addCustomMessageHandler(
  "shinyAceExt",
      function(data) { 
        var id = data.id;
        var $el = $('#' + id);
        var editor = $el.data('aceEditor');
        
        //alert(el);
        if(data.ptRMode){ 
          editor.getSession().setMode({path: "ace/mode/ptr", v: Date.now()});
          editor.setBehavioursEnabled(true);
        }
        //todo: add more messaging capablilities
      }
  );
