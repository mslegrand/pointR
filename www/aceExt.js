// ace customizations
var ptr_HighlightedLines=[];

Shiny.addCustomMessageHandler(
  "shinyAceExt",
      function(data) { 
        var id = data.id;
        var $el = $('#' + id);
        var editor = $el.data('aceEditor');
        var HighlightedLines;
        
        //alert(el);
        if(data.ptRMode){ 
          editor.getSession().setMode({path: "ace/mode/ptr", v: Date.now()});
          editor.setBehavioursEnabled(true);
        }
        if ( data.addMarker ){
          //alert("hello");
          var pos = data.addMarker;
          var row1 = pos[0]; 
          var col1 = pos[1]; 
          var row2 = row1+1; 
          var Range = ace.require("ace/range").Range;
          var mid= editor.getSession().addMarker(
              new Range(row1, 0, row2, 1), 
              'ace_error-marker', 
              'line', true
          );
          ptr_HighlightedLines.push(mid); 
          console.log("marker added-> " + mid);
        }
        if(data.removeAllMarkers){
          while(ptr_HighlightedLines.length>0){
            var highlightedLine = ptr_HighlightedLines.pop();
            editor.getSession().removeMarker(highlightedLine);
          }      
        }
        
        //todo: add more messaging capablilities
      }
  );
