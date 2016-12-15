// ace customizations
var ptr_HighlightedLines=[];


//Used exclusively for swapping colors of svgR keywords
function getStyleRule(name) {
    var ix, 
    sheet= $('#shinyAceStyle')[0]['sheet'];
    for (ix=0; ix<sheet.cssRules.length; ix++) {
      if (sheet.cssRules[ix].selectorText === name){
        return sheet.cssRules[ix].style;
      }        
    }
  return null;
}



Shiny.addCustomMessageHandler(
  "shinyAceExt",
      function(data) { 
        var id = data.id;
        var $el = $('#' + id);
        var editor = $el.data('aceEditor');
        var HighlightedLines;
        
        if(data.ptRMode){ 
          editor.getSession().setMode({path: "ace/mode/ptr", v: Date.now()});
          editor.setBehavioursEnabled(true);
        }
        if ( data.addMarker ){
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
        if(data.tabSize){
          editor.getSession().setTabSize( data.tabSize[0] );
        }
        if(data.resetElementColor){
          
          $.each(data.resetElementColor, function(key,element){
            //alert('key: ' + key + '\n' + 'value: ' + element);
            var rule=getStyleRule(key);
            rule.color=element;
           });
        }
        // want to set json options
        //todo: add more messaging capablilities
      }
  );
