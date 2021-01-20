//------begin:-------stuff to handle custom context menu
 
$(function () {
    $('.clickMe').click(function () {
      var cmd = $(this).text().trim();
      var aceId= $("#cntnr").attr('data-value');
      var $el = $( aceId);
      var editor = $el.data('aceEditor'); 
      if(cmd==='Lookup element'){
        editor.getSession().selection.moveCursorLongWordLeft();
        editor.getSession().selection.selectWordRight();
        let text = editor.getSession().getTextRange(editor.getSelectionRange());
        Shiny.onInputChange('helpMssg', {query:text, num:Math.random(), editorId: aceId} );
      } else if(cmd=="Edit Code Block"){
        
        editor.find('```', {backwards:true});
        let row1=editor.getSelectionRange().start.row;
        editor.find('```', {backwards:false});
        let row2=editor.getSelectionRange().start.row;
        //alert('row1=' + row1 +", row2=" + row2);
        let col=  editor.session.getLine(row2).length;
        let Range = ace.require('ace/range').Range;
        //alert(col);
        let range = new Range(row1, 0, row2, col);
        let doc= editor.session.getDocument();
        
        //import { Range } from "ace-builds"
        //alert(JSON.stringify(range));
        // let row = range.end.row;
        // let col=  editor.session.getLine(row).length;
        //range.end.column=Infinity;
      
        //alert(range);
        if( !range.isEmpty() ){ 
          let text = editor.getSession().getTextRange(range);
          let lines= text.split("\n");
          let patt=new RegExp("^```");
          let res0= patt.test(lines[0]);
          let res1 = patt.test(lines[lines.length-1]);
          let rngOk = res0 && res1;
          if(rngOk){
            let Anchor = ace.require('ace/anchor').Anchor;
            let anc1=new Anchor(doc,row1,0);
            let anc2=new Anchor(doc,row2,0);
            let anc={anc1: anc1, anc2:anc2};
            let rid=Math.random().toString(36).substring(8);
            if(typeof editor.getSession().anchors ==='undefined'){
              editor.getSession().anchors={};
            } 
            editor.getSession().anchors[rid]=anc;
            
            lines=lines.slice(1, lines.length-1);
            text=lines.join('\n');
            //alert(JSON.stringify(Object.keys(editor.getSession().anchors)));
            // set current tab to read only
            // send message to ptR to create new tab with text as content
            Shiny.onInputChange('messageContextMenu', 
            {
             start_row: row1,
             end_row: row2,
             code :  text,
             id:rid
            });
          
            // exit gracefully
            //alert(text);
          }
          
          
        }
      } else if(cmd==='Copy') {
        let text = editor.getSession().getTextRange(editor.getSelectionRange());
        //window.clipboard.writeText(text);
        window.writeText(text);
      } else if(cmd==='Cut') {
        let range = editor.getSelectionRange();
        if( !range.isEmpty() ){ 
          let text = editor.getSession().getTextRange(range);
          //window.clipboard.writeText(text);
          window.writeText(text);
          editor.getSession().replace(range,  ""); 
        } 
      } else if(cmd==='Paste') {
        let range = editor.getSelectionRange();
        //let text = clipboard.readText();
        let text = window.readText();
        editor.getSession().replace(range,  text); 
        console.log("You have selected "+ cmd + " with ace id = " +aceId);
      } else if(cmd==='Delete') {
        let range = editor.getSelectionRange();
        if( !range.isEmpty() ){
          editor.getSession().replace(range,  ""); 
        } 
      }
      else {
         alert("Default: You have selected "+cmd + " with ace id = " +aceId);
      }
    });
});

//-----end:--------stuff to hadle custom context menu

