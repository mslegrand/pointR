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
        let Range = ace.require('ace/range').Range;
        let rng1=editor.find('```', {backwards:true, start:editor.getCursorPosition()});
        if(!rng1){ return null}
        let row1=rng1.start.row;
        // add check for "{ r , }"" in row1, 
        let rngL1 =  new Range(row1, 0, row1, Infinity);
        let line1 = editor.getSession().getTextRange(rngL1);
        if(!/\Wr\W/.test(line1)){ // if not found exit
          return null;
        }
        // split line and look for r, and label
        // remove ```, {, } and split
        let toks=line1.replace("```","").replace(/ /g,"").replace("{","").replace("}","").split(",");
        // search for r
        let hasR = toks.filter(tok => tok=='r').length==1;
        if(!hasR){
          return null;
        }
        let label = toks.filter(tok => tok.match(/label=.*/));
        if(label.length===1){
          label=label[0].replace(/label='/,"").replace("'","").replace('"','');
        } else {
          label =toks.filter(tok=>!tok.match('=')).filter(tok=>tok!='r');
          if(label.length==1){
            label=label[0].replace("'","").replace('"','');
          } else {
            label="";
          }
        } 

        let rng2=editor.find('```', {backwards:false, start:editor.getCursorPosition()});
        if(!rng2){ return null} // if not found exit
        let row2=rng2.start.row;
         // check .anchors to see if row1, row2 are taken
        let Anchor = ace.require('ace/anchor').Anchor;
        let ancs = editor.getSession().anchors;
        // console.log('----------- ancs='+JSON.stringify(ancs));
        // console.log("------------> typeof ancs" +typeof ancs);
        let childAceId=null;
        let ancTag=null;
        if(typeof editor.getSession().anchors !='undefined'){
            let ancs = editor.getSession().anchors;
            for(let k in ancs){
              let val=ancs[k];
              // console.log( 'k='+k);
              let r1=val.anc1.row;
              let r2=val.anc2.row;
              //console.log('-------- r1='+r1," r2="+r2+" ----------");
              if(r1==r1 && r2== row2){
               ancTag=k;
              }
            }
            // console.log(JSON.stringify(ancTag));
            if(!!ancTag){
              $('.shiny-ace').each(function(){
                 let lid=this.id;
                 // console.log('lid='+lid);
                 let editr = $('#'+lid).data('aceEditor'); 
                 if(!!editr.getSession().link ){
                   let link=editr.getSession().link;
                   // console.log('link is:'+JSON.stringify(link));
                   if(link.length !== undefined){
                      let res=link[0].split(".");
                      let rid=res[1];
                      if(rid==ancTag){
                        childAceId=lid;
                      }
                   } 
                 }
              });
              if(!!childAceId){
                // console.log('>>>>>>>>>> childAceId exists=' + childAceId); 
                if(!!$('#'+childAceId)){
                  Shiny.onInputChange('messageContextMenu', 
                  {
                   cmd: "openTab",
                   id:childAceId
                  }); 
                  return(null);
                }
              }
            }
        }
         
        let col=  editor.session.getLine(row2).length;
        
        //alert(col);
        let range = new Range(row1, 0, row2, col);
        let doc= editor.session.getDocument();
        
        
        if( !range.isEmpty() ){ 
          let text = editor.getSession().getTextRange(range);
          let lines= text.split("\n");
          let patt=new RegExp("^```");
          let res0= patt.test(lines[0]);
          let res1 = patt.test(lines[lines.length-1]);
          let rngOk = res0 && res1;
          if(rngOk){
            //let Anchor = ace.require('ace/anchor').Anchor;
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
             cmd: "newTab",
             start_row: row1,
             end_row: row2,
             code :  text,
             id:rid,
             label: label
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
        // console.log("You have selected "+ cmd + " with ace id = " +aceId);
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

