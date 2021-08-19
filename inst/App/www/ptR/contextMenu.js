//------begin:-------stuff to handle custom context menu
 
$(function () {
    const svgRBlockSnip="```{r, ${1:title,} echo=${2:FALSE}, results='asis'}\nWH=c(500,300)\nsvgR(wh=WH,\n     #your custom code goes here\n     ${0:NULL}\n)\n```";
    const ptRBlockSnip= "```{r, ${1:title,} echo=${2:FALSE}, results='asis'}\nWH<-c(600,400)\n\n# Defined by mouse: edit with care!\nptR<-list(\n  x=tribble(\n    ~points,\n    matrix(0,2,0)\n  )\n)\n\nsvgR(wh=WH,\n     #your custom code goes here\n     ${0:NULL}\n)\n\n```";
    
    const DNDSSnip="\n\
*********************\n\
POPUP\n\
```\n\
${1:hint}\n\
```\n\
SNIPPET\n\
```\n\
${2:snippet}\n\
```\n\
SVGR\n\
```\n\
library(svgR)\n\
ptR<-list(\n\
  x=matrix(0,2,0)\n\
)\n\
svgR(\n\
  #your code here\n\
  ${0:NULL}\n\
)\n\
```\n\
*********************\n\
";
    function insideDNDS(editor ){
      let Range = ace.require('ace/range').Range;
      var curPos=editor.getCursorPosition();
      console.log("------------------------------");
      console.log(JSON.stringify(curPos));
      var row1=curPos.row;
      // check if curRow contains ```
      console.log('hello hello hello');
      let rngL1 =  new Range(row1, 0, row1, Infinity);
      //let line1 = editor.getSession().getTextRange(rngL1);
      //console.log('line1================'+line1);
      //let found=(line1.indexOf('***')>=0);
      let found=editor.find('***', { start:rngL1, range:rngL1});
      if(found){
        row1=row1+1; //move down
      }
      // setCurPos to row1
      // search down for row2
      // search to see if POPUp is inside
      //
      //console.log('found ***='+found);
      //alert('found ***='+found);
      return null;
    }
    
    function insideCodeBlock(editor ){
      let Range = ace.require('ace/range').Range;
      let curPos=editor.getCursorPosition();
      let rtv= false;
      console.log("------------------------------");
      console.log(JSON.stringify(curPos));
      let curRow=curPos.row;
      let curLine = editor.getSession().getLine(curRow);
      if(-1!==curLine.indexOf('```')){ //check if curLine contains ```
        return true;
      } else {
        // search up for ```
        let topRng=editor.find('```', {backwards:true, start:curPos, range:null, skipCurrent:false, wrap:false}, false);
        if(typeof topRng !== 'undefined'){
          // if found, does that line contain r,?
          let topRow=topRng.start.row;
          let curLine = editor.getSession().getLine(topRow);
          if(-1!==curLine.indexOf('r')){
            rtv= true;
          } 
        } 
        // restore curPos
        editor.moveCursorTo(curRow,0);
        // remove selection
        editor.clearSelection();
        return rtv;
      }
      // check if curRow contains ```
    }
    
    $('.clickMe').click(function () {
      var cmd = $(this).text().trim();
      console.log('cmd='+cmd);
      var aceId= $("#cntnr").attr('data-value');
      var $el = $( aceId);
      var editor = $el.data('aceEditor');
      let Range = ace.require('ace/range').Range;
      if(cmd==='Lookup element'){
        editor.getSession().selection.moveCursorLongWordLeft();
        editor.getSession().selection.selectWordRight();
        let text = editor.getSession().getTextRange(editor.getSelectionRange());
        Shiny.onInputChange('helpMssg', {query:text, num:Math.random(), editorId: aceId} );
      } else if(cmd=="Insert svgR Block"){
        //$el.trigger(tab_press);
        
        if(!insideCodeBlock(editor)){
          let snippet =svgRBlockSnip;
          let snippetManager = ace.require("ace/snippets").snippetManager;
          snippetManager.insertSnippet(editor, snippet);
          editor.focus();
        }
        
      } else if(cmd=="Insert ptR Block"){
        if(!insideCodeBlock(editor)){
          let snippet =ptRBlockSnip;
          let snippetManager = ace.require("ace/snippets").snippetManager;
          snippetManager.insertSnippet(editor, snippet);
          editor.focus();
        }
      } else if(cmd=='Insert DNDS Block'){
        // todo add check for placement of block
        let edPos=editor.getCursorPosition();
        //console.log('edPos'+JSON.stringify(edPos));
        let curRow  =editor.getCursorPosition().row;
        let curLine = editor.getSession().getLine(curRow);
        console.log('curLine='+curLine);
        console.log("curLine.indexOf('***')="+curLine.indexOf('***'));
        if(-1===curLine.indexOf('***')){
          // get botRow
          let botRow=editor.session.getLength();
          let botRng=editor.find('***', {backwards:false, start:edPos, range:null, skipCurrent:true, wrap:false},false);
          let topRng=editor.find('***', {backwards:true, start:edPos, range:null, skipCurrent:true, wrap:false}, false);
          //console.log('botRng='+JSON.stringify(botRng));
          //console.log('topRng='+JSON.stringify(topRng));
          //console.log('!botRng.isEmpty()'+!botRng.isEmpty() );
          if(typeof botRng !== 'undefined' && typeof topRng !== 'undefined'     ){
            botRow=botRng.start.row;
            let topRow=topRng.start.row;
            //console.log('range is ='+topRow+", "+botRow);
            // test if insideDNDS
            let trng= new Range(topRow, 0, botRow, Infinity);
            editor.moveCursorTo(topRow,0);
            //console.log('trange is ='+JSON.stringify(trng));
            let lines=editor.session.getTextRange(trng);
            //console.log(lines);
            if(-1===lines.indexOf('POPUP')){
              //console.log('POPUP not found');
              // keep let curRow number
            } else {
              //console.log('POPUP  found');
              // reset let curRow
              curRow=botRow;
            }
            editor.clearSelection();
          } else {
            //console.log('empty range');
          }
          
          // get topRow
          // get popUpRow
          editor.moveCursorTo(curRow,0);
          let snippet=DNDSSnip;

          let snippetManager = ace.require("ace/snippets").snippetManager;
          snippetManager.insertSnippet(editor, snippet);
          editor.scrollToLine(curRow, false, false, function () {});
          editor.focus();
        }
        
      } else if(cmd=="Edit Code Block" || cmd=='Edit DNDS Icon'){
        //insideDNDS(editor);
        let Range = ace.require('ace/range').Range;
        let rng1=null;
        
        parMode=null;
        if(cmd== "Edit Code Block"){
          parMode='ptrrmd';
          rng1=editor.find('```', {backwards:true, start:editor.getCursorPosition(), range:null});
          if(!rng1){ return null}
          
        } else { //cmd=='Edit DNDS Icon'
          parMode='dnippets';
          rng1=editor.find('***', {backwards:true, start:editor.getCursorPosition(), range:null});
          if(!rng1){ return null}
          //search for POPUP followed by ```,
          //rng1=editor.find('POPUP\n```\n', {backwards:false, start:editor.getCursorPosition(), range:null});
          //The next line should be the label
          rng1=editor.find('SVGR', {backwards:false, start:editor.getCursorPosition(), range:null});
          if(!rng1){ return null}
          rng1=editor.find('```', {backwards:false, start:editor.getCursorPosition(), range:null});
          if(!rng1){ return null}
        }
        
        //rng1 now points to the top of the block
        // extract row1 for anchor1
        let row1=rng1.start.row; //used for anchor1
        let label="";
        
        if(parMode=='ptrrmd'){
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
          
          label = toks.filter(tok => tok.match(/label=.*/));
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
        } else { //parMode='dnippets'
          // search upwards for POPUP
          let pos=editor.getCursorPosition();
          let rng0=editor.find('POPUP', {backwards:true, start:editor.getCursorPosition(), range:null});
          let row0=2+rng0.end.row;
          let rngL1 =  new Range(row0, 0, row0, Infinity);
          let text = editor.getSession().getTextRange(rngL1);
          //console.log('text='+text);
          label=text;
          editor.moveCursorToPosition(pos);
        }
        // add check for "{ r , }"" in row1, 
        
        
        let rng2=editor.find('```', {backwards:false, start:editor.getCursorPosition(), range:null});
        
        if(!rng2){ return null} // if not found exit
        
        let row2=rng2.start.row;
         // check .anchors to see if row1, row2 are taken
        let Anchor = ace.require('ace/anchor').Anchor;
        let ancs = editor.getSession().anchors;
        let childAceId=null;
        let ancTag=null;
        if(typeof editor.getSession().anchors !='undefined'){
            let ancs = editor.getSession().anchors;
            for(let k in ancs){
              let val=ancs[k];
              let r1=val.anc1.row;
              let r2=val.anc2.row;
              if(r1==r1 && r2== row2){
               ancTag=k;
              }
            }
            if(!!ancTag){
              $('.shiny-ace').each(function(){
                 let lid=this.id;
                 let editr = $('#'+lid).data('aceEditor'); 
                 if(!!editr.getSession().link ){
                   let link=editr.getSession().link;
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
                if(!!$('#'+childAceId)){
                  Shiny.onInputChange('messageContextMenu', 
                  {
                   cmd: "openTab",
                   id:childAceId,
                   parMode: parMode
                  }); 
                  return(null);
                }
              }
            }
        }
         
        let col=  editor.session.getLine(row2).length;
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
             label: label,
             parMode: parMode
            });
          
            // exit gracefully
            //alert(text);
          }
          
          
        } //end of edit code block
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

