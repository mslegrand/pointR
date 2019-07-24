//------begin:-------stuff to handle custom context menu

// window.sendToElectron('cmdAppRun',data.app2RunPath, data.tabId);

/*
function copy2clip(txt){
  var cb = document.getElementById("cb");
  cb.value = txt;
  cb.style.display='block';
  cb.select();
  $(document).execCommand('copy');
  cb.style.display='none';
 };
*/
 
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
      } 
      else if(cmd==='Copy') {
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

//-----end:--------stuff to handle custom context menu
