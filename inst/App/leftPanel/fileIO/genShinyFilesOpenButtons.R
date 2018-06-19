

genShinyOpenFilesButtons<-function(){
  tagList(
    shinySaveButton("buttonExportSVG", label="", title="Export as ...",    list('SVG'=c("SVG")) ,    class='hiddenButton'),
    shinyFilesButton("buttonFileOpen", label="", title="Open File",     c('R','PTR', 'SVGR'),   multiple=FALSE, class='hiddenButton'),
    shinyFilesButton("buttonSnippetOpen",    label="", title="Import Snippet", multiple=FALSE,  class='hiddenButton')
  )
}

# genShinySaveFilesButtons<-function(){
#   tagList(
#     shinySaveButton( id="buttonFileSaveR",  label="", title="Save as ...",     class='hiddenButton'),
#     shinySaveButton( id="buttonFileSaveRmd",     label="", title="Save as ...",    filetype=list(Rmd='Rmd',  R='R', text='txt'), class='hiddenButton'),
#     shinySaveButton( id="buttonExportSVG", label="", title="Save as ...",    filetype=list('hidden_mime_type'=c("SVG")) ,    class='hiddenButton')
#   )
# }
# 
