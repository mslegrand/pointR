

genShinyOpenFilesButtons<-function(){
  tagList(
    shinySaveButton("buttonSvgExport",        label="",  title="Export as ...",     filetype=list(svg='svg') ,          class='hiddenButton'),
    shinySaveButton("buttonPreprocPtExport",  label="", title="Export as ...",      filetype=list(R='R'),               class='hiddenButton'), 
    shinySaveButton("buttonPreprocAtExport",  label="", title="Export as ...",      filetype=list(R='R'),               class='hiddenButton'), 
    shinyFilesButton("buttonFileOpen",        label="", title="Open File",              c('R','PTR', 'SVGR', 'js'),      multiple=FALSE,  class='hiddenButton'),
    shinyFilesButton("buttonFileOpenProject", label="", title="Select Project to Open", c('pproj'),                multiple=FALSE,     class='hiddenButton'),
    shinyFilesButton("buttonSnippetImport",   label="", title="Import Snippets",        c('snippets'),             multiple=FALSE,     class='hiddenButton'),
    shinyFilesButton("buttonDnippetImport",   label="", title="Import Dnippets",        c('dnippets'),             multiple=FALSE,     class='hiddenButton'),
    shinyFilesButton("buttonPreProcPtImport", label="", title="Import PreProcessors for points",    c('R'), multiple=FALSE,   class='hiddenButton'),
    shinyFilesButton("buttonPreProcAtImport", label="", title="Import PreProcessors for values",    c('R'), multiple=FALSE,   class='hiddenButton')
  )
}
