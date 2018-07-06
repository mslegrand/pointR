

genShinyOpenFilesButtons<-function(){
  tagList(
    shinySaveButton("buttonExportSVG",        label="", title="Export as ...",        list('SVG'=c("SVG")) ,     class='hiddenButton'),
    shinyFilesButton("buttonFileOpen",        label="", title="Open File",            c('R','PTR', 'SVGR'),      multiple=FALSE,  class='hiddenButton'),
    shinyFilesButton("buttonSnippetImport",   label="", title="Import Snippets",      c('snippets'),             multiple=FALSE,  class='hiddenButton'),
    shinyFilesButton("buttonDnippetImport",   label="", title="Import Dnippets",      c('dnippets'),             multiple=FALSE,  class='hiddenButton'),
    shinyFilesButton("buttonPreProcPtImport", label="", title="Import PreProcessors for points",      
                                                                                      c('preprocpts'),          multiple=FALSE,   class='hiddenButton')
  )
}
