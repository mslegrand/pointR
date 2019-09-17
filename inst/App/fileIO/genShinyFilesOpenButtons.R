

UIGenShinyOpenFilesButtons<-function(){
  tagList(
    shinySaveButton("buttonSvgExport",        label="",  title="Export as ...",     filetype=list(svg='svg') ,          class='hiddenButton'),
    shinySaveButton("buttonPreprocPtExport",  label="", title="Export as ...",      filetype=list(R='R'),               class='hiddenButton'), 
    shinySaveButton("buttonPreprocAtExport",  label="", title="Export as ...",      filetype=list(R='R'),               class='hiddenButton'), 
    
    shinyFilesButton("buttonFileOpen",        label="", title="Open File",                        multiple=FALSE,  class='hiddenButton'),
    shinyFilesButton("buttonFileOpenProject", label="", title="Select Project to Open",           multiple=FALSE,  class='hiddenButton'),
    
    shinyFilesButton("buttonSnippetImport",   label="", title="Import Snippets",                         multiple=FALSE,  class='hiddenButton'),
    shinyFilesButton("buttonDnippetImport",   label="", title="Import Dnippets",                           multiple=FALSE,  class='hiddenButton'),
    shinyFilesButton("buttonPreProcPtImport", label="", title="Import PreProcessors for points",                  multiple=FALSE,   class='hiddenButton'),
    shinyFilesButton("buttonPreProcAtImport", label="", title="Import PreProcessors for values",                  multiple=FALSE,   class='hiddenButton')
  )
}

