shinyFileChooseFileTypes<-list(
  buttonFileOpen=c('R','PTR','SVGR','js','dnds'), #does not seem to have any effect. (i.e. Rmd not there, but still can open anyway)
  buttonFileOpenProject=c('pprj'),
  buttonSnippetImport=c('snip'),
  buttonDnippetImport=c('dnds'),
  buttonPreProcPtImport= c('R'),
  buttonPreProcAtImport=c('R')
)

for(name in names(shinyFileChooseFileTypes)){
  shinyFileChoose(input, name, session=session, roots=c(home="~"), filetypes=shinyFileChooseFileTypes[[name]] )
}
for( name in c( "buttonSvgExport", "buttonPreprocPtExport",   "buttonPreprocAtExport" )){
  shinyFileSave(input,  name, session=session, roots=c(home="~")  ) #hidden
}