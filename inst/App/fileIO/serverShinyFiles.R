# shinyFileChooseFileTypes<-list(
#   buttonFileOpen=c('R','PTR','SVGR','js','dnds'), #does not seem to have any effect. (i.e. Rmd not there, but still can open anyway)
#   buttonFileOpenProject=c('pprj'),
#   buttonSnippetImport=c('snip'),
#   buttonDnippetImport=c('dnds'),
#   buttonPreProcPtImport= c('R'),
#   buttonPreProcAtImport=c('R')
# )

# 
# for(name in names(shinyFileChooseFileTypes)){
#   filetypes=shinyFileChooseFileTypes[[name]]
#   #filetypes=c('pprj')
#   if(name=='buttonFileOpenProject'){
#     #browser()
#     filetypes=c('pprj')
#   }
#   #browser()
#   shinyFileChoose(input, name, session=session, roots=c(home="~"), 
#                   #filetypes=c('pprj'))
#                   filetypes=filetypes)
#                   #filetypes=shinyFileChooseFileTypes[[name]] )
# }

shinyFileChoose(input, "buttonFileOpen",  session=session, roots=c(home="~"),
                filetypes=c('R','PTR','SVGR','js','dnds', 'Rmd'))
shinyFileChoose(input,"buttonFileOpenProject",  session=session, roots=c(home="~"), filetypes=c('pprj'))
shinyFileChoose(input,"buttonSnippetImport",  session=session, roots=c(home="~"), filetypes=c('snip'))
shinyFileChoose(input,"buttonDnippetImport",  session=session, roots=c(home="~"), filetypes=c('dnds'))
shinyFileChoose(input,"buttonPreProcPtImport",  session=session, roots=c(home="~"), filetypes=c('R'))
shinyFileChoose(input,"buttonPreProcAtImport",  session=session, roots=c(home="~"), filetypes=c('R'))



for( name in c( "buttonSvgExport", "buttonPreprocPtExport",   "buttonPreprocAtExport" )){
  shinyFileSave(input,  name, session=session, roots=c(home="~")  ) #hidden
}