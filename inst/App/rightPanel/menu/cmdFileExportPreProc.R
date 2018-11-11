
cmdPreProcPtsExport<-function(){
  click('buttonExportPreproc')
}


observeEvent(input$buttonExportPreproc,{
  fp.dt<-parseSavePath(c(home='~'), input$buttonExportPreproc)
  if(length(fp.dt)>0 ){
    fileName<-as.character(fp.dt$datapath[1])
    scripts<-getPreProcPtScript()
    txt<-paste0('list(\n',paste(names(scripts),'=',sprintf("'%s'",scripts), collapse=",\n"),"\n)")
    writeLines(txt, fileName)
  }
})

