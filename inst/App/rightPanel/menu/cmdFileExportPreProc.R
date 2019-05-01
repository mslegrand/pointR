
cmdPreProcPtsExport<-function(){
  click('buttonExportPreproc')
}


observeEvent(input$buttonExportPreproc,{
  fp.dt<-parseSavePath(c(home='~'), input$buttonExportPreproc)
  if(length(fp.dt)>0 ){
    fileName<-as.character(fp.dt$datapath[1])
    scripts<-getPreProcPtScript()
    #saves as list
    #txt<-paste0('list(\n',paste(names(scripts),'=',sprintf("'%s'",scripts), collapse=",\n"),"\n)")
    
    txt0<-paste(names(scripts),'= function( pt, context, WH, keys){\n',scripts,"\n}", collapse=",\n")
    str_split(txt0, '\n')[[1]]->lines
    paste0("  ", lines,collapse="\n")->txt1
    txt<-paste0('preprocPts<-list(\n', txt1, "\n)")

    writeLines(txt, fileName)
  }
})

