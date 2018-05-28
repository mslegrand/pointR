
processKnit<-reactive({
  clearErrorMssg()
  src<-getMarkdown() #getCode() #input$source #------ace editor
  panels$sourceType==rmdPanelTag
  if(length(src)==1 && nchar(src)>0){
    tryCatch({
      knit2html(text = src, fragment.only = TRUE, quiet = TRUE)
      setSourceType(sourceType=rmdPanelTag)
     } , #end of try
      error=function(e){ 
        if(all(!str_detect(e,'Output:'))){
          e<-c(e,traceback())
        }
      err<-paste(unlist(e), collapse="\n", sep="\n")
      setErrorMssg(err)
    }) #end tryCatch
  } #end if
})