
processKnit<-reactive({
  clearErrorMssg()
  #src<-getCode() #input$source #------ace editor
  src<-request$code
  panels$sourceType<-rmdPanelTag
  if(length(src)==1 && nchar(src)>0){
    #tryCatch({
      knit2html(text = src, fragment.only = FALSE, quiet = TRUE)
      setSourceType(sourceType=rmdPanelTag)
     #} 
    #  , #end of try
    #   error=function(e){ 
    #     if(all(!str_detect(e,'Output:'))){
    #       e<-c(e,traceback())
    #     }
    #   err<-paste(unlist(e), collapse="\n", sep="\n")
    #   setErrorMssg(err)
    # }
    #) #end tryCatch
  } #end if
})
