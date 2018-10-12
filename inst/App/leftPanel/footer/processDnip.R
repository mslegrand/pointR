
processDnip<-reactive({
  cat(">---> processDnip\n")
  clearErrorMssg()
  src<-request$code
  panels$sourceType<-rmdPanelTag
  if(length(src)==1 && nchar(src)>0){
    
    tryCatch({
      cat('>--> dripplets2Rmd\n')
      src<-dripplets2Rmd(src)
      cat('<--< dripplets2Rmd\n')
      cat('>-->> knit2html\n')
      knit2html(text = src, fragment.only = FALSE, quiet = TRUE)
      cat('<--<< knit2html\n')
      setSourceType(sourceType=rmdPanelTag)
    } 
    , #end of try
    error=function(e){
      if(all(!str_detect(e,'Output:'))){
        e<-c(e,traceback())
      }
      err<-paste(unlist(e), collapse="\n", sep="\n")
      setErrorMssg(err)
    }
    ) #end tryCatch
  } #end if
  cat("<---< processDnip\n")
})
