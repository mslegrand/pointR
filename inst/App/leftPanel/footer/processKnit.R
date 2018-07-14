
processKnit<-reactive({
  clearErrorMssg()
  src<-request$code
  panels$sourceType<-rmdPanelTag
  
  if(length(src)==1 && nchar(src)>0){
    if(grepl("output: dnd_snippet",src)){
      src<-dripplets2Rmd(src)
    }
    tryCatch({
      knit2html(text = src, fragment.only = FALSE, quiet = TRUE)
      #knit2html(text = src, fragment.only = FALSE, quiet = TRUE)
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
})





