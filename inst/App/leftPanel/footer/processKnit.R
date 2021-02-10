
processKnit<-reactive({
  # cat_list<<-c( cat_list,">---> processKnit\n")
  clearErrorMssg()
  #src<-request$code
  updateAceExt( id= getAceEditorId(), sender='commit.removeMarkers', removeAllMarkers='removeAllMarkers', updateRmdDependents=getAceEditorId() )
  
  src<-getCode()
  
  setSourceType(rmdPanelTag)
  
  if(length(src)==1 && nchar(src)>0){
    if(grepl("output: dnd_snippet",src)){
      # cat_list<<-c( cat_list,'>--> dripplets2Rmd\n')
      src<-dripplets2Rmd(src)
      # cat_list<<-c( cat_list,'<--< dripplets2Rmd\n')
    }
    tryCatch({
      # cat_list<<-c( cat_list,'>--> knit2html\n')
      knit2html(text = src, fragment.only = FALSE, quiet = TRUE, envir=new.env())
      # cat_list<<-c( cat_list,'<--< knit2html\n')
      setSourceType(sourceType=rmdPanelTag)
     } 
     , #end of try
      error=function(e){
        e<-e$message
        if(all(!str_detect(e,'Output:'))){
          e<-c(e,traceback())
        }
      err<-paste(unlist(e), collapse="\n", sep="\n")
      setErrorMssg(err)
    }
    ) #end tryCatch
  } #end if
  # cat_list<<-c( cat_list,"<---< processKnit\n")
})





