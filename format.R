
sp<-"  "

formatPts<-function(pts){
  if(length(pts)==0 ){
    return("c()")
  } else{
    tmp<-unlist(pts)
    tmp<-matrix(tmp,2)
    tmp<-apply(tmp, 2, function(x)paste(x,collapse=","))
    tmp<-paste("c(",tmp,")")
    tmp<-paste(tmp, collapse=",")
    tmp<-paste0("matrix(\n",sp,sp,"c(", tmp, "),\n,",sp,"2,)")
    return(tmp)
  }
}


formatPtDefs<-function(defTag, ptRList){
  fPtDefs<-sapply(ptRList, formatPts)
  tmp<-paste0(sp,names(fPtDefs),"=",fPtDefs,collapse=",\n")
  replacement<-paste0(defTag,"<-list(\n",tmp,"\n)")
  return(replacement)
}


formatDFValue<-function(df){
  if(nrow(df)==0 ){
    return("c()")
  } else {
    dfNames<-names(df)
    dfTxt<-sapply(dfNames, function(name){
      value<-df[[name]]
      if( !is.numeric(value) ){
        value<-paste0('"',value,'"') # value=c('"a"', '"b"', '"c"',...)
      } 
      value<-paste(value, collapse=", ")  #value ='a, b, c, '
      paste0(sp,name,"=c(",value,")")
    })
    dfTxt2<-paste0(sp, dfTxt, collapse=",\n")
    return(dfTxt2)
  }
}

formatDFDefs<-function(dfDefs, dfDefsTag="ptR.df"){
  dfdNames<-names(dfDefs)
  tmp0<-lapply(dfdNames, function(nm){
    df<-dfDefs[[nm]]
    dfVal<-formatDFValue(df)
    dfSpec<-c(
      paste0(sp,nm, "=data.frame("), 
      dfVal, 
      paste0(sp,")")
    )
    dfSpec<-paste0( dfSpec, collapse="\n")
  })
  tmp1<-paste0(tmp0, collapse=",\n")
  tmp2<-paste0(
    paste0(dfDefsTag,"<-list(\n"),
    tmp1,
    "\n)\n"
  )
}    

