mouseCmdMoveMatrix<-function(mssg){
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  replacementList<-list()
  newPtDefs<-getPtDefs() 
  
  sender='tagDrag.mouse'
  id<-mssg$id
  dxy<-vec 
  tmp<-unlist(str_split(id,"_")) 
  row<-as.numeric(tail(tmp,1)) #this should be the same as selected row index
  selection<-getAssetName() 
  matColIndx<-ncol(newPtDefs$tib[[selection]][[ row, getTibPtColPos() ]])
  
  if( mssg$ctrlKey==TRUE){ #add row to rowGroupsDB
    updateRowPicker(session, "myTibRowCntrl", addToGroup = row)
    pageId<-getTibTabId()
    row0<-filter(rowGroupsDB(), tabId==pageId & name==selection & rows!=row)$rows
    row<-c(row,row0)
  }  else {
    updateRowPicker(session, "myTibRowCntrl", removeEntireGroup=TRUE)
  }
  
  txt<-getPreProcScript()['onMoveMat']
  if( !is.null(txt) ){
      tryCatch({ 
        getDxy<-function(){names(dxy)<-c('dx','dy'); dxy}
        context<-list(
          name=getAssetName(),
          column=getTibPtColPos(),
          row=row,
          tibs=getPtDefs()$tib
        )
        ppenv<-list(
          keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey),
          WH=getSVGWH()
        )
        tibs<-eval(parse(text=txt), ppenv )
        validateTibLists(getPtDefs()$tib, tibs)
        newPtDefs$tib<-tibs
        matCol<-ncol(tibs[[getAssetName()]][row[1], getTibPtColPos()] )
        updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=row[1], matCol=matCol))  
      },error=function(e){
        e<-c('preproErr',unlist(e))
        err<-paste(unlist(e), collapse="\n", sep="\n")
        alert(err)
      })
  } else {
    matCol<-NULL
    for(arow in row){
       m<-newPtDefs$tib[[selection]][[ arow, getTibPtColPos() ]]
       newPtDefs$tib[[selection]][[ arow, getTibPtColPos() ]]<-m+vec
       if(is.null(matCol)){
         matCol<-ncol(m)
       }
    }
    
    updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=row[1], matCol=matCol)) 
  }
  
  
  
   
}