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
  
  # Todo for inter tib move support
  # selection -> 1 or more selections
  # row (currently corresponding to single name) - sets of rows
  #  might implement by either 
  #  1. replace rows by named list of rows (names are tib names)
  #  or 
  #  2. multiple calls to eval preproc:
  #      a. creat a list of contexts
  #         i. for each non-empty selection in rowGroupsDB(db) add a context
  #      b. loop over contexts either by
  #         i looping and calling eval for each context
  #         ii providing the list to the user, have the user loop
  
  
  # Issues: we suppose that we derive context from rowGroupDB, but
  # rowGroupDB contains
  # - tabId: should be fixed for this page
  # - name: the asset name (tib name)
  # - rows: the indices of the selected rows
  
  # 1. we currently don't know the ptCol to translate, (add to rowGroupDB?)
  # 2. the user might corrupt the tibs before we get to the given tib
  
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
        
        ppenv<-list(
          keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey),
          WH=getSVGWH()
        )
        # LOOP: for each context in context list, form context (remember to do current asset + col last)
        
        context<-list(
          name=getAssetName(),
          column=getTibPtColPos(),
          row=row,
          tibs=getPtDefs()$tib
        )
        tibs<-eval(parse(text=txt), ppenv )
        validateTibLists(getPtDefs()$tib, tibs)
        newPtDefs$tib<-tibs
        # matCol index update for all or just current???
        matCol<-ncol(tibs[[getAssetName()]][row[1], getTibPtColPos()] )
        # end LOOP
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