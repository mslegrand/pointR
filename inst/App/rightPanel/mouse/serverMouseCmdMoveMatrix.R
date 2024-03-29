mouseCmdMoveMatrix<-function(mssg){
  log.fin(mouseCmdMoveMatrix)
  if(length(mssg$vec)>0){
    vec<- as.numeric(unlist(mssg$vec))
  }
  src<-getCode()
  
  replacementList<-list()
  newPtDefs<-getPtDefs() 
  tibs<-getPtDefs()$tib
  sender='tagDrag.mouse'
  id<-mssg$id
  dxy<-vec 
  tmp<-unlist(str_split(id,"_")) 
  row<-as.numeric(tail(tmp,1)) #this should be the same as selected row index
  selection<-getAssetName() 
  matColIndx<-ncol(newPtDefs$tib[[selection]][[getTibPtColPos()]][[ row  ]])

  # Todo: for inter tib move support
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
  
  
  # Issues: we suppose that we derive context from rowGroupsDB, but
  # rowGroupDB contains
  # - tabId: should be fixed for this page
  # - name: the asset name (tib name)
  # - rows: the indices of the selected rows
  
  # 1. we currently don't know which ptCol in the given row to apply the translation, 
  #    - to fix we could 
  #      - add ptCol to rowGroupDB? (added if we use findeR from a ptCol)
  #      - take first ptCol and apply
  #      - translate all non-empty ptrows
  # 2. the user might corrupt the tibs before we get to the given tib
  
  
  # if we add a colName to rowGroupDB, then what?
  #   must clear rowGroupDB if we change col or asset, unless we change via findR alt+ctrl (kill=F)
  #       observer( col, aname,{ if(kill==T){ } else{ kill=T})
  #   then we loop over entries in rowGroupsDB to reassign pts
  #   Need to prevent double translation of current row, col, asset
  #   so remove from rowGroupsDB and then add back to rowGroupsDB
  
  cntx<-tibble(
    name=selection,
    rows=row,
    colName=getTibColumnName()
  )
  pageId<-getTibTabId()
  if( mssg$shiftKey==TRUE){ 
    if(getTibRow()!=row){
      updateRowPicker(session, "myTibRowCntrl", addToGroup = row, selectRow = row )
    } else {
      updateRowPicker(session, "myTibRowCntrl", toggleGroup = row)
    }
    #updateRowPicker(session, "myTibRowCntrl", addToGroup = c(getTibRow(), row), selectRow = row)
    #updateRowPicker(session, "myTibRowCntrl", addToGroup = row)
    cname<-getTibColumnName()
    cntx1<-filter(rowGroupsDB(), tabId==pageId & !(name==selection & rows==row & colName == cname ))
    cntx1<-select(cntx1,'name','rows','colName')
    cntx<-rbind(cntx1,cntx)
  }  else {
    updateRowPicker(session, "myTibRowCntrl", removeEntireGroup=TRUE)
  }
  contextList<-pmap(cntx, function(name, rows, colName){
    # to check that tib has names
    ctype<-extractColType(tibs[[name]][[colName]])
    if(identical(ctype,'point')){
      columnIndex<-which(names(tibs[[name]])==colName  )
      list(name=name, column=columnIndex, row=rows)
    } else {
      NULL
    }
  })
  contextList<-Filter(function(x){!is.null(x)}, contextList)
  tryCatch({
    matCol<-NULL
    getDxy<-function(){names(dxy)<-c('dx','dy'); dxy}
    ppenv<-list(
      getDxy=getDxy,
      moveMatrix=moveMatrix,
      keys=list(alt=mssg$altKey, shift=mssg$shiftKey, ctrl=mssg$ctrlKey, meta=mssg$metaKey, keycode=mssg$keycode),
      WH=getSVGWH()
    )
    for(ctx in contextList){
      txt<-extractPreProcScript(
        tab_Id=pageId, 
        tib_Name= ctx$name,
        column_Name= names( tibs[[ctx$name]] )[ ctx$column ]  
      )['onMoveMat']
      if(is.null(txt)){
        m<-tibs[[ctx$name]][[ctx$column ]][[ctx$row ]]
        tibs[[ctx$name]][[ctx$column ]][[ctx$row ]]<-m+dxy 
      } else {
        context<-c(ctx, list(tibs=tibs))
        tppenv<-c(ppenv,context)
        tibs<-eval(parse(text=txt), tppenv )
        validateTibLists(getPtDefs()$tib, tibs)
      }
    }
    matCol<-ncol(tibs[[getAssetName()]][[getTibPtColPos()]][[row]] )
    newPtDefs$tib<-tibs
    updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=row, matCol=matCol))
  }, error=function(e){
    e<-c('preproErr',e$message)
    err<-paste(unlist(e), collapse="\n", sep="\n")
    alert(err)
  })
  log.fout(mouseCmdMoveMatrix)
}