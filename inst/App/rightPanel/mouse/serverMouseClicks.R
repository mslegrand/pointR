#// called when adding a new point

addPt2ptDefs<-function(name, row, matCol,  ptDefs, newPt){
  
  if(is.numeric(row) && 
     is.numeric(matCol)  &&
     row>0 && 
     length(ptDefs$tib)>0 && 
     length(ptDefs$tib[[name]])>0 && 
     row<=nrow(ptDefs$tib[[name]])
     ){
    tib<-ptDefs$tib[[name]]
    col<-getTibPtColPos() #which(names(tib)==ptColName)
    pts<-tib[[row,col]] 
    pts<-append(pts,newPt,2*(matCol))
    tib[[row,col]]<-matrix(pts,2)
    ptDefs$tib[[name]]<-tib
  } else {
    ptDefs<-NULL #failed
    #cat("addPt2ptDefs returning NULL")
  }
  ptDefs
}


observe({ 
  input$mouseMssg #may want to rename this
  isolate({
    if(length(input$mouseMssg)>0){
      mssg<-input$mouseMssg
      #get cmd
      cmd<-mssg$cmd
      
      if(length(mssg$vec)>0){
        vec<- as.numeric(unlist(mssg$vec))
      }
      src<-getCode()
      replacementList<-list()
      ptDefs<-getPtDefs() 
      panelName=getRightMidPanel()
      if(panelName=="Points" || (panelName=='point' ) ){
        sender='PointsBar.mouse'
        if(cmd=='add'){ #---------add point
          mouseCmdAddPt(mssg)
        }#------end---add point
        
        if(cmd=='move'){ # --------move point
          cat('Enter: mouse cmd move')
          sender='PointsBar.mouse.move'
          id<-mssg$id
          newPtDefs<-ptDefs
          vid<-strsplit(id,"-")[[1]] 
          #get selection
          selection<-vid[2]
          #get point index
          newPt<-vec
          rowIndex<-as.numeric(vid[3]) # index is the absolute position of in the points array
          matColIndx<-as.numeric(vid[4])
          if( hasPtScript() ){
            cat('hasPtScript:: onMovePt script:\n')
            txt<-getPreProcPtScript()['onMovePt']
            cat(txt)
           tryCatch({ 
              getPoint<-function(){newPt}
              getLocation<-function(){
                list(
                  assetName=getAssetName(),
                  columIndex=getTibPtColPos(),
                  rowIndex=rowIndex,
                  matColIndex=matColIndx,
                  tibs=getPtDefs()$tib
                )
              }
              tibs<-eval(parse(text=txt))
              newPtDefs$tib<-tibs
            },error=function(e){
              e<-c('preproErr',unlist(e))
              err<-paste(unlist(e), collapse="\n", sep="\n")
              setErrorMssg(err)
            })
          } else {
            newPtDefs$tib[[selection]][[ rowIndex, getTibPtColPos() ]][,matColIndx]<-newPt
          }

          updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matColIndx))
          
        }        
      

      if(panelName=='matrix'){
        sender='tagDrag.mouse'
        if(cmd=='transGrp'){ # -- move tagged group (from tagDrag)
          
          tid<-mssg$id
          dxy<-vec 
          tmp<-unlist(str_split(tid,"_"))
          row<-as.numeric(tail(tmp,1))
          
          selection<-getAssetName() 
          m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
          ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]<-m+vec
          matCol<-ncol(m)
          newPtDefs<-ptDefs
          updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=row, matCol=matCol))
        }
      }
      
            
      if(panelName=='value'){
        sender='tagVal.mouse'
        if(cmd=='tagValSelect'){ # -- move tagged group (from tagDrag)
          sender='tagDrag.mouse'
          tid<-mssg$id
          tmp<-unlist(str_split(tid,"_"))
          row<-as.numeric(tail(tmp,1))
          selection<-getAssetName() 
          m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
          matCol<-ncol(m)
        
          updateAceExtDef(ptDefs, sender=sender, selector=list( rowIndex=row, matCol=matCol))

        }
      }
      
      if(panelName==transformTag){
        #-------transformations of nodes marked as class 'movable' (or 'transFormR' or 'dragR' )
        sender=paste0(panelName, '.mouse')
        if(cmd=='trans'){ # -- translate the object by id
          tid<-mssg$id
          trDefDelta2<-paste0("matrix(c(",paste0(vec,collapse=", "), "),2)" ) 
          pos<-tid2replacementCoord(tid)
          replacementList<-list(list(rng=pos, txt= trDefDelta2))
          updateAceExt(id= getAceEditorId(), replacement=replacementList, sender = sender, ok=1 )
        }
        
        #-------transformations of nodes marked as class 'movable'
        if(cmd=='rotate'){ # ----rotate
          tid<-mssg$id
          vec<-mssg$vec
          trDefDelta2<-paste0("matrix(c(",paste0(vec,collapse=", "), "),2)" ) 
          pos<-tid2replacementCoord(tid)
          replacementList<-list(list(rng=pos, txt= trDefDelta2))
          updateAceExt(id= getAceEditorId(), replacement=replacementList, sender = sender, ok=1 )
        } 
        
        #-------transformations of nodes marked as class 'movable'
        if(cmd=='scale'){ # ----scale
          tid<-mssg$id
          vec<-mssg$vec
          trDefDelta2<-paste0("matrix(c(",paste0(vec,collapse=", "), "),2)" ) 
          pos<-tid2replacementCoord(tid)
          replacementList<-list(list(rng=pos, txt= trDefDelta2))
          updateAceExt(id= getAceEditorId(), replacement=replacementList, sender = sender, ok=1 )
        }
      }
      }
    } #end of mouseMssg if
  })
})
