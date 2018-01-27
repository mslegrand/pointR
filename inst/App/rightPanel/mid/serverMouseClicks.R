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
      #get cmd
      cmd<-input$mouseMssg$cmd
      
      if(length(input$mouseMssg$vec)>0){
        vec<- as.numeric(unlist(input$mouseMssg$vec))
      }
      src<-getCode()
      replacementList<-list()
      ptDefs<-getPtDefs() 
      barName=getRightMidPanel2()
      if(barName=="Points" || (barName=='point' ) ){
        sender='PointsBar.mouse'
        if(cmd=='add'){ #---------add point
          sender='PointsBar.mouse.add'
          #cat('Enter: mouse cmd add')
          newPt<-vec
          #get selection
          
          selection<-getTibName() 
          rowIndex<-row<-getTibRow()
          matColIndx<-getTibMatCol()

          if(is.null(matColIndx)){
            cat('matColIndx is null\n') #should never happen???
          } else {
            newPtDefs<-addPt2ptDefs(
              getTibName(),
              rowIndex,
              matColIndx,
              ptDefs, 
              newPt 
            )
            if(!is.null(newPtDefs)){ #update only upon success
                updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matColIndx+1))
            }
          }
         }
        
        if(cmd=='move'){ # --------move point
          #cat('Enter: mouse cmd move')
          sender='PointsBar.mouse.move'
          id<-input$mouseMssg$id
          vid<-strsplit(id,"-")[[1]] 
          #get selection
          selection<-vid[2]
          #get point index
          #indx<-as.numeric(vid[3]) # index is the absolute position of in the points array
          newPt<-vec
          rowIndex<-as.numeric(vid[3])
          matColIndx<-as.numeric(vid[4])
          ptDefs$tib[[selection]][[ rowIndex, getTibPtColPos() ]][,matColIndx]<-newPt
          newPtDefs<-ptDefs
          updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=rowIndex, matCol=matColIndx))
          
        }        
      }

      if(barName=='matrix'){
        sender='tagDrag.mouse'
        if(cmd=='transGrp'){ # -- move tagged group (from tagDrag)
          
          tid<-input$mouseMssg$id
          dxy<-vec 
          tmp<-unlist(str_split(tid,"_"))
          row<-as.numeric(tail(tmp,1))
          
          selection<-getTibName() 
          m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
          ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]<-m+vec
          matCol<-ncol(m)
          newPtDefs<-ptDefs
          updateAceExtDef(newPtDefs, sender=sender, selector=list( rowIndex=row, matCol=matCol))
        }
      }
      
            
      if(barName=='value'){
        sender='tagVal.mouse'
        if(cmd=='tagValSelect'){ # -- move tagged group (from tagDrag)
          sender='tagDrag.mouse'
          tid<-input$mouseMssg$id
          tmp<-unlist(str_split(tid,"_"))
          row<-as.numeric(tail(tmp,1))
          selection<-getTibName() 
          m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
          matCol<-ncol(m)
        
          updateAceExtDef(ptDefs, sender=sender, selector=list( rowIndex=row, matCol=matCol))

        }
      }
      
      if(barName=='transform'){
        #-------transformations of nodes marked as class 'movable' (or 'transFormR' or 'dragR' )
        sender=paste0(barName, '.mouse')
        if(cmd=='trans'){ # -- translate the object by id
          tid<-input$mouseMssg$id
          trDefDelta2<-paste0("matrix(c(",paste0(vec,collapse=", "), "),2)" ) 
          pos<-tid2replacementCoord(tid)
          replacementList<-list(list(rng=pos, txt= trDefDelta2))
          session$sendCustomMessage(
            type = "shinyAceExt",
            list(id= "source", replacement=replacementList, sender=sender, ok=1)
          )
        }
        
        #-------transformations of nodes marked as class 'movable'
        if(cmd=='rotate'){ # ----rotate
          tid<-input$mouseMssg$id
          vec<-input$mouseMssg$vec
          trDefDelta2<-paste0("matrix(c(",paste0(vec,collapse=", "), "),2)" ) 
          pos<-tid2replacementCoord(tid)
          replacementList<-list(list(rng=pos, txt= trDefDelta2))
          session$sendCustomMessage(
            type = "shinyAceExt",
            list(id= "source", replacement=replacementList, sender=sender, ok=1)
          )
        } 
        
        #-------transformations of nodes marked as class 'movable'
        if(cmd=='scale'){ # ----scale
          tid<-input$mouseMssg$id
          vec<-input$mouseMssg$vec
          trDefDelta2<-paste0("matrix(c(",paste0(vec,collapse=", "), "),2)" ) 
          pos<-tid2replacementCoord(tid)
          replacementList<-list(list(rng=pos, txt= trDefDelta2))
          session$sendCustomMessage(
            type = "shinyAceExt",
            list(id= "source", replacement=replacementList, sender = sender, ok=1)
          )
        }
      }
      
    }
  })
})
