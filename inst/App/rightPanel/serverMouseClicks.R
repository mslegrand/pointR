#// called when adding a new point







addPt2ptDefs<-function(name, row, matCol, point.indx, ptDefs, newPt){
  
  if(is.numeric(row) && is.numeric(matCol) && is.numeric(point.indx) &&
     row>0 && length(ptDefs$tib)>0 && length(ptDefs$tib[[name]])>0 
     && row<=nrow(ptDefs$tib[[name]])
     ){
    
    tib<-ptDefs$tib[[name]]
    col<-getTibPtColPos() #which(names(tib)==ptColName)
    pts<-tib[[row,col]] 
    pts<-append(pts,newPt,2*(matCol))
    tib[[row,col]]<-matrix(pts,2)
    ptDefs$tib[[name]]<-tib
    #updateSelected(row=row, matCol=matCol+1, point.index=point.indx+1 )
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
      # isolate({
      #   cat("input$mouseMssg\n")
      #   print(input$mouseMssg)
      #   cat("\n")        
      # })
      if(length(input$mouseMssg$vec)>0){
        vec<- as.numeric(unlist(input$mouseMssg$vec))
      }
      src<-getCode()
      
      replacementList<-list()
      
      ptDefs<-getPtDefs() #!!!  to do: replace with getTib
  
      barName=rightPanel()
      if(barName=="Points"){
        sender='PointsBar.mouse'
        if(cmd=='add'){ #---------add point
          sender='PointsBar.mouse.add'
          #cat('Enter: mouse cmd add')
          newPt<-vec
          #get selection
          
          selection<-getTibName() #selectedPoint$name
          #update local ptRList
          #point.indx<-getPtIndex() #selectedPoint$point.index
          #cat('cmd add: getPtIndx()=', point.indx,'\n')
          #rc<-absPtIndx2TibPtPos(point.indx)
          
          rowIndex<-row<-getTibRow()
          
          matColIndx<-getTibMatCol()
          #rc<-list(row=rowIndex, matCol= matColIndx)
          # indx<-tibPtPos2AbsPtIndx()(rowIndex, matColIndx)
          point.indx<-0  # TODO!!! remove this
          
          if(is.null(matColIndx)){
            cat('matColIndx is null\n') #should never happen???
          } else {
            newPtDefs<-addPt2ptDefs(
            getTibName(),
            rowIndex,
            matColIndx,
            point.indx, # TODO!!! remove this
            ptDefs, 
            newPt 
          )
          
          if(!is.null(newPtDefs)){ #update only upon success
             #selectedTibble$point.index<-selectedTibble$point.index+1
              updateAceExtDef(newPtDefs, sender=sender)
              #updateSelected(point.index=indx+1)
              updateSelected(row=rowIndex, matCol=matColIndx+1 )
              #cat('mouse add:: updateSelected')
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
          #m[,rindx]<-newPt # m is the matrix of points in the given row
          
           
           #reassign point
 
          #selectedPoint$point.index<-(indx+1)/2
          
          
          #rc<-absPtIndx2TibPtPos(indx)
          rowIndex<-as.numeric(vid[3])
          matColIndx<-as.numeric(vid[4])
          rc<-list(row=rowIndex, matCol= matColIndx)
          indx<-tibPtPos2AbsPtIndx()(rowIndex, matColIndx)
          
          
          cat('point.index=',indx,'\n')
          ptDefs$tib[[selection]][[ rc$row, getTibPtColPos() ]][,rc$matCol]<-newPt
          newPtDefs<-ptDefs
          #selectedTibble$point.index<-rc$matColPos
          #selectedTibble$row<-rc$row
          # selectedTibble$row<-rc$row
          # selectedTibble$col<-rc$col
          updateAceExtDef(newPtDefs, sender=sender)
          updateSelected(point.index=indx, row=rc$row, matCol=rc$matCol)
        }        
      }

      if(barName=='tagDrag'){
        sender='tagDrag.mouse'
        if(cmd=='transGrp'){ # -- move tagged group (from tagDrag)
          
          tid<-input$mouseMssg$id
          #vec<- as.numeric(input$mouseMssg$vec)
          dxy<-vec #eval(parse(text=tmp))
          tmp<-unlist(str_split(tid,"_"))
          row<-as.numeric(tail(tmp,1))
          
          selection<-getTibName() #selectedPoint$name
          #rc<-absPtIndx2TibPtPos(indx)
          m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
          ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]<-m+vec
          matCol<-ncol(m)
          pts<-ptDefs$tib[[selection]][[getTibPtColPos()]]
          point.index<-ptPos2AbsPtIndx(pts,row, matCol)
          newPtDefs<-ptDefs
          #selectedTibble$point.index<-rc$matColPos
          #selectedTibble$row<-rc$row
          
          updateAceExtDef(newPtDefs, sender=sender)
          updateSelected(row=row, matCol=matCol, point.index=point.index)
        }
      }
      
            
      if(barName=='tagValues'){
        sender='tagVal.mouse'
        if(cmd=='tagValSelect'){ # -- move tagged group (from tagDrag)
          tid<-input$mouseMssg$id
          tmp<-unlist(str_split(tid,"_"))
          row<-as.numeric(tail(tmp,1))
          selection<-getTibName() #selectedPoint$name
          m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
          matCol<-ncol(m)
          m<-ptDefs$tib[[selection]][[ row, getTibPtColPos() ]]
          pts<-ptDefs$tib[[selection]][[getTibPtColPos()]]
          point.index<-ptPos2AbsPtIndx(pts,row, matCol)
          
          updateSelected(row=row, matCol=matCol, point.index=point.index)
        }
      }
      
      if(barName=='Transforms'){
        #-------transformations of nodes marked as class 'movable'
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
