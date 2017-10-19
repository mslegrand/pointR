#// called when adding a new point
addPt2ptDefs<-function(newPt, indx, ptDefs, selection){
  #selection<-input$ptRSelect
  #selection<-selectedPoint$name
  #update local ptRList
  #indx<-selectedPoint$point.index

  ptRList<-ptDefs$pts
  ptRList[[selection]]<-append(ptRList[[selection]],newPt,2*indx)

  #df<-NULL
  tagList<-ptDefs$df
  if(!is.null(tagList)){
    df<-tagList[[selection]]
    #if selection has been tagged, and we have a frequency
    # the also update the tags
    if(!is.null(df)){ 
      tags<-df$tag
      freq<-reactiveTag$freq[[selection]]
      if(is.null(freq)){
        tags2move<-which(tags>indx)
        if(length(tags2move)>0){
          tags[tags2move]<-1+ tags[tags2move]
          df$tag<-tags
          tagList[[selection]]<-df
        }
      } else { # freq assigned case: ADD TAG AT END IF NEEDED.
        freq<-as.integer(freq)
        len<-length(ptRList[[selection]])/2
        if( freq==1|| 1==(len %% freq)){
          df2append<-tail(df,1)
          df2append$tag<-len
          df<-rbind(df,df2append)
          tagList[[selection]]<-df
        }
      }
    } 
    # otherwise there is nothing to do for tags
  } # end of tagR handling
  #update point values
  #selectedPoint$point.index<-selectedPoint$point.index+1
  newPtDefs<-list(pts=ptRList, df= tagList )
  #replacementList<-ptDef2ReplacementList(newPtDef, src)
}

observe({ 
  input$mouseMssg #may want to rename this
  isolate({
    if(length(input$mouseMssg)>0){
      #get cmd
      cmd<-input$mouseMssg$cmd
      if(length(input$mouseMssg$vec)>0){
        vec<- unlist(input$mouseMssg$vec)
        cat('cmd=',cmd,"\nvec=c(", paste(vec, collapse=","), ")\n")
      }
      src<-getCode()
      
      replacementList<-list()
      #todo: error check???
      #pt<-eval(parse(text=pt)) 
      #ptRList<-getPtDefs()$pts
      ptDefs<-getPtDefs()
      barName=rightPanel()
      if(barName=="Points"){
        sender='PointsBar.mouse'
        if(cmd=='add'){ #---------add point
          newPt<-vec
          #get selection
          selection<-selectedPoint$name
          #update local ptRList
          indx<-selectedPoint$point.index
          ptDefs<-getPtDefs()
          newPtDefs<-addPt2ptDefs(newPt, indx, ptDefs, selection)
          selectedPoint$point.index<-selectedPoint$point.index+1
          updateAceExtDef(newPtDefs, sender=sender)
        }
        
        if(cmd=='move'){ # --------move point
          id<-input$mouseMssg$id
          vid<-strsplit(id,"-")[[1]] 
          #get selection
          selection<-vid[2]
          #get point index
          indx<-2*as.numeric(vid[3])-1
          #reassign point
          newPtDefs<-ptDefs
          newPtDefs$pts[[selection]][indx:(indx+1)]<-vec
          selectedPoint$point.index<-(indx+1)/2
          updateAceExtDef(newPtDefs, sender=sender)
        }        
      }

      if(barName=='tagDrag'){
        sender='tagDrag.mouse'
        if(cmd=='transGrp'){ # -- move tagged group (from tagDrag)
          tid<-input$mouseMssg$id
          #selectedPoint$point.index<-tid
          cat('transGrp\n')
          print(tid)
          #pt<-input$mouseMssg$pt
          vec<- input$mouseMssg$vec
          dxy<-unlist(vec) #eval(parse(text=tmp))
          print(dxy)
          print(class(dxy))
          # get the tag name, 
          ptName<-getPtName() 
          # get points
          ptRList<-getPtDefs()$pts
          pts<-getPtDefs()$pts[[ptName]] #ptRList[[ptName]]
          tagRList<-getPtDefs()$df
          tag.indx<-getPtIndex() #as.numeric(tagDragInfoList$index() ) #!!! tagIndx2 should be replaced with a safer alternative
          #browser()
          ptTags<-getPtDefs()$df[[ptName]]
          if( !is.null(tag.indx) && !is.null(ptTags)){
            tags<-ptTags$tag
            ti<-which(tag.indx==tags) 
            id.nos<-sequence(ncol(pts))
            # the tag point range
            tagInterval<-findInterval(id.nos,tags)
            tmp<-pts[,tagInterval==ti]
            pts[,tagInterval==ti]<-matrix(tmp+dxy,2)
            ptRList[[ptName]]<-pts
            
            newPtDefs<-list(pts=ptRList, df= tagRList ) 
            updateAceExtDef(newPtDefs, sender=sender)
          }
        }
      }
      
            
      if(barName=='tagValues'){
        sender='tagVal.mouse'
        if(cmd=='tagValSelect'){ # -- move tagged group (from tagDrag)
          tid<-input$mouseMssg$id
          cat(paste0("tid=", tid, "\n"))
          tag.index<-tail(str_split(tid, "_")[[1]],1)
          cat("tag.index=",tag.index,"\n")
          tag.index<-as.integer(tag.index)
          tagIndices<-getTagIndexChoices()
         
          point.index<-tagIndices[tag.index]
          cat(paste0("point.index=", point.index, "\n"))
          selectedPoint$point.index<-point.index
        }
      }
      
      if(barName=='Transforms'){
        #-------transformations of nodes marked as class 'movable'
        sender=paste0(barName, '.mouse')
        if(cmd=='trans'){ # -- translate the object by id
          tid<-input$mouseMssg$id
          #vec<-input$mouseMssg$vec
          #trDefDelta<-formatC(eval(parse(text=tmp)))
          #trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2)" ) 
          trDefDelta2<-paste0("matrix(c(",paste0(vec,collapse=", "), "),2)" ) 
          #src<-tr2src( src, tid, trDefDelta2 ) # !!! REPLACE
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
          #vec<-input$mouseMssg$vec
          #trDefDelta<-formatC(eval(parse(text=tmp)))
          #trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2)" ) 
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
          #vec<-input$mouseMssg$vec
          #trDefDelta<-formatC(eval(parse(text=tmp)))
          #trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2)" ) 
          trDefDelta2<-paste0("matrix(c(",paste0(vec,collapse=", "), "),2)" ) 
          pos<-tid2replacementCoord(tid)
          replacementList<-list(list(rng=pos, txt= trDefDelta2))
          session$sendCustomMessage(
            type = "shinyAceExt",
            list(id= "source", replacement=replacementList, sender = sender, ok=1)
          )
        }
      }
      
      # update internal user source
      #setCode(src) # !!! REPLACE with the below
      #print(str(replacementList))
      
      # if( length(replacementList)>0 ){
      #   session$sendCustomMessage(
      #     type = "shinyAceExt",
      #     list(id= "source", replacement=replacementList, sender='mouse.ptr', ok=1)
      #   )
      # }
      #triggerRefresh('ptr.mouse',1)
      #request$refresh=runif(1, min = 1, max = 2) # trigger refresh
      # update current editor
      
    }
  })
})
