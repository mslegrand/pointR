observe({ 
  input$mouseMssg #may want to rename this
  isolate({
    if(length(input$mouseMssg)>0){
      #get cmd
      cmd<-input$mouseMssg[1]
      #tmp<-paste(input$mouseMssg,collapse="\n** ")
      #cat( file=stderr(), paste("mouseMssg: cmd=",tmp,"\n>\n\n")  ) 
      pt<- input$mouseMssg[2]
      src<-user$code
      #todo: error check???
      
      pt<-eval(parse(text=pt)) 
      ptRList<-getPtDefs()$pts
      if(cmd=='add'){ #---------add point
        newPt<-pt
        #get selection
        #selection<-input$ptRSelect
        selection<-selectedPoint$name
        #update local ptRList
        indx<-selectedPoint$point.index
        ptRList[[selection]]<-append(ptRList[[selection]],newPt,2*indx)
        df<-NULL
        tagList<-getPtDefs()$df
        if(!is.null(tagList)){
          df<-tagList[[selection]]
          if(!is.null(df)){
            tags<-df$tag
            # locate the position of the new point
            #wrt the tags
            tags2move<-which(tags>indx)
            if(length(tags2move)>0){
              tags[tags2move]<-1+ tags[tags2move]
              df$tag<-tags
              tagList[[selection]]<-df
              src<-df2Source(src,tagList)
            } else { #we are at the end
              freq<-reactiveTag$freq[[selection]]
              if(!is.null(freq)) {
                freq<-as.integer(freq)
                offset<-1+indx-tail(tags,1)
                if(offset==freq){ # at the end and needs to be tagged
                  df2append<-tail(df,1)
                  df2append$tag<-1+indx
                  df<-rbind(df,df2append)
                  tagList[[selection]]<-df
                  src<-df2Source(src,tagList)
                }
              }
            }
          }
        }
        #update point values
        selectedPoint$point.index<-selectedPoint$point.index+1
        src<-pts2Source(src,ptRList)
      } 
      if(cmd=='move'){ # --------move point
        id<-input$mouseMssg[3]
        vid<-strsplit(id,"-")[[1]] 
        #get selection
        selection<-vid[2]
        #get point index
        indx<-2*as.numeric(vid[3])-1
        #reassign point
        ptRList[[selection]][indx:(indx+1)]<-pt
        #update point values
        src<-pts2Source(src,ptRList)
        selectedPoint$point.index<-(indx+1)/2
      }
      if(cmd=='transGrp'){
        tid<-input$mouseMssg[3]
        tmp<-input$mouseMssg[2]
        dxy<-eval(parse(text=tmp))
        # get the tag name, 
        #ptName<-selectedPoint$name
        ptName<-getPtName()
        #ptName<-input$ptRSelect
        # get points
        pts<-getPtDefs()$pts[[ptName]] #ptRList[[ptName]]
        tagRList<-getPtDefs()$df
        #as.numeric(input$tagIndx2)
        tag.indx<-getPtIndex() #as.numeric(tagDragInfoList$index() ) #!!! tagIndx2 should be replaced with a safer alternative
        
        ptTags<-tagRList[[ptName]]
        if(!is.null(tagRList)){
          ptTags<-tagRList[[ptName]]
        } else {
          ptTags<-NULL
        }
        if( !is.null(tag.indx) && !is.null(ptTags)){
          tags<-ptTags$tag
          ti<-which(tag.indx==tags) 
          id.nos<-sequence(ncol(pts))
        # the tag point range
          tagInterval<-findInterval(id.nos,tags)
          tmp1<-pts[,tagInterval==ti][1,]+dxy[1]
          tmp2<-pts[,tagInterval==ti][2,]+dxy[2]
          pts[,tagInterval==ti]<-rbind(tmp1,tmp2)
          ptRList[[ptName]]<-pts
          src<-pts2Source(src,ptRList)
        # add to the corresponding points cxy
        # update the source
        }
      }
      #-------transformations 
      if(cmd=='trans'){ # -- translate
        tid<-input$mouseMssg[3]
        tmp<-input$mouseMssg[2]
        trDefDelta<-formatC(eval(parse(text=tmp)))
        trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2,)" ) 
        src<-tr2src( src, tid, trDefDelta2 )
      }
      if(cmd=='rotate'){ # ----rotate
        tid<-input$mouseMssg[3]
        tmp<-input$mouseMssg[2]
        trDefDelta<-formatC(eval(parse(text=tmp)))
        trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2,)" ) 
        src<-tr2src( src, tid, trDefDelta2 )
      } 
      if(cmd=='scale'){ # ----scale
        tid<-input$mouseMssg[3]
        tmp<-input$mouseMssg[2]
        trDefDelta<-formatC(eval(parse(text=tmp)))
        trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2,)" ) 
        src<-tr2src( src, tid, trDefDelta2 )
      } 
      # update internal user source
      user$code<-src
      
      
    }
  })
})
