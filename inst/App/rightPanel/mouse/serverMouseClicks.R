


observeEvent(input$mouseMssg, { #may want to rename this: input$mouseMssg
    if(length(input$mouseMssg)>0){
      mssg<-input$mouseMssg
      cmd<-mssg$cmd
      vec<-mssg$vec
    }
    if( !is.null(cmd) && !is.null(vec)){
      panelName=getRightMidPanel()
      if(panelName=='point'){
        if(cmd=='add'){ #---------add point
          mouseCmdAddPt(mssg)
        }#------end---add point
        if(cmd=='move'){ # --------move point
          mouseCmdMovePt(mssg)
        }    
      }
      if(panelName=='matrix'){
        if(cmd=='transGrp'){ # -- move tagged group (from tagDrag)
          mouseCmdMoveMatrix(mssg)
        }
      }
      if(panelName=='value'){
        if(cmd=='tagValSelect'){ # -- move tagged group (from tagDrag)
          mouseCmdValue(mssg)
        }
      }
      if(panelName==transformTag){
        #-------transformations of nodes marked as class 'movable' (or 'transFormR' or 'dragR' )
        if( cmd %in% c('trans','rotate','scale')){
          mouseCmdTransform(mssg)
        }
      }      
    } # endof if(!is.null(cmd) && !is.null(vec))
})
