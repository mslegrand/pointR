preprocPts<-list(
  onNewRow = function( pt, context, WH, keys){
   
     
       
        # template for value precprossing on new row
        
        value<-getAttrValue() # current  value
        
        # apply your value manipulations here
        value<-paste0(sample(letters, 8, replace=TRUE), collapse="")
        
        tibs<-setAttrValue(value=value, context=context )
        tibs # always return tibs 
         
       
     
  },
  onChangeRow = function( pt, context, WH, keys){
   
     
       
         # template for value precprossing on row change
        
        value<-getAttrValue() # pt contains coordinates derived from the mouse click
        
        # apply your point manipulations here
        if(!is.null(keys$keycode)){
            
            tib<-context$tibs[['links']]
            row<-getLastRow(tib)
            if(keys$keycode==65){ # 65 is keycode for 'a'
                row$fromId<-value
                tib<-appendLastRow(tib,row)
            } else if(keys$keycode==66){  # 65 is keycode for 'b'
                row$toId<-value
                tib<-replaceLastRow(tib, row)
            }
            tibs[['links']]<-tib
            
        } else{
            tibs<-setAttrValue(value=value, context=context )
        }
        
        tibs # always return tibs 
         
       
     
  }
)
