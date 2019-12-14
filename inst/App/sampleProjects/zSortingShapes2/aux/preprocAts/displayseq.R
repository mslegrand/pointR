preprocPts<-list(
  onNewRow = function( pt, context, WH, keys){
   
     
       
         # template for value precprossing on row change
        
        value<-getAttrValue() # current  value
        
        # apply your value manipulations here
        value<-paste0(sample(letters, 8, replace=TRUE), collapse="")
        tib<-context$tibs[['displayseq']]
        row<-getLastRow(tib)
        row$id<-value
        tib<-appendLastRow(tib,row)
        context$tibs[['displayseq']]<-tib
        tibs<-setAttrValue(value=value, context=context )
        tibs # always return tibs 
         
       
     
  },
  onChangeRow = function( pt, context, WH, keys){
   
     
       
         # template for value precprossing on row change
        
        value<-getAttrValue() # pt contains coordinates derived from the mouse click
        
       
        tibs<-setAttrValue(value=value, context=context )
        
        
        tibs # always return tibs 
         
       
     
  }
)
