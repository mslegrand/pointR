preprocPts<-list(
  onNewPt = function( pt, context, WH, keys){
   # template for point insertion precprossing
  
  pt<-getPoint() # pt contains coordinates derived from the mouse click
  
  # apply your point manipulations here
  pt<-50*round(pt/50)
  tibs<-insertPoint(pt=pt, context=context )
  tibs # always return tibs 
  },
  onMovePt = function( pt, context, WH, keys){
   # template for point insertion precprossing
  
  pt<-getPoint() # pt contains coordinates derived from the mouse move
  
  #apply your point manipulations here
  pt<-50*round(pt/50)
  tibs<-movePoint(pt=pt, context=context )
  tibs # always return tibs 
  },
  onMoveMat = function( pt, context, WH, keys){
   # template for point insertion precprossing
  
  dxy=getDxy() # pt contains coordinates derived from the mouse move
  
  #apply your point manipulations here
  dxy<-50*round(dxy/50)
  tibs<-moveMatrix(dxy=dxy, context=context )
  tibs # always return tibs 
  }
)
