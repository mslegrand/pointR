# template for point insertion precprossing

dxy=getDxy() # pt contains coordinates derived from the mouse move

#apply your point manipulations here

tibs<-moveMatrix(dxy=dxy, context=context )
tibs # always return tibs
