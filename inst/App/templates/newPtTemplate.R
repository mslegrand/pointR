# template for point insertion precprossing

pt<-getPoint() # pt contains coordinates derived from the mouse click

# apply your point manipulations here

tibs<-insertPoint(pt=pt, location=getLocation() )
tibs # always return tibs
