# template for point insertion precprossing

pt<-getPoint() # pt contains coordinates derived from the mouse move

#apply your point manipulations here

tibs<-movePoint(pt=pt, location=getLocation() )
tibs # always return tibs
