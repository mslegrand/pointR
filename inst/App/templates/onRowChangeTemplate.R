# template for value precprossing on row change

value<-getAttrValue() # pt contains coordinates derived from the mouse click

# apply your point manipulations here

tibs<-setAttrValue(value=value, context=context )
tibs # always return tibs
