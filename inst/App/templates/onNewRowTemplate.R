# template for value precprossing on row change

value<-getAttrValue() # current  value

# apply your value manipulations here

tibs<-setAttrValue(value=value, context=context )
tibs # always return tibs
