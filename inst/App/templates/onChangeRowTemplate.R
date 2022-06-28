# template for value precprossing on row change

value<-getAttrValue() # by value contains value as entered

# apply your value manipulations here

tibs<-setAttrValue(value=value, context=context )
tibs # always return tibs

