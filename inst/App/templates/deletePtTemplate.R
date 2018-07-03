
tibs<-deletePt(location=getLocation(), tibs=getTibs() )

# onDeletePoint<-function(tibs, tibName, tibColumn, rowIndex, ptColIndex){
#   m<-tibs[[tibName]][[tibColumn]][[rowIndex]]
#   m<-deletePt(m, ptColIndex) #internal convenience function
#   tibs[[tibName]][[tibColumn]][[rowIndex]]<-m
#   tibs # 
# }