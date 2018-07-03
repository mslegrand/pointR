
pt<-getPt() # coordinates derived from the mouse click
tibs<-insertPt(pt=pt, location=getLocation(), tibs=getTibs() )
#tibs assigned for insertion

# onNewPoint<-function(tibs, tibName, tibColumn, pt, rowIndex, ptColIndex){
#   m<-tibs[[tibName]][[tibColumn]][[rowIndex]]
#   m<-insertPt(m, pt, ptColIndex) #internal convenience function
#   tibs[[tibName]][[tibColumn]][[rowIndex]]<-m
#   tibs # 
# }
# 
