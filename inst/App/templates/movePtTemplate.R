
onMovePoint<-function(tibs, tibName, tibColumn, pt, rowIndex, ptColIndex){
  m<-tibs[[tibName]][[tibColumn]][[rowIndex]]
  m<-movePt(m, pt, ptColIndex) #internal convenience function
  tibs[[tibName]][[tibColumn]][[rowIndex]]<-m
  tibs # 
}