
onSplitAt<-function(tibs, tibName, tibColumn, rowIndex, ptColIndex){ #not ready!
  m<-tibs[[tibName]][[tibColumn]][[rowIndex]]
  m<-splitAt(m, ptColIndex) #internal convenience function
  tibs[[tibName]][[tibColumn]][[rowIndex]]<-m
  tibs # 
}