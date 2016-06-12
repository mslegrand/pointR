
# -----------ACTIVE POINT MATRIX------------------------
#  observes code and plotNavBar
#  sets active Point, point selection,  and selectedPoint$point.index

observe({
  user$code
  plotMode<-input$plotNavBar
  isolate({
    point.index<-selectedPoint$point.index
    selected<-input$ptRSelect
    ptRList<-getPtDefs()$pts
    res<-ex.getSelectInfo(ptRList, selected, point.index)
    selectedPoint$point.index<-res$point.index
    updateSelectInput(session, "ptRSelect",
                      choices=names(ptRList),
                      selected= res$selected )
  })
})



observe({
  input$ptRSelect
  isolate({
    ptRList<-getPtDefs()$pts
    selectedPoint$point.index<-length(ptRList[[input$ptRSelect]])
    selected=reactiveTag$freq[[input$ptRSelect]] 
    if(is.null(selected)){
      selected<-"Off"
    }
    updateSelectInput(session, "tagFreq", selected=selected )
  })
})


