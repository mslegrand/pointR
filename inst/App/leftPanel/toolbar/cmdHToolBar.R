

observeEvent( input$tbUndo ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", undo=TRUE)
  )
}, ignoreInit = TRUE)

observeEvent( input$tbRedo,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", redo=TRUE)
  )
}, ignoreInit = TRUE)

observeEvent( input$tbCollapse ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", foldAll=TRUE)
  )
}, ignoreInit = TRUE)

observeEvent( input$tbExpand ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", unfoldAll=TRUE)
  )
}, ignoreInit = TRUE)

observeEvent( input$tbNextBookMark ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", nextBookMark=TRUE)
  )
}, ignoreInit = TRUE)

observeEvent( input$tbPreviousBookMark ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", previousBookMark=TRUE)
  )
}, ignoreInit = TRUE)




observeEvent( input$tbPrint ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", print=TRUE)
  )
}, ignoreInit = TRUE)


observeEvent( input$tbIndentRight ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", indentSelection=TRUE)
  )
}, ignoreInit = TRUE)

observeEvent( input$tbIndentLeft ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", outdentSelection=TRUE)
  )
}, ignoreInit = TRUE)

observeEvent( input$tbToggleComment,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", toggleComment=TRUE)
  )
}, ignoreInit = TRUE)

observeEvent( input$tbFindAndBookMark ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", findAndBookMark=TRUE)
  )
}, ignoreInit = TRUE)



observeEvent( input$tbDeleteAllBookMarks ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", tbDeleteAllBookMarks=TRUE)
  )
}, ignoreInit = TRUE)



observeEvent( input$tbFindAndBookMark ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", findAndBookMark=TRUE)
  )
}, ignoreInit = TRUE)

observeEvent( input$tbFindNext ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", findNext=TRUE)
  )
}, ignoreInit = TRUE)

observeEvent( input$tbFindPrevious ,{
  session$sendCustomMessage(
    type = "shinyAceExt",    
    list(id= "source", findPrevious=TRUE)
  )
}, ignoreInit = TRUE)
