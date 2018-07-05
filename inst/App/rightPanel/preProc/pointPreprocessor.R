
newPointPreprocessor<-function(
     id='BadWolf',  
     title='Point Preprocessor',
     PPPCode = list(
       onNewPt='newPt',
       onMovePt= 'movePt',
       onDeletePt= 'deletePt'
     ),
     selected="onNewPt"
  )
{
  dropdownId= paste0("PtPreProc-",id)
  #is actually paste0('sw-dropdown-',dropdownId)
  cat("dropdownId=",dropdownId,"\n")
  div(
    dropdown( inputId = dropdownId ,
    div( id='ptPreProcBackPanel', class='backPanel',
         div( style="margin:10px; color:white;", 
              div( style="margin:10px",
               h5(title, style='color:white;'),
              radioGroupButtons(
                inputId = "ptPreProcCmdChoice",
                label = "Action",
                choices = c("onNewPt",  "onMovePt", "onDeletePt"),
                selected=selected
              )
            )
         ),

      aceEditor(
            outputId='ptPreProcAceEditor',
            height = "300px",
            mode='r',
            value = PPPCode[[selected]]
          ),
      div( style="margin:10px",
           actionButton("commitPtPreProc", "Commit")
      )
    ),
   
    icon=icon("filter", lib = "glyphicon"), #icon=icon("wrench", lib = "glyphicon"), #icon = icon("toolbox"),
    status = "primary", width = "400px", size='sm',
    up=TRUE, right=FALSE,
    animate = animateOptions(
      enter = animations$fading_entrances$fadeInLeftBig,
      exit = animations$fading_exits$fadeOutRightBig
    )
  )
  )
}

