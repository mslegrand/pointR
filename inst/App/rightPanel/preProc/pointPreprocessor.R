
newPointPreprocessor<-function(
     id='BadWolf',  
     title='Point Preprocessor',
     PPPCode = list(
       onNewPt='newPt',
       onMovePt= 'movePt',
       onMoveMat= 'onMoveMat'
     ),
     selected="onNewPt"
  )
{
  dropdownId= paste0("PtPreProc-",id)
  #is actually paste0('sw-dropdown-',dropdownId)
  #cat("dropdownId=",dropdownId,"\n")
  absolutePanel( id='PtPreProcDiv', left=5, bottom=5, 
    dropdown( inputId = dropdownId , 
    div( id='ptPreProcBackPanel', class='backPanel',
         div( style="margin:10px; color:white;", 
              div( style="margin:10px",
               h5(title, style='color:white;'),
              radioGroupButtons(
                inputId = "ptPreProcCmdChoice",
                label = "Action",
                choices = c("onNewPt",  "onMovePt", "onMoveMat"),
                selected='onNewPt'
              )
            )
         ),
      aceEditor(
            outputId='ptPreProcAceEditor',
            height = "300px",
            mode='r',
            value='on new' #fileTemplates[['newPtTemplate.R']]
          ),
      div( style="margin:10px",
           #actionButton("commitPtPreProc", "Commit"),
           span(id= "commitPtPreProcRequest", 'Commit', class="btn" )
           
           #actionBttn("commitPtPreProc", label = "Commit")
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

