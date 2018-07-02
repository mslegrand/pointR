
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
    div( id='dogbert', class='backPanel',
         div( style="margin:10px",
               h5(title, style='color:white;'),
              radioGroupButtons(
                inputId = "dilbert",
                label = "Action",
                choices = c("onNewPt",  "onMovePt", "onDeletePt", "onSplitAfterPt"),
                selected=selected
              )
         ),

      aceEditor(
            outputId='catberg',
            height = "300px",
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


# insertEDinPP<-function(id){
#   cat("acePointPreprocId=",acePointPreprocId,"\n")
#   acePointPreprocId=paste0("ACE", id)
#   ui=aceEditor(
#     outputId=acePointPreprocId, 
#     height = "300px", 
#     value = text
#   )
#   insertUI(
#     selector='#dogbert',
#     where='beforeEnd',
#     ui=ui
#   )
# }