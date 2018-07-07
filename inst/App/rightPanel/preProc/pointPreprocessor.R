
newPointPreprocessor<-function(
     id='BadWolf',  
     title='Point Preprocessor'
  )
{
  dropdownId= paste0("PtPreProc-",id)
  absolutePanel( id='PtPreProcDiv', left=5, bottom=5, 
    dropdown( inputId = dropdownId , 
    div( id='ptPreProcBackPanel', class='backPanel',
         div( style="margin:10px; color:white;", 
              div( style="margin:10px",
               h5(title, style='color:white;'),
               div(actionBttn('tmp','tmp'), style="display:none;"),
               uiOutput("uiPreProcChooser")
               
              #  ,
              # radioGroupButtons(
              #   inputId = "ptPreProcCmdChoice",
              #   label = "Action",
              #   choices = c(preprocChoices,'.') #,
              #   #selected='onNewPt'
              # )
              # ,
              # div(class='btn-group-container-sw btn-group',
              #   div( class='btn-group', role='group',
              #        span ( class='btn btn-default active', id='but1', 'onNewPt')
              #   ),
              #   div( class='btn-group', role='group',
              #         span ( class='btn', id='but2', 'onMovePt')
              #   ),
              #   div( class='btn-group', role='group',
              #         span ( class='btn', id='but3', 'onMoveMat')
              #   )
              # )
            )
         ),
      aceEditor(
            outputId='ptPreProcAceEditor',
            height = "300px",
            mode='r',
            value='on new' #fileTemplates[['newPtTemplate.R']]
          ),
      div( style="margin:10px",
           span(id= "commitPtPreProcRequest", 'Commit', class="btn" )
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

