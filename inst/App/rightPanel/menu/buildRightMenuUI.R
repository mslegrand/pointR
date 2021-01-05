checkBoxInput4Menu<-function (inputId, label, value = FALSE, width = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "checkbox")
  if (!is.null(value) && value) 
    inputTag$attribs$checked <- "checked"
  div(class = "form-group shiny-input-container", style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), "; margin-bottom:0;"), div(class = "checkbox", 
                                                                          tags$label(inputTag, tags$span(label))))
}

buildRightMenu<-function(){
  dmdMenuBarPage(
    menuBarId="plotNavBar",
    menuDropdown('Display', 
                 menuDropdown('Points',
                              shinyDMDMenu::menuItem( checkBoxInput4Menu('cmdLabelPoints', 'Label Points', FALSE, width="100px"), value="consolidate"),
                              shinyDMDMenu::menuItem( checkBoxInput4Menu('cmdRestrictRows', 'Restrict to Current Row', FALSE, width="150px"), value="consolidate"),
                              shinyDMDMenu::menuItem( checkBoxInput4Menu('cmdInsertEnabled', 'Insertion Enabled', TRUE, width="120px"), value="consolidate")
                              # shinyDMDMenu::menuItem('Show Points without Labels', value='cmdShowPointsNoLabels'),
                              # shinyDMDMenu::menuItem('Show Points with Labels',    value='cmdShowPointLabels')
                 ),
                 menuDropdown( 'Grid',
                               #shinyDMDMenu::menuItem('Show', value='cmdShowGrid'),
                               shinyDMDMenu::menuItem( checkBoxInput4Menu('cmdShowGrid', 'Show', FALSE, width="100px"), value="consolidate"),
                               shinyDMDMenu::menuItem('Spacing', value='cmdAdjustGridSpacing')
                 )
                 ,
                 menuDropdown( 'Backdrop',
                               shinyDMDMenu::menuDropdown('colour',
                                 menuItem(div(id='bullfrog', style='width:80px;',
                                   colourInput( "backdropColour", NULL, palette = "limited", 
                                     allowedCols = 
                                       c(
                                         "white", "black", "red", "#DDD", "blue","green", "#333",
                                         value='white'
                                       )
                                     )
                                  ))
                                ),
                               shinyDMDMenu::menuItem( 
                                    checkBoxInput4Menu('solidBackdrop', 'Solid', TRUE, width="100px"), value="consolidate"
                                )
                 )
    ),
    menuDropdown(
      "Tools",
      shinyDMDMenu::menuDropdown(
        "Point Preprocessor", 
        shinyDMDMenu::menuItem('New', value='cmdNewPP'),
        shinyDMDMenu::menuDropdown(id='dropDown-editPreProc-points','Edit preproc points'),
        shinyDMDMenu::menuItem('Import', value='cmdImportPP')#,
        #shinyDMDMenu::menuItem('Export', value='cmdExportPP'),
        #shinyDMDMenu::menuItem('Remove', value='cmdRemovePP')
      ),
      shinyDMDMenu::menuDropdown(
        "Attribute Preprocessor",
        shinyDMDMenu::menuItem('New', value='cmdNewAP'),
        shinyDMDMenu::menuDropdown(id='dropDown-editPreProc-attrs','Edit preproc attrs'),
        shinyDMDMenu::menuItem('Import', value='cmdImportAP')
        #shinyDMDMenu::menuItem('Export', value='cmdExportAP'),
        #shinyDMDMenu::menuItem('Remove', value='cmdRemoveAP')
      ),
      shinyDMDMenu::menuDropdown(
        "Column Choice Sets", 
        shinyDMDMenu::menuItem('New', value='cmdNewColumnChoices'),
        shinyDMDMenu::menuDropdown(id='dropDown-cmdEditColumnChoices', 'Edit Choices'),
        shinyDMDMenu::menuItem('Import', value='cmdImportColumnChoices')
      )
    )
  ) #menubar end 
}
