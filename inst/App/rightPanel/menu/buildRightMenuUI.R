buildRightMenu<-function(){
  dmdMenuBarPage(
    menuBarId="plotNavBar",
    menuDropdown('Display', 
                 menuDropdown('Points',
                              shinyDMDMenu::menuItem('Show Points without Labels', value='cmdShowPointsNoLabels'),
                              shinyDMDMenu::menuItem('Show Points with Labels',    value='cmdShowPointLabels')
                 ),
                 menuDropdown( 'Grid',
                               shinyDMDMenu::menuItem('Show', value='cmdShowGrid'),
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
                                  checkboxInput('solidBackdrop', 'Solid', TRUE, width="100px"), value="consolidate"
                                )
                 )
    ),
    menuDropdown(
      "Tools",
      shinyDMDMenu::menuDropdown(
        "Point Preprocessor",
        shinyDMDMenu::menuItem('New', value='cmdNewPP'),
        shinyDMDMenu::menuItem('Edit', value='cmdEditPP'),
        shinyDMDMenu::menuItem('Import', value='cmdImportPP'),
        shinyDMDMenu::menuItem('Export', value='cmdExportPP'),
        shinyDMDMenu::menuItem('Remove', value='cmdRemovePP')
      ),
      shinyDMDMenu::menuDropdown(
        "Attribute Preprocessor",
        shinyDMDMenu::menuItem('New', value='cmdNewAP'),
        shinyDMDMenu::menuItem('Import', value='cmdImportAP'),
        shinyDMDMenu::menuItem('Export', value='cmdExportAP'),
        shinyDMDMenu::menuItem('Remove', value='cmdRemoveAP')
      )
    )
  ) #menubar end 
}
