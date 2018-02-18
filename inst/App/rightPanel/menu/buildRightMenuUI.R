buildRightMenu<-function(){
  dmdMenuBarPage(
    menuBarId="plotNavBar",
    menuDropdown('Column',
                 shinyDMDMenu::menuItem('Add Column', value='cmdNewColumn') ,
                 shinyDMDMenu::menuItem('Set Max ncol', value='cmdSetMatColMax')
                 
                 #   shinyDMDMenu::menuItem('Delete Column', value='cmdDeleteColumn'),
                 #   shinyDMDMenu::menuItem('Rename Column', value='cmdRenameColumn'),
                 #   shinyDMDMenu::menuItem('Clone Column', value='cmdCloneColumn'),
                 #   shinyDMDMenu::menuItem('Input Using', value='cmdInputUsingColumn')
    ),
    menuDropdown('Display', 
                 menuDropdown('Points',
                              shinyDMDMenu::menuItem('Show Points without Labels', value='cmdShowPointsNoLabels'),
                              shinyDMDMenu::menuItem('Show Points with Labels', value='cmdShowPointLabels'),
                              shinyDMDMenu::menuItem('Hide Points', value='cmdHidePoints') 
                 ),
                 menuDropdown( 'Grid',
                               shinyDMDMenu::menuItem('Show Grid', value='cmdShowGrid')
                               # ,
                               # menuDropdown('lines',
                               #              shinyDMDMenu::menuItem('Width', value='cmdGridLineWidh'), 
                               #              shinyDMDMenu::menuItem('Color', value='cmdGridLineColor')
                               # )
                 )
                 # ,
                 # menuDropdown( 'Backdrop',
                 #               shinyDMDMenu::menuItem('Hide', value='cmdHideBack'),
                 #               shinyDMDMenu::menuItem('Color', value='cmdBackDropColor')
                 # )
    ),
    menuDropdown(
      "Tools", 
      shinyDMDMenu::menuItem("PointFiltering (Not implemented)" )
    )
  ) #menubar end 
}
