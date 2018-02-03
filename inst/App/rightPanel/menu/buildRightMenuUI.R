buildRightMenu<-function(){
  dmdMenuBarPage(
    menuBarId="plotNavBar",
    menuDropdown('Column',
                 menuItem('Add Column', value='cmdNewColumn') ,
                 menuItem('Set Max ncol', value='cmdSetMatColMax')
                 
                 #   menuItem('Delete Column', value='cmdDeleteColumn'),
                 #   menuItem('Rename Column', value='cmdRenameColumn'),
                 #   menuItem('Clone Column', value='cmdCloneColumn'),
                 #   menuItem('Input Using', value='cmdInputUsingColumn')
    ),
    menuDropdown('Display', 
                 menuDropdown('Points',
                              menuItem('Hide Points', value='cmdHidePoints'), 
                              menuItem('Show Points with Labels', value='cmdShowPointLabels')
                 ),
                 menuDropdown( 'Grid',
                               menuItem('Show Grid', value='cmdShowGrid')
                               # ,
                               # menuDropdown('lines',
                               #              menuItem('Width', value='cmdGridLineWidh'), 
                               #              menuItem('Color', value='cmdGridLineColor')
                               # )
                 )
                 # ,
                 # menuDropdown( 'Backdrop',
                 #               menuItem('Hide', value='cmdHideBack'),
                 #               menuItem('Color', value='cmdBackDropColor')
                 # )
    ),
    menuDropdown(
      "Tools", 
      menuItem("PointFiltering (Not implemented)" )
    )
  ) #menubar end 
}
