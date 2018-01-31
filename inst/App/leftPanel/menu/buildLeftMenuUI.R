
buildLeftMenu<-function(version){
  dmdMenuBarPage( 
    title=version, 
    # theme=shinytheme("cerulean"),
    menuBarId="editNavBar",
    menuDropdown(
      "File", 
      menuItem("New"),
      menuItem("Open"),
      menuDropdown("Recent Files"),
      menuDivider(),
      menuItem("Save"),
      menuItem("Save As...", value="saveAs"),
      menuItem("Export as SVG"),
      menuDivider(),
      menuItem("Quit", value="quit")
    ),
    # menuDropdown('Tibble',
    #              menuItem('New Tibble', value='cmdNewTibble'),
    #              menuItem('Rename Tibble', value='cmdRenameTibble'),
    #              menuItem('Delete Tibble', value='cmdDeleteTibble')
    # ),
    menuDropdown(
      'Configure',
      menuDropdown(
        "Editor Options", 
        menuItem("Theme" ),
        menuItem("Font Size"), 
        menuItem("Adjust Tabs",       value="adjustTabs"),
        menuItem("Show White Space"),
        menuItem(defaultOpts$tabType)
      ),
      menuDropdown(
        "Snippets",
        menuItem("Import", value="importSnippetFile"),
        menuItem("Disable")
      )
    ),
    menuDropdown(
      "Help",
      menuItem("Editor ShortCuts"),
      menuItem("Element Reference"),
      menuDropdown(
        "Useful Links", 
        menuItem(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/User_Guide.html\"  target=\"_blank\" >svgR User Guide </a></li>")),
        menuItem(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/\"  target=\"_blank\" >io.svgR</a></li>")),
        menuItem(HTML("<li><a  href=\"https://www.w3.org/TR/SVG/intro.html\"  target=\"_blank\" >W3C SVG reference</a></li>"))
      ),
      menuItem("About", value="aboutCmd")
    )
  )
}
