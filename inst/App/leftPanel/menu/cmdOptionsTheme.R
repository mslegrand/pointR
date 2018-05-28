cmdOptionsTheme<-function(){
  
  themes<-getAceThemes()
  modalTheme <- function() {
    modalDialog(
      selectInput("selectTheme", "Theme", themes, multiple=FALSE, 
                  selected=editOption$theme,
                  selectize = FALSE, width="300px", size=1  ), 
      footer = tagList(actionButton("modalThemeCancel", "Cancel"),actionButton("modalThemeOk", "OK") )
    ) 
  }
  showModal( modalTheme() )
}

observeEvent(input$modalThemeCancel, {
  removeModal()
}) 

observeEvent(input$modalThemeOk, {
  nextTheme<-input$selectTheme
  if(nextTheme!=""){
    editOption$theme=nextTheme
  }  
  removeModal()
}) 

observeEvent(editOption$theme,{
  nextTheme<-editOption$theme
  isDark<-function(nt){
    nt %in% c(
      "ambiance","chaos","clouds_midnight","cobalt","idle_fingers",
      "kr_theme","merbivore","merbivore_soft","mono_industrial","monokai",
      "pastel_on_dark","solarized_dark","terminal","tomorrow_night","tomorrow_night_blue",
      "tomorrow_night_bright","tomorrow_night_eighties","twilight","vibrant_ink"
    )
  }
  colorSet<-list(
    ".ace_svgRAN"=c(255,140,0), ".ace_svgRME"=c(112,128,144),
    ".ace_svgRFE" =c(0,255,255), ".ace_svgRGR" =c(255,0,255),
    ".ace_svgRSH" =c(0,0,205), ".ace_svgRCO" =c(172,61,239),
    ".ace_svgRTX" =c(178,34,34), ".ace_svgRFI" =c(0,128,0),
    ".ace_svgRMM" =c(240,82,45) )
  
  colorSet<-lapply(colorSet, function(col){
    if(isDark(nextTheme)){
      col<-pmin(255,col+160)
    }
    paste0("rgb(", paste0(col,collapse=","), ")")
  })
  
  # session$sendCustomMessage(
  #   type = "shinyAceExt", 
  #   list(id= getAceEditorId(), sender='fileCmd.color', resetElementColor=colorSet)
  # )
  updateAceExt(id= getAceEditorId(), sender='fileCmd.color', resetElementColor=colorSet )

})

