




bar1<-list(
  tbNewFile=list(            icon=c("icon-doc", "icon-plus"),               title = "New",                      cmd="tbNew" ),               
  tbSaveFile=list(           icon=c("icon-doc", "icon-floppy"),             title = "Save",                     cmd="tbSave" ),                 
  tbUndo=list(               icon=c("icon-ccw"),                            title = "Undo",                     cmd="undo" ),              
  tbRedo=list(               icon=c("icon-cw"),                             title = "Redo",                     cmd="redo"  ),               
  tbIndentRight=list(        icon=c("icon-indent-right"),                   title = "Indent Right Selection",   cmd="indent" ),               
  tbIndentLeft=list(         icon=c("icon-indent-left"),                    title = "Indent Left Selection",    cmd="outdent"  ),               
  tbToggleComment=list(      icon=c("icon-comment-empty","icon-toggle-on"), title = "Toggle Comment Selection", cmd="togglecomment" ),                  
  tbCollapse=list(           icon=c("icon-collapse"),                       title = "Collapse All",             cmd="foldall"  ),                
  tbExpand=list(             icon=c("icon-expand"),                         title = "Expand All",               cmd="unfoldall"  ),               
  tbNextBookMark=list(       icon=c("icon-bookmark", "icon-level-down"),    title = "Next BookMark",            cmd="nextBookMark"  ),                  
  tbPreviousBookMark=list(   icon=c("icon-bookmark", "icon-level-up" ),     title = "Previous BookMark",        cmd="previousBookMark"  ),                  
  tbDeleteAllBookMarks=list( icon=c("icon-bookmark","icon-cancel" ),        title = "Delete All BookMarks",     cmd="deleteAllBookMarks" ),                
  tbPrint=list(              icon=c("icon-print-1" ),                       title = "Print",                     cmd= 'print'  ),
  tbHelp=list(               icon=c("icon-help" ),                          title = "Help",                      cmd= 'help'  )
)



bar2<-list(
  tbGoto=list(               icon=c("icon-right-big", "-#"),             title = "Go to line",                 cmd= 'gotoline'),
  tbJumpToMatch=list(        icon=c("icon-right-big", "({"),             title = "Jump to matching",           cmd= 'jumptomatching'),
  tbSelectToMatch=list(      icon=c("icon-resize-vertical", "({"),       title = "Select to matching",         cmd= 'selecttomatching'),
  tbFind=list(               icon=c("icon-search" ),                     title = "Find",                       cmd= 'find'),
  tbFindNext=list(           icon=c("icon-search","icon-level-down" ),   title = "Find  Up",                   cmd= 'selectOrFindNext'),
  tbFindPrevious=list(       icon=c("icon-search","icon-level-up" ),     title = "Find  Down",                 cmd= 'selectOrFindPrevious'  ),
  tbFindNReplace=list(       icon=c("icon-search" ),                     title = "Find and Replace",           cmd= 'replace'  ),
  tbNextError=list(          icon=c("icon-attention","icon-level-down"), title = "Goto Next Error",            cmd= 'goToNextError'  ),
  tbPreviousError=list(      icon=c("icon-attention","icon-level-up" ),  title = "Goto Previous Error",        cmd= 'goToPreviousError'  ),
  tbMacroRecord=list(        icon=c("icon-record", "icon-toggle-on" ),   title = "Toggle Macro Recording",     cmd= 'togglerecording'  ),  
  tbMacroPlay=list(          icon=c("icon-record", "icon-play" ),        title = "Replay Macro Recording",     cmd= 'replaymacro')
  
)

buildHToolBar<-function(tbSpec){
  actionGroupButtons(
    inputIds=names(tbSpec),
    labels=lapply(tbSpec,function(tbsp){
      icons<-tbsp$icon
      indx<-grep("icon-",icons)
      icons[indx]<-sapply(icons[indx], function(i){ span('class'=i) }, simplify=FALSE, USE.NAMES=FALSE)
      span(icons)  %>% bs_embed_tooltip(title = tbsp$title)
    }), status='primary'
  )
}  