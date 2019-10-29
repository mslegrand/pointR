//called to obtaine mouse with keycode
onKeyMouseDown = function (evt, svgId) {
  let kc=$( "#svgOutPanel" ).data("keycode");
  if(!!evt.altKey){
    // console.log('keyMouse: svgId='+svgId);
    // console.log('keyMouse: kc='+kc);
    let svg=document.querySelector("#" + svgId);
    let pt= svg.createSVGPoint();
    pt.x = evt.clientX;
    pt.y = evt.clientY;
    evt.stopPropagation();
    let cursorpt =  pt.matrixTransform(svg.getScreenCTM().inverse());
    // console.log('keyMouse: cursorpt='+ cursorpt.x +","+cursorpt.y );
    Shiny.onInputChange("mouseMssg",
                        {
                          cmd:      "keyMouse",
                          vec:      [cursorpt.x, cursorpt.y],
                          id:       svgId,
                          keycode:      kc,
                          altKey:   !!evt.altKey,
                          shiftKey: !!evt.shiftKey,
                          ctrlKey:  !!evt.ctrlKey,
                          metaKey:  !!evt.metaKey
                        }
    );
  }
  // The cursor point, translated into svg coordinates
};




