
function PtRPanelKeyMouse(svgId){
  //this.svgId=svgId;
  //this.svg=document.querySelector("#" + svgId); 
  //this.pt= this.svg.createSVGPoint();
}

//called to obtaine mouse with keycode
PtRPanelKeyMouse.prototype.onMouseDown = function (evt, svgId) {
  let kc=$( "#svgOutPanel" ).data("keycode");
  if(!!kc){
    var svg=document.querySelector("#" + svgId);
    var pt= this.svg.createSVGPoint();
    pt.x = evt.clientX;
    pt.y = evt.clientY;
    evt.stopPropagation();
    var cursorpt =  pt.matrixTransform(svg.getScreenCTM().inverse());
    Shiny.onInputChange("mouseMssg",
                        {
                          cmd:      "key",
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




