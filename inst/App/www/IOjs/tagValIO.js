
function PtRPanelTagVal(svgId){ //currently svgId is not used here
  this.selectedElement = 0;
  this.svgId=svgId;
}

//TRANSFORM

// selectElement 
PtRPanelTagVal.prototype.selectElement = function (evt) {
  evt.stopPropagation();
  this.selectedElement = evt.currentTarget;
  
  //add eventattrs to element
  //this.selectedElement.parentNode.appendChild( this.selectedElement ); //brings to top
  var pth = "ptRPlotter_" + this.svgId;
  this.selectedElement.setAttributeNS(null, "onmouseout", pth + ".deselectElement(evt)");
  this.selectedElement.setAttributeNS(null, "onmouseup",  pth + ".deselectElement(evt)");
};


  // deselect that element
PtRPanelTagVal.prototype.deselectElement =  function (evt) {
  if(this.selectedElement !== 0){
    evt.stopPropagation();
    let kc=$( "#svgOutPanel" ).data("keycode");
    let r = Math.random().toString(36).substring(7);
    Shiny.onInputChange("mouseMssg",
      {
          cmd: "tagValSelect",
          vec: [0, 0],
          id : this.selectedElement.getAttribute("tid"),
          keycode:      kc,
          altKey:   !!evt.altKey,
          shiftKey: !!evt.shiftKey,
          ctrlKey:  !!evt.ctrlKey,
          metaKey:  !!evt.metaKey, 
          rnd: r
      }
    );
    this.selectedElement.removeAttributeNS(null, "onmouseout");
    this.selectedElement.removeAttributeNS(null, "onmouseup");
    this.selectedElement = 0;
    
  }
};








