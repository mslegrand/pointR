
function PtRPanelTagVal(svgId){ //currently svgId is not used here
  this.selectedElement = 0;
  this.svgId=svgId;
}

//TRANSFORM

// selectElement 
PtRPanelTagVal.prototype.selectElement = function (evt) {
  
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
    Shiny.onInputChange("mouseMssg",
      {
          cmd: "tagValSelect",
          vec: [0, 0],
          id : this.selectedElement.getAttribute("tid"),
          altKey:   !!evt.altKey,
          shiftKey: !!evt.shiftKey,
          ctrlKey:  !!evt.ctrlKey,
          metaKey:  !!evt.metaKey
      }
    );
    this.selectedElement.removeAttributeNS(null, "onmouseout");
    this.selectedElement.removeAttributeNS(null, "onmouseup");
    this.selectedElement = 0;
    
  }
};








