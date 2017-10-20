
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
    //  alert("selectedElement");
    
    
    //var tid = ptR_selectedElement.getAttribute("tid");
      
    //var currentMatrixAsString="c(" + ptR_currentMatrix.join(",") + ")";
    //var trans=[movedByX,movedBy]; // return the translation
    //var tid = ptR_selectedElement.getAttribute("tid");
    
    //var dxy="c(" + movedByX + "," + movedByY + ")";
    //var chosen=["transGrp", dxy, tid];
    //Shiny.onInputChange("mouseMssg",chosen);
    Shiny.onInputChange("mouseMssg",
      {
        cmd: "tagValSelect",
        vec: [0, 0],
        id : this.selectedElement.getAttribute("tid")
      }
    );
    this.selectedElement.removeAttributeNS(null, "onmouseout");
    this.selectedElement.removeAttributeNS(null, "onmouseup");
    this.selectedElement = 0;
    
  }
};



var ptRPlotter_ptR_SVG_TagVal = new PtRPanelTagVal("ptR_SVG_TagVal");




