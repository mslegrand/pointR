
function PtRPanelTranslate(svgId){ //currently svgId is not used here
  this.svg=document.querySelector("#" + svgId); 
  this.svgId=svgId +"_TRANSLATE";
  this.selectedElement = 0; //
  this.currentX = 0;
  this.currentY = 0;
  this.origX = 0;
  this.origY = 0;

  this.currentMatrix = 0;
}


// selectElement 
PtRPanelTranslate.prototype.selectElement = function(evt) {
   var pth = "ptRPlotter_"+this.svgId;
   
  this.selectedElement = evt.currentTarget;
  this.currentX = evt.clientX;
  this.currentY = evt.clientY;
  this.origX=evt.clientX;
  this.origY=evt.clientY;
  this.currentMatrix = this.selectedElement.getAttributeNS(null, "transform").slice(7,-1).split(" ");
  for(var i=0; i<this.currentMatrix.length; i++) {
    this.currentMatrix[i] = parseFloat(this.currentMatrix[i]);
  }
  //add eventattrs to element
  this.selectedElement.parentNode.appendChild( this.selectedElement ); //brings to top
  this.selectedElement.setAttributeNS(null, "onmousemove", pth + ".moveElement(evt)");
  this.selectedElement.setAttributeNS(null, "onmouseout", pth + ".deselectElement(evt)");
  this.selectedElement.setAttributeNS(null, "onmouseup",  pth + ".deselectElement(evt)");
};

// translation of an element
PtRPanelTranslate.prototype.moveElement = function(evt) {
  if(this.selectedElement!==0){ // this shouldn"t be necessary
    var dx = evt.clientX - this.currentX;
    var dy = evt.clientY - this.currentY;
    this.currentMatrix[4] += dx;
    this.currentMatrix[5] += dy;

    this.selectedElement.setAttributeNS(null, "transform", "matrix(" + this.currentMatrix.join(" ") + ")");
    this.currentX = evt.clientX;
    this.currentY = evt.clientY;
  }
};

// deselect that element
PtRPanelTranslate.prototype.deselectElement = function(evt) {
  if(this.selectedElement !== 0){
    var movedByX = evt.clientX - this.origX;
    var movedByY = evt.clientY - this.origY;
    
    Shiny.onInputChange("mouseMssg",{
      cmd: "trans",
      vec: this.currentMatrix, 
      id: this.selectedElement.getAttribute("tid")
    });
    this.selectedElement.removeAttributeNS(null, "onmousemove");
    this.selectedElement.removeAttributeNS(null, "onmouseout");
    this.selectedElement.removeAttributeNS(null, "onmouseup");
    this.selectedElement = 0;
    
  }
};

var ptRPlotter_ptR_SVG_TRANSFORM_TRANSLATE = new PtRPanelTranslate("ptR_SVG_TRANSFORM");
