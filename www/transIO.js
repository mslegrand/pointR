var chosen =[];
var movedTo=[];

var ptR_selectedElement = 0;
var ptR_currentX = 0;
var ptR_currentY = 0;
var ptR_origX = 0;
var ptR_origY = 0;
var ptR_currentMatrix = 0;

//var ptR_svg = document.querySelector("svg"); // make this into 1 line???
//var pt  = ptR_svg.createSVGPoint();

//TRANSFORM

// selectElement 
function selectElement(evt) {
  ptR_selectedElement = evt.currentTarget;
  ptR_currentX = evt.clientX;
  ptR_currentY = evt.clientY;
  ptR_origX=evt.clientX;
  ptR_origY=evt.clientY;
  ptR_currentMatrix = ptR_selectedElement.getAttributeNS(null, "transform").slice(7,-1).split(" ");
  for(var i=0; i<ptR_currentMatrix.length; i++) {
    ptR_currentMatrix[i] = parseFloat(ptR_currentMatrix[i]);
  }
  //add eventattrs to element
  ptR_selectedElement.parentNode.appendChild( ptR_selectedElement ); //brings to top
  ptR_selectedElement.setAttributeNS(null, "onmousemove", "moveElement(evt)");
  ptR_selectedElement.setAttributeNS(null, "onmouseout", "deselectElement(evt)");
  ptR_selectedElement.setAttributeNS(null, "onmouseup",  "deselectElement(evt)");
}

// translation of an element
function moveElement(evt) {
  if(ptR_selectedElement!==0){ // this shouldn"t be necessary
    var dx = evt.clientX - ptR_currentX;
    var dy = evt.clientY - ptR_currentY;
    ptR_currentMatrix[4] += dx;
    ptR_currentMatrix[5] += dy;

    ptR_selectedElement.setAttributeNS(null, "transform", "matrix(" + ptR_currentMatrix.join(" ") + ")");
    ptR_currentX = evt.clientX;
    ptR_currentY = evt.clientY;
  }
  
}

// deselect that element
function deselectElement(evt) {
  if(ptR_selectedElement !== 0){
    var movedByX = evt.clientX - ptR_origX;
    var movedByY = evt.clientY - ptR_origY;
    
    var tid = ptR_selectedElement.getAttribute("tid");
    
    var currentMatrixAsString="c(" + ptR_currentMatrix.join(",") + ")";
    chosen=["trans", currentMatrixAsString, tid];
    Shiny.onInputChange("mouseMssg",chosen);
    
    ptR_selectedElement.removeAttributeNS(null, "onmousemove");
    ptR_selectedElement.removeAttributeNS(null, "onmouseout");
    ptR_selectedElement.removeAttributeNS(null, "onmouseup");
    ptR_selectedElement = 0;
    
  }
}
