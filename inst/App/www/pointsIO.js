// implements moving a single point

// var chosen =[];
// var movedTo=[];

var ptR_selectedElement = 0;
var ptR_currentX = 0;
var ptR_currentY = 0;

var ptR_svg = document.querySelector("#ptR_SVG_Point");
var pt  = ptR_svg.createSVGPoint();

//called to create a new point
function newPoint(evt) {
  pt.x = evt.clientX;
  pt.y = evt.clientY;
  
  // The cursor point, translated into svg coordinates
  var cursorpt =  pt.matrixTransform(ptR_svg.getScreenCTM().inverse());
  var ptTxt="c(" + cursorpt.x + ", " + cursorpt.y + ")";
  var chosen=["add", ptTxt];
  Shiny.onInputChange("mouseMssg",chosen);
}

function selectPoint(evt){
  ptR_selectedElement = evt.target;
  ptR_currentX = evt.clientX;
  ptR_currentY = evt.clientY;
  ptR_selectedElement.setAttributeNS(null, "onmousemove", "movePoint(evt)");
  ptR_selectedElement.setAttributeNS(null, "onmouseout", "deselectPoint(evt)");
  ptR_selectedElement.setAttributeNS(null, "onmouseup",  "deselectPoint(evt)");
}

function movePoint(evt){
  if(ptR_selectedElement !== 0){
    ptR_selectedElement = evt.target;
    var dx = evt.clientX - ptR_currentX;
    var dy = evt.clientY - ptR_currentY; 
  
    //get the element attribute values x,y 
    var cx = Number(ptR_selectedElement.getAttribute("cx"));
    var cy = Number(ptR_selectedElement.getAttribute("cy"));
    // update each attribute by x=x+dx, y=y+dy
    cx=cx+dx;
    cy=cy+dy;
    ptR_selectedElement.setAttributeNS(null, "cx", cx.toString());
    ptR_selectedElement.setAttributeNS(null, "cy", cy.toString());
    //update the current position
    ptR_currentX = evt.clientX;
    ptR_currentY = evt.clientY;
  }
}

function deselectPoint(evt){
  if(ptR_selectedElement !== 0){
    pt.x = evt.clientX;
    pt.y = evt.clientY;
  
    // The cursor point, translated into svg coordinates
    var cursorpt =  pt.matrixTransform(ptR_svg.getScreenCTM().inverse());
    var ptTxt="c(" + cursorpt.x + ", " + cursorpt.y + ")";
    var id = ptR_selectedElement.getAttribute("id");

    chosen=["move", ptTxt, id];
    Shiny.onInputChange("mouseMssg",chosen);
    
    ptR_selectedElement.removeAttributeNS(null, "onmousemove");
    ptR_selectedElement.removeAttributeNS(null, "onmouseout");
    ptR_selectedElement.removeAttributeNS(null, "onmouseup");
    ptR_selectedElement = 0;
  }              
}

