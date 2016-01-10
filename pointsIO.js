var chosen =[];
var movedTo=[];

var selectedElement = 0;
var currentX = 0;
var currentY = 0;
var origX = 0;
var origY = 0;
var currentMatrix = 0;

var svg = document.querySelector("svg");
var pt  = svg.createSVGPoint();

//POINTS


//called to create a new point
function newPoint(evt) {
  pt.x = evt.clientX;
  pt.y = evt.clientY;
  
  // The cursor point, translated into svg coordinates
  var cursorpt =  pt.matrixTransform(svg.getScreenCTM().inverse());
  //console.log("(" + cursorpt.x + ", " + cursorpt.y + ")");
  ptTxt="c(" + cursorpt.x + ", " + cursorpt.y + ")";
  chosen=["add", ptTxt];
  Shiny.onInputChange("mydata",chosen);
}

function selectPoint(evt){
  selectedElement = evt.target;
  currentX= evt.clientX;
  currentY = evt.clientY;
  selectedElement.setAttributeNS(null, "onmousemove", "movePoint(evt)");
  selectedElement.setAttributeNS(null, "onmouseout", "deselectPoint(evt)");
  selectedElement.setAttributeNS(null, "onmouseup",  "deselectPoint(evt)");
}

function movePoint(evt){
  if(selectedElement != 0){
    selectedElement = evt.target;
    var dx = evt.clientX - currentX;
    var dy = evt.clientY - currentY; 
  
    //get the element attribute values x,y 
    var cx = Number(selectedElement.getAttribute("cx"));
    var cy = Number(selectedElement.getAttribute("cy"));
    // update each attribute by x=x+dx, y=y+dy
    cx=cx+dx;
    cy=cy+dy;
    selectedElement.setAttributeNS(null, "cx", cx.toString());
    selectedElement.setAttributeNS(null, "cy", cy.toString());
    //update the current position
    currentX = evt.clientX;
    currentY = evt.clientY;
  }
}

function deselectPoint(evt){
  if(selectedElement != 0){
    //alert("delsected")
    pt.x = evt.clientX;
    pt.y = evt.clientY;
  
    // The cursor point, translated into svg coordinates
    var cursorpt =  pt.matrixTransform(svg.getScreenCTM().inverse());
    //console.log("(" + cursorpt.x + ", " + cursorpt.y + ")");
    ptTxt="c(" + cursorpt.x + ", " + cursorpt.y + ")";
    var id = selectedElement.getAttribute("id");


    //ptTxt="c(" + cursorpt.x + ", " + cursorpt.y + ")"
    chosen=["move", ptTxt, id];
    Shiny.onInputChange("mydata",chosen);
    
    //movedTo=[ptTxt];
    //alert("movedTo=["+movedToId+" "+movedToX])
    //Shiny.onInputChange("movedTo", movedTo);
    // selectedPtId= 
    //var id = selectedElement.getAttribute("id");
    // Shiny.onInputChange("id",id);
    selectedElement.removeAttributeNS(null, "onmousemove");
    selectedElement.removeAttributeNS(null, "onmouseout");
    selectedElement.removeAttributeNS(null, "onmouseup");
    selectedElement = 0;
  }              
}

