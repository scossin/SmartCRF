document.addEventListener('copy', function(e){
  var selectedText = window.getSelection().toString(); 
  console.log(selectedText);
  Shiny.onInputChange("arguments",selectedText);
});

document.addEventListener('paste', function(e){
   var id = Math.random(); // message sent to shiny need to be different at each call
  Shiny.onInputChange("showModalSAVE",id);
});
