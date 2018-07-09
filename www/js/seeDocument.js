seeDocument = function(e){
	console.log("user wants to see a document");
	var value = $(e).attr('value');
	console.log(value);
	Shiny.onInputChange("seeDocument",value);
};