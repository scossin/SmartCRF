Shiny.addCustomMessageHandler('getAvailableDocuments', function(message){
	var divId = message.divId; // divId : addTimelineId
	console.log("the divId is : " + divId);
	
	var jQuerySelector = '#' + divId + ' > div';

	console.log("the JquerySelector is : " + jQuerySelector);
	
	var divs = $(jQuerySelector); // select all child div of this divId
	
	console.log("number of child : " + divs.length);
	
	var concatenatedNames = [];
	for (var i = 0 ; i<divs.length ; i++){ // for each child
	console.log("entering the loop");
		var id = divs[i].id;
		var documentName = id.match("^[A-Z]+")[0];
		concatenatedNames[i] = documentName;
	}

	Shiny.onInputChange("availableDocuments",concatenatedNames);
});
