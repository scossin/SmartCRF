Shiny.addCustomMessageHandler('timelineClicked', function(message){
	var id = Math.random(); // needs to change
	var dispatcher = message.dispatcher;
	console.log("dispatch to : ");
	console.log(dispatcher);
	Shiny.onInputChange(dispatcher,id);
});