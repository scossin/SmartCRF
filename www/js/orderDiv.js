Shiny.addCustomMessageHandler('orderDiv', function(message){
	message.reverse();
			 for (div in message){
				 //alert(div);
				 var objectId = $("#" + message[div]);
				 //objectId.remove();
				 $(objectId).parent().prepend($(objectId));			
			 }
});
