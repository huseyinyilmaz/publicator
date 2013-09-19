enable_logging = true;

function get_publicator_client(session_id){
    // Create chat client 
    var publicatorClient = {
	
	connect: function(session_id){
	    this.send_message({type: 'connect_to_room',
			       room_code: room_code,
			       user_code: user_code,
			       user_nick: user_nick});
	},
	send_message: function(obj){
	    if(enable_logging && console)
		console.log('request', obj);
	    var json_string = JSON.stringify(obj);
	    this.bullet.send(json_string);
	},
	
	handlers: {
	    onopen_handler_list:[],
	    ondisconnect_handler_list:[],
	    onmessage_handler_list:[],
	    onheartbeat_handler_list:[]
	},
	onopen:function(fun){this.handlers.onopen_handler_list.push(fun);},
	ondisconnect:function(fun){this.handlers.ondisconnect_handler_list.push(fun);},
	onheartbeat:function(fun){this.handlers.onheartbeat_handler_list.push(fun);},
	onmessage:function(fun){this.handlers.onmessage_handler_list.push(fun);}

    };
    //Make chatClient an event Handler
    publicatorClient = _.extend(publicatorClient, Backbone.Events);


    var host = location.host;
    var bullet = $.bullet('ws://' + host + '/ws');
    publicatorClient.bullet = bullet;

    function call_fun_list(fun_list){
	fun_list.forEach(function(element){element();})};
    
    // Bind bullet events to chatClient events
    bullet.onopen = function(){
	call_fun_list(publicatorClient.handlers.onopen_handler_list);};
    bullet.ondisconnect = function(){
	call_fun_list(publicatorClient.handlers.ondisconnect_handler_list);};
    bullet.onhearthbeat = function(){
	call_fun_list(publicatorClient.handlers.onhearthbeat_handler_list);};
    bullet.onmessage = function(e){
	publicatorClient.handlers.onmessage_handler_list.forEach(function(fun){
	    fun(JSON.parse(e.data));
	});
    }
    
    return publicatorClient;
    
};

var host = location.host;

$.getJSON('session/',function(data){
    client = get_publicator_client(data.session);
    client.onheartbeat(function(){
	console.log('on hearthbeat');
    })
    client.onmessage(function(e){
	console.log('message',e);
    })

    window.publicatorClient = client;
    console.log(client);
});


