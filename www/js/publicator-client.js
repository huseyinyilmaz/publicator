"use strict";

window.enable_logging = true;

window.publicator = {
    host: '',
    websocket_host: '',

    set_host: function(host){
	if(!host){
	    host = '';}
	this.host = host;
    },
    get_session_id: function(callback){
	function get_random_string(){return Math.random().toString(36).substring(7);};
	function callback_fun(e){
	    var session_id = e.session;
	    console.log("Session id XXX", session_id);
	    callback(session_id)}

	var url = (this.host + '/session/' +
	           get_random_string());
	// if host is external add callback param to acrivate jsonp
	if(this.host)
	    url += '/?callback=?';
	//Add random string string to end of the url so it will not be cached from browser
	$.getJSON(url, callback_fun);
    },

    get_client: function(callback, session_id, host){
	// Create chat client 
	var publicatorClient = {
	    status_list:{
		initializing: 'initializing',
		opened: 'opened',
		closed: 'closed'}
	    status: status_list.initializing,
	    host: host,
	    send_message: function(obj){
		if(enable_logging && console)
		    console.log('request', obj);
		var json_string = JSON.stringify(obj);
		this.websocket.send(json_string);
	    },

            subscribe: function(channel_code, handler_type){
                if(!handler_type){
            	handler_type = 'message_only';
                };
                this.send_message({
            	type: 'subscribe',
            	data: {'channel_code': channel_code,
            	       'type': handler_type}});},
                
            unsubscribe: function(channel_code){
                this.send_message({
            	type: 'unsubscribe',
            	data: channel_code});},
            get_subscribtions: function(){
                this.send_message({
            	type: 'get_subscribtions',
            	data: null});},
            
            publish: function(channel_code, message){
                this.send_message({
            	type: 'publish',
            	data: {'channel_code': channel_code,
            	       'message':message}});},
                        get_consumers: function(channel_code){
                this.send_message({
            	type: 'get_consumers',
            	data: {'channel_code': channel_code}});},
                
            handlers: {
		onopen_handler_list: [],
		onclose_handler_list: [],
                onmessage_handler_list:[],
		oninfo_handler_list:[],
		onerror_handler_list:[]
            },
            onopen:function(fun){this.handlers.onopen_handler_list.push(fun);},
            onclose:function(fun){this.handlers.onclose_handler_list.push(fun);},
            onmessage:function(fun){this.handlers.onmessage_handler_list.push(fun);},
	    oninfo:function(fun){this.handlers.oninfo_handler_list.push(fun);},
	    onerror:function(fun){this.handlers.onerror_handler_list.push(fun);}
        };
	//Make chatClient an event Handler
	publicatorClient = _.extend(publicatorClient, Backbone.Events);
	var websocket_host = location.host;
	if(this.host != ''){
	    websocket_host = host;
	}
	var url = 'ws://' + websocket_host + '/' + session_id + '/ws/';
	console.log('websocket host = ' + url);
	var websocket = new WebSocket(url);
	
	publicatorClient.websocket = websocket;

	function call_fun_list(fun_list, evt){
	    fun_list.forEach(function(element){element(evt);})};
    
	// Bind websocket events to chatClient events
	websocket.onopen = function(evt){
	    // if chatClient is being initialized return chatClient to callback
	    if(chatClient.status = chatClient.initializing){
		callback(publicatorClient);
	    }
	    call_fun_list(publicatorClient.handlers.onopen_handler_list, evt);};

	    };
	websocket.onclose = function(evt){
	    // restart connection if necessary.
	    call_fun_list(publicatorClient.handlers.onclose_handler_list, evt);};
	};
	websocket.onerror = function(evt){
	    // trigger websocket error messages.
	    call_fun_list(publicatorClient.handlers.onerror_handler_list, evt);};
	
	websocket.onmessage = function(evt){
	    console.log('websocket_on_message_handler', evt);
	    if(evt.type == 'message'){
		publicatorClient.handlers.onmessage_handler_list.forEach(function(fun){
		    // fun(JSON.parse(e.data));
		    fun(evt.data);
		});
	    }else if(evt.type == 'error'){
		publicatorClient.handlers.onerror_handler_list.forEach(function(fun){
		    // fun(JSON.parse(e.data));
		    fun(evt.data);
		});
	    }else{
		var info_class = evt.type;
		evt.type = 'info';
		evt.info_class = info_class;
		publicatorClient.handlers.oninfo_handler_list.forEach(function(fun){
		    fun(evt);
		});
	    }
	};
    
	return publicatorClient;
    
    }
}
