"use strict";

window.enable_logging = true;


window.publicator = {
    host: '',
    bullet_host: '',

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

    get_client: function(session_id, host){
	// Create chat client 
	var publicatorClient = {
	    host: host,
	    send_message: function(obj){
		if(enable_logging && console)
		    console.log('request', obj);
		var json_string = JSON.stringify(obj);
		this.bullet.send(json_string);
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
	var bullet_host = location.host;
	if(this.host != ''){
	    bullet_host = host
	}
	console.log('bullet host = ' + 'ws://' + bullet_host + '/' + session_id + '/ws/')
	var bullet = $.bullet('ws://' + bullet_host + '/' + session_id + '/ws/');
	
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
	    });};
    
	return publicatorClient;
    
    }
}
