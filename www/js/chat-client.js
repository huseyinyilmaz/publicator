$(function(){
    ////////////////////
    // User structure //
    ////////////////////
    user_prototype = {};
    function User(user_code, user_nick, is_current){
	this.user_code = user_code;
	this.user_nick = user_nick;
	this.is_current = is_current;
    };
    // set prototype
    User.prototype = user_prototype;
    
    function create_user(user_code, user_nick, is_current){
	is_current = Boolean(is_current);
	return new User(user_code, user_nick, is_current);};

    /////////////////
    // Chat Client //
    /////////////////
    var publicatorChatClient = {
	client: null,
	room_code: null,
	user: null,
	user_list: null,
	connect_to_server: function(session_id){
	    this.client = publicator.get_client(session_id, 'localhost:8766');
	    console.log('Got Client', this.client);
	    // create current user
	    this.user = new create_user(session_id, session_id);
	    user_list = [this.user];
	    this.client.onopen(function(){publicatorChatClient.trigger('onopen');});
	    this.client.ondisconnect(function(){publicatorChatClient.trigger('disconnect');});
            this.client.onmessage(function(msg){publicatorChatClient.trigger('onmessage', msg);});
	    this.client.onheartbeat(function(){publicatorChatClient.trigger('onheartbeat');});
	},
	connect_to_room: function(room_code){
	    this.client.subscribe(room_code, 'all');
	    this.client.publish(room_code,
				{'type': 'user_data',
				 'user_code': this.user.code,
				 'user_nick': this.user.nick});
	},
	send_message: function(obj){
	    if(enable_logging && console)
		console.log('request', obj);
	    var json_string = JSON.stringify(obj);
	    this.bullet.send(json_string);
	}
    };


    ////////////////////
    // Bootstrap code //
    ////////////////////
    //Make chatClient an event Handler
    publicatorChatClient = _.extend(publicatorChatClient, Backbone.Events);
    publicator.set_host('http://localhost:8766');
    //Call session
    publicator.get_session_id(_.bind(publicatorChatClient.connect_to_server, publicatorChatClient));
    //initialization completed. Add chatClient to global namespace
    window.publicatorChatClient = publicatorChatClient;
});
