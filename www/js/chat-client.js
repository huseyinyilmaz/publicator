"use strict";

$(function(){
    ////////////////////
    // User structure //
    ////////////////////
    var user_prototype = {};
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

    function call_fun_list(fun_list, evt){
	fun_list.forEach(function(element){element(evt);})};
    
    var status_list = {
	initializing: 'initializing',
	opened: 'opened',
	closed: 'closed'};

    /////////////////
    // Chat Client //
    /////////////////
    var publicatorChat = {
	get_client: function(room_code){
	    var chatClient = {
		client: null,
		
		/* if puclicator-client is not initialized yet return 'initializing'
		 * if publicator-client is connected return opened
		 * if publicator-client is disconnected retur closed */
		status: status_list.initializing,
		status_list: status_list,
		
    		room_code: room_code,
    		user: null,
    		user_list: null,
		handlers: {
		    onopen_handler_list: [],
		    onclose_handler_list: [],
                    onmessage_handler_list:[],
		    oninfo_handler_list:[],
		    onerror_handler_list:[]
		},

		
		onopen: function(fun){this.handlers.onopen_handler_list.push(fun);},
		onclose: function(fun){this.handlers.onclose_handler_list.push(fun);},
		onmessage: function(fun){this.handlers.onmessage_handler_list.push(fun);},
		oninfo: function(fun){this.handlers.oninfo_handler_list.push(fun);},
		onerror: function(fun){this.handlers.onerror_handler_list.push(fun);},
		
    		connect_to_server: function(session_id){
    		    publicator.get_client(
			_.bind(function(client){
			    var that = this;
			    this.client = client;
    			    console.log('Got Client', this.client);
    			    // create current user
    			    this.user = new create_user(session_id, session_id);
    			    this.user_list = [this.user];
			    this.client.onopen(function(msg){
				that.status = that.status_list.opened;
				call_fun_list(that.handlers.onopen_handler_list, msg);
			    });
			    this.client.onclose(function(msg){
				that.status = that.status_list.closed;
				call_fun_list(that.handlers.onclose_handler_list, msg);
			    });
			    this.client.onmessage(function(msg){that._receive_message(msg);});
			    this.client.onerror(function(msg){console.log('AAAAA', msg);});
			    this.client.oninfo(function(msg){console.log('BBBBBB', msg)});
			    // subscribe to room
			    this.client.subscribe(room_code, 'all');
    			    this.client.publish(room_code,
    						{'type': 'user_data',
    						 'user_code': this.user.code,
    						 'user_nick': this.user.nick});
			    this.status = this.status_list.opened;
			    // XXX trigger on open handlers
			},this),
			session_id,
			'localhost:8766');

    		},

		_receive_message: function(data){
		    // We always send json data
		    data = JSON.parse(data);
		    if(enable_logging && console)
			console.log('response', data);

		    switch(data.type){
		    case 'subscribed':
			if(data.data != this.room_code){
			    alert('Connected to wrong room');
			};
			chatApp.log('Connected to room ' + data.data);
			chatApp.log('Current user data: ' +
				    this.user.code + ' - ' + this.user.nick);
			//send info to user (connected to room)
			break;
 
		    case 'user_data':
			chatApp.log('New user data: ' + data.code + ' - ' + data.nick);
			if (data.code == chatApp.user_code){
			    chatApp.user_code = data.code;
			    chatApp.user_nick = data.nick;
			    //Change user nick on ui
			    $("#current_user_nick").text(chatApp.user_nick);
			}
     
			chatApp.add_user(data.code, data.nick);
			break;
		    case 'room_data':
			  chatApp.log('New room data: ' +
				      data.code +
				      ' Is room locked(' + data.is_locked + ')' );
			  chatApp.update_room(data.code, data.is_locked)
			  break;

			  case 'user_removed':
			  chatApp.log('Remove user: ' + data.code);
			  chatApp.remove_user(data.code);
			  break;

		          case 'message':
			  chatClient.trigger('onmessage', data.msg);
			  // chatApp.add_message(data.code, data.message, 'message');
			  break;

		          case 'error':
			  chatApp.log('ERROR: ' + data.message );
			  alert(data.data);
			  break;
		      };
		    

		},

    		send_message: function(obj){
    		    if(enable_logging && console)
    			console.log('request', obj);
    		    var json_string = JSON.stringify(obj);
    		    this.bullet.send(json_string);
    		}
            }; //chatClient
	    
	    //XXX move this to main file
	    publicator.set_host('http://localhost:8766');
	    //Call session which starts initialization
	    publicator.get_session_id(_.bind(chatClient.connect_to_server,
					     chatClient));
	    return chatClient;
	    
	}
    }


    ////////////////////
    // Bootstrap code //
    ////////////////////
    window.publicatorChat = publicatorChat;
});
