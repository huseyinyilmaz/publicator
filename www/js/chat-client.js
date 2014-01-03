(function(){
    "use strict";

    ////////////////////
    // User structure //
    ////////////////////
    var user_prototype = {};
    function User(user_code, user_nick){
        this.code = user_code;
        this.nick = user_nick;
        this.is_writing = false;
    }
    // set prototype
    User.prototype = user_prototype;
    
    function create_user(user_code, user_nick){
        return new User(user_code, user_nick);}

    function call_fun_list(fun_list, evt){
        fun_list.forEach(function(element){element(evt);});}

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
                // {user_code => user}
                users: {},
                update_user: function(user){
                    if(this.users[user.code] === undefined){
                        this.users[user.code] = user;
                        this.trigger_user_change();
                    }else{
                        //user is exist in users list
                        var existing_user = this.users[user.code];
                        // if there is change on user change it on 
                        if(existing_user.nick != user.nick){
                            existing_user.nick = user.nick;
                            //if changing user is current user also change
                            //current user nick
                            if(this.user.code == user.code){
                                this.user.nick = user.nick;
                            }
                            this.trigger_user_change();
                        }
                    }
                },
                update_user_writing_state: function(code, state){
                    var user = this.users[code];
                    if(user.is_writing !== state){
                        user.is_writing = state;
                        if(code === this.user.code)
                            this.user.is_writing = state;
                    }
                },
                add_user_code_list: function(user_code_list){
                    var that = this;
                    var new_codes = _.filter(
                        user_code_list,
                        function(user_code){
                            return !_.any(that.users,
                                         function(value,key,obj){
                                             return key == user_code;
                                         });});
                    if(new_codes){
                        _.each(new_codes,
                               function(code){
                                   that.users[code] = create_user(code, code);
                               });
                        this.trigger_user_change();
                    }
                },
                remove_user: function(user_code){
                    delete this.users[user_code];
                    this.trigger_user_change();
                },
                handlers: {
                    onopen_handler_list: [],
                    onclose_handler_list: [],
                    onmessage_handler_list:[],
                    oninfo_handler_list:[],
                    onerror_handler_list:[]},
        
                
                onopen: function(fun){this.handlers.onopen_handler_list.push(fun);},
                onclose: function(fun){this.handlers.onclose_handler_list.push(fun);},
                onmessage: function(fun){this.handlers.onmessage_handler_list.push(fun);},
                oninfo: function(fun){this.handlers.oninfo_handler_list.push(fun);},
                onerror: function(fun){this.handlers.onerror_handler_list.push(fun);},
        
                trigger_open: function(){
                    this.send_user_data(this.user);
                    this.status = this.status_list.opened;
                    call_fun_list(this.handlers.onopen_handler_list,
                                  {room_code: this.room_code,
                                   user_code: this.user.code,
                                   user_nick: this.user.nick});
                    this.client.get_consumers(this.room_code);
                },
        
                trigger_close: function(){
                    this.status = this.status_list.closed;
                    call_fun_list(this.handlers.onclose_handler_list, data.data);
                },
                trigger_message: function(data){
                    call_fun_list(this.handlers.onmessage_handler_list, data);
                },
                trigger_info: function(data){
                    call_fun_list(this.handlers.oninfo_handler_list, data);
                },
                trigger_error: function(data){
                    call_fun_list(this.handlers.onerror_handler_list, data);
                },
                trigger_user_change: function(){
                    this.trigger_info({type: 'user_change'});
                },
                trigger_writing_change:function(code){
                    this.trigger_info({type:'writing_change'});
                },
                
                connect_to_server: function(session_id){
                    publicator.get_client(
                        _.bind(function(client){
                            var that = this;
                            this.client = client;
                            console.log('Got Client', this.client);
                            // create current user
                            // Will be add users after creating getting back users data
                            this.user = create_user(session_id, session_id);
                            this.client.onopen(function(msg){
                                that.status = that.status_list.opened;
                                that.client.subscribe(room_code, 'all');
                                // this.client.XXX;
                            });
                            this.client.onclose(function(msg){
                                that.status = that.status_list.closed;
                                call_fun_list(that.handlers.onclose_handler_list, msg);
                                // publicatorChat.onOpen is triggered after room connection
                            });
                            this.client.onmessage(function(data){
                                if(data.channel_code == that.room_code){
                                    switch(data.type){
                                    case 'message':
                                        that._receive_message(data.data);
                                        break;
                                    default:
                                        console.log("INVALID XXX data", data);
                                    }
                                }else{
                                    console.log('Getting different room_code', data);
                                }
                                // subscribe to room
                            });
                            
                            this.client.onerror(function(msg){
                                that.trigger_error(msg);});
                            this.client.oninfo(function(msg){
                                if((that.roomcode === undefined && msg.type == 'subscribed')||
                                   msg.channel_code == that.room_code){
                                    that._receive_info(msg);
                                }else{
                                    console.error('Wrong channel code.(info)');
                                }
                                
                            });
        
                        },this),
                        session_id);
        	},
            
                _receive_info: function(data){
                    if(enable_logging && console)
                        console.log('info response, chat-client', data);
                    switch(data.type){
                    case 'subscribed':
                        this.trigger_open();
                        break;
                    case 'consumers':
                        this.add_user_code_list(data.data);
                        break;
                    case 'add_subscribtion':
                        var user = create_user(data.data, data.data);
                        chatClient.update_user(user);
                        this.send_user_data(chatClient.user);
                        break;
                    case 'remove_subscribtion':
                        this.remove_user(data.data);
                        break;
                    default:
                        console.log("Invalid info type, chat-client", data);
                    }
        
                },
                _receive_message: function(data){
                    if(enable_logging && console)
                        console.log('response, chat-client', data);
                    switch(data.type){
                    case 'message':
                        var user_code = data.code;
                        this.trigger_message(data);
                        this.update_user_writing_state(user_code, false);
                        this.trigger_writing_change();
                        break;
                    case 'user_data':
                        console.log('New user data: ' + data.user_code + ' - ' + data.user_nick);
                        var user = create_user(data.user_code, data.user_nick);
                        chatClient.update_user(user);
                        break;
                    case 'start_writing':
                        this.update_user_writing_state(data.data, true);
                        
                        this.trigger_writing_change();
                        break;
                    case 'stop_writing':
                        this.update_user_writing_state(data.data, false);
                        this.trigger_writing_change();
                        break;
                    case 'user_removed':
                        chatApp.log('Remove user: ' + data.code);
                        chatApp.remove_user(data.code);
                        break;
        
                    case 'error':
                        chatApp.log('ERROR: ' + data.message );
                        alert(data.data);
                        break;
                    }
        
                },
        
                send_message: function(msg){
                    if(enable_logging && console)
                        console.log('request, chat-client', msg);
                    this.client.publish(this.room_code,
                                        {code:this.user.code,
                                         type: 'message',
                                         data: msg});
        
                },
                send_user_data: function(user){
                    this.client.publish(this.room_code,
                                        {'type': 'user_data',
                                         'user_code': user.code,
                                         'user_nick': user.nick});
                },
        
                rename: function(nick){
                    this.send_user_data(create_user(chatClient.user.code,nick));
                },
                start_writing: function(){
                    if(!this.user.is_writing){
                        this.client.publish(this.room_code,
                                            {'type': 'start_writing',
                                             'data': this.user.code});                
                    }
                },
                stop_writing: function(){
                    if(this.user.is_writing){
                        this.client.publish(this.room_code,
                                            {'type': 'stop_writing',
                                             'data': this.user.code});                
                    }
                }
        
    }; //chatClient
                //Call session which starts initialization
                publicator.get_session_id(_.bind(chatClient.connect_to_server,
                                                 chatClient));
                return chatClient;
            }
        };

    ////////////////////
    // Bootstrap code //
    ////////////////////
    window.publicator.chat = publicatorChat;
    
}());
