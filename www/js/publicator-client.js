(function(){
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
            function get_random_string(){return Math.random().toString(36).substring(7);}
            function callback_fun(e){
                var session_id = e.session;
                // if there is no session id throw given server error
                // like permission denied.
                if(!session_id){
                    throw e.error;
                }
                callback(session_id);}

            var url = ('http://' + this.host + '/session/' +
                       get_random_string());
            // if host is external add callback param to activate jsonp
            if(this.host)
                url += '/?callback=?';
            //Add random string string to end of the url so it will not be cached from browser
            $.ajax({
                dataType: "json",
                url: url,
                data: {auth_info: 'publicator_client.js_test_auth'},
                success: callback_fun
            });
            
        },

        get_client: function(callback, session_id){
            var status_list = {
                initializing: 'initializing',
                opened: 'opened',
                closed: 'closed'};

            // Create chat client 
            var publicatorClient = {
                status_list: status_list,
                status: status_list.initializing,
                host: publicator.host,
                send_message: function(obj){
                    if(enable_logging && console)
                        console.log('request', obj);
                    this.transport.send(obj);
                },

                subscribe: function(channel_code, handler_type){
                    if(!handler_type){
                        handler_type = 'message_only';
                    }
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
                onerror:function(fun){this.handlers.onerror_handler_list.push(fun);},

                trigger_message: function(data){
                    publicatorClient.handlers.onmessage_handler_list.forEach(
                        function(fun){fun(data);});
                },
                trigger_info: function(data){
                    publicatorClient.handlers.oninfo_handler_list.forEach(
                        function(fun){fun(data);});
                },
                trigger_error: function(data){
                    publicatorClient.handlers.onerror_handler_list.forEach(
                        function(fun){fun(data);});
                }
                
            };
            // var websocket_host = location.host;
            // if(this.host !== ''){
            var websocket_host = publicator.host;
            // }

            function call_fun_list(fun_list, evt){
                fun_list.forEach(function(element){element(evt);});}

            var Transport = publicator.transports.websocket;
            var transport = Transport(publicator.host, session_id, false);
            publicatorClient.transport = transport;

            transport.onopen(function(evt){
                // if publicatorClient is being initialized return
                // putlicatorClient to callback
                if(publicatorClient.status === publicatorClient.status_list.initializing){
                    callback(publicatorClient);
                }
                call_fun_list(publicatorClient.handlers.onopen_handler_list, evt);});


            transport.onmessage(function(obj){
                switch(obj.type){
                case 'subscribed':
                    publicatorClient.trigger_info(obj);
                    break;
                case 'consumers':
                    publicatorClient.trigger_info(obj);
                    break;
                case 'add_subscribtion':
                    publicatorClient.trigger_info(obj);
                    break;
                case 'remove_subscribtion':
                    publicatorClient.trigger_info(obj);
                    break;
                case 'error':
                    publicatorClient.trigger_error(obj);
                    break;
                default:
                    publicatorClient.trigger_message(obj);
                    break;
                }});
            
            return publicatorClient;
        }//get_client
    };//window.publicator


    /////////////////////////////////////////////////////////////
    ////////////////
    // TRANSPORTS //
    ////////////////
    
    publicator.transports = {};

    /////////////////////////
    // Websocket Transport //
    /////////////////////////
    publicator.transports.websocket =
        function(host, session_id, is_secure){
            var handlers = {
                onopen_handler_list: [],
                onclose_handler_list: [],
                onmessage_handler_list:[],
                onerror_handler_list:[]};

            var transport = {

                onopen:function(fun){handlers.onopen_handler_list.push(fun);},
                onclose:function(fun){handlers.onclose_handler_list.push(fun);},
                onmessage:function(fun){handlers.onmessage_handler_list.push(fun);},
                onerror:function(fun){handlers.onerror_handler_list.push(fun);},

                send: function(obj){
                    var json_string = JSON.stringify(obj);
                    transport.websocket.send(json_string);
                },
                trigger_message: function(data){
                    handlers.onmessage_handler_list.forEach(
                        function(fun){fun(data);});
                },
                trigger_onopen: function(){
                    handlers.onopen_handler_list.forEach(
                        function(fun){fun();});
                },
                trigger_onclose: function(){
                    handlers.onclose_handler_list.forEach(
                        function(fun){fun();});
                },
                trigger_error: function(data){
                    handlers.onerror_handler_list.forEach(
                        function(fun){fun(data);});
                }};//transport
            
            var url = (is_secure?'wss://':'ws://') + host + '/' +
                    session_id + '/ws/';
            
            var websocket = new WebSocket(url);
            transport.websocket = websocket;
            // Bind websocket events to publicatorClient events
            
            websocket.onopen = transport.trigger_onopen;
            websocket.onclose = transport.trigger_on_close;
    
            websocket.onerror = function(evt){
                transport.trigger_on_error(evt);};
    
            websocket.onmessage = function(evt){
                console.log('Websocket_on_message_handler', evt);
                if(evt.type == 'message'){
                    var obj = JSON.parse(evt.data);
                    transport.trigger_message(obj);
                }else if(evt.type == 'error'){
                    transport.trigger_error(evt.data);
                }else{
                    transport.trigger_error(evt);
                }
            };
            return transport;
        };//websocket transport
}());
