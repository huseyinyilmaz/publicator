(function(){
    "use strict";
    // window.WebSocket = undefined;
    window.enable_logging = true;

    function get_random_string(){
        return Math.random().toString(36).substring(7);
    }

    function uri_encode(obj) {
        var str = [];
        for(var p in obj)
            str.push(encodeURIComponent(p) + "=" + encodeURIComponent(obj[p]));
        return str.join("&");
    }

    function is_secure(){
        return document.location.protocol == 'https:';
    }
    
    window.publicator = {
        host: '',
        websocket_host: '',
        set_host: function(host){
            if(!host){
                host = '';}
            this.host = host;
        },
        send_ajax: function(url, data, callback){
            var xhr = new XMLHttpRequest();
            xhr.onreadystatechange = function(a,b,c){
                if(this.readyState === this.DONE) {
                    if(this.status == 200){
                        callback(JSON.parse(this.response));
                    }else{
                        callback({type:'error', data: "Unexpected status: " + this.status + ""});
                    }
                }
            };//readystatechange
            xhr.open('POST', url);
            xhr.setRequestHeader('Content-Type', 'application/json');
            xhr.send(JSON.stringify(data));
        },

        // sends jsonp request to client
        send_jsonp: function(url, data, callback){
            var func_name = 'publicator_callback_' + get_random_string();
            var uri_data = {data: JSON.stringify(data),
                            callback: func_name};
            data.callback = func_name;
            var head = document.getElementsByTagName('head')[0];

            var script = document.createElement('script');
            script.src = url + '?' + uri_encode(uri_data);
            script.setAttribute('id', func_name);            
            window[func_name] = function(result){
                callback(result);
                head.removeChild(script);
            };

            head.appendChild(script);

        },

        get_session_id: function(callback){
            function callback_fun(data){
                if(data.type == 'error'){
                    throw data.data;
                }else{
                    callback(data.data);
                }
            }

            // function callback_fun(e){
            //     var session_id = e.session;
            //     // if there is no session id throw given server error
            //     // like permission denied.
            //     if(!session_id){
            //         throw e.error;
            //     }
            //     callback(session_id);}

            var url = ((is_secure()?'https://':'http://') + this.host + '/session/');

            // if host is external add callback param to activate jsonp
            // if(this.host)
            //     url += '/?callback=?';
            //Add random string string to end of the url so it will not be cached from browser
            // $.ajax({
            //     dataType: "json",
            //     url: url,
            //     data: {auth_info: 'publicator_client.js_test_auth'},
            //     success: callback_fun
            // });
            
            //Add random string string to end of the url so it will not be cached from browser
            // publicator.send_ajax('POST',
            //                      url,
            //                      {auth_info: 'publicator_client.js_test_auth'},
            //                      callback_fun);
            var session_transport = publicator.send_jsonp;
            if(document.location.host == this.host)
                session_transport = publicator.send_ajax;
            session_transport(url,
                              {auth_info: 'publicator_client.js_test_auth'},
                              callback_fun);

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

            var create_transport = publicator.transports.http;
            if(window.WebSocket){
                create_transport = publicator.transports.websocket;
            }
            // var Transport = publicator.transports.websocket;
            var transport = create_transport(publicator.host, session_id);
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
    };//window.publiactor


    /////////////////////////////////////////////////////////////
    ////////////////
    // TRANSPORTS //
    ////////////////
    
    publicator.transports = {};

    function make_transport(obj){
        var handlers = {
            onopen_handler_list: [],
            onclose_handler_list: [],
            onmessage_handler_list:[],
            onerror_handler_list:[]};

        obj.onopen = function(fun){handlers.onopen_handler_list.push(fun);};
        obj.onclose = function(fun){handlers.onclose_handler_list.push(fun);};
        obj.onmessage = function(fun){handlers.onmessage_handler_list.push(fun);};
        obj.onerror = function(fun){handlers.onerror_handler_list.push(fun);};

        obj.trigger_message = function(data){
            handlers.onmessage_handler_list.forEach(
                function(fun){fun(data);});
        };
        obj.trigger_onopen = function(){
            handlers.onopen_handler_list.forEach(
                function(fun){fun();});
        };
        obj.trigger_onclose = function(){
            handlers.onclose_handler_list.forEach(
                function(fun){fun();});
        };
        obj.trigger_error = function(data){
            handlers.onerror_handler_list.forEach(
                function(fun){fun(data);});
        };
    }

    ////////////////////
    // Http Transport //
    ////////////////////
    publicator.transports.http =
        function(host, session_id){
            // if host is same, send with ajax
            // otherwise send with jsonp
            var http_send = publicator.send_jsonp;
            if(document.location.host == host)
                http_send = publicator.send_ajax;

            var url = (is_secure()?'https://':'http://') + host + '/' +
                    session_id + '/http/';

            var transport = {
                hearthbeat: 1000,
                send: function(obj){
                    var callback = function(result){
                        console.log('Http_on_message_handler', result);
                        transport.trigger_message(result);
                    };
                    http_send(url, obj, callback);
                }
            };//transport
            make_transport(transport);
            var first_run = true;
            function poll_messages(){
                if(first_run){
                    transport.trigger_onopen();
                }
                first_run = false;
                http_send(url, {type:'get_messages'},function(data){
                    data.forEach(function(msg){transport.trigger_message(msg);});
                    setTimeout(poll_messages, transport.hearthbeat);    
                });
                
            }

            setTimeout(poll_messages, transport.hearthbeat);
            
            return transport;
        };
    /////////////////////////
    // Websocket Transport //
    /////////////////////////
    publicator.transports.websocket =
        function(host, session_id){
            var transport = {
                send: function(obj){
                    var json_string = JSON.stringify(obj);
                    transport.websocket.send(json_string);
                }
            };//transport

            make_transport(transport);
            var url = (is_secure()?'wss://':'ws://') + host + '/' +
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
