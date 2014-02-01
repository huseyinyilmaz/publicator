$(function(){
    window.enable_logging = true;
    publicator.set_host('localhost:8766');
    
    //////////////////////
    // Create namespace //
    //////////////////////
    var publicatorApp = {
	session_id: '',
	client: null,
	channels: null,
	subscribe:function(channel_code){},
	unsubscribe:function(channel_code){},
	send_message: function(msg){
            var channel = this.channels.get_active_channel();
            var channel_code = channel.get('code');
            channel.get('messages').log('Message published - ' + msg);
            this.client.publish(channel_code, msg);
	},
	connect:function(session_id){
            this.log('initialize system with session id -', session_id);
            var client = publicator.get_client(
                _.bind(function(client){
                    this.client = client;
                    this.session_id = session_id;

                    client.onmessage(_.bind(function(e){
                        this.log('response',e);
                        switch(e.type){
                        case 'heartbeat':
                            break;
                        case 'subscribtions':
                            _.each(e.data, _.bind(function(code){
                                this.channels.add({id:code,
                                                   code:code},
                                                  {merge:true});
                            }, this));
                    break;
		case 'message':
                            this.log('messages received');
                            var channel = this.channels.get_channel(e.channel_code);
                            this.log(channel);
                            var messages = channel.get('messages');
                            this.log(messages);
                            messages.message(e.data);
                            break;
		case 'error':
                            if(e.data=='invalid_channel_code'){
                                alert("Given chanel name is invalid.");
                            }
                            else{
			        console.error('An Error ocurred on server. Starting a new connection');
			        console.error('error: ' + e.data);
			        publicatorApp.router.start_new_session();
                            }
                            break;

		        }//end switch
                    }, this));

                    client.onopen(_.bind(function(){
		        this.log('onopen');
		        this.render_connected();
		        // client.subscribe('channel_a');
		        client.get_subscribtions();
		        // client.get_subscribtions();
		        // client.publish('channel_a', 'sample message text');
                    },this));

                    client.onclose(_.bind(function(){
		        this.log('onclose');
		        this.render_disconnected();
                    },this));
                    client.onerror(_.bind(function(error){
                        console.error("error", error);
                    },this));
                    
                },this),
                session_id);
	},
	
	log:function(str, obj){
            if(true || enable_logging && console){
		if(obj){
                    console.log(str, obj);
		}else{
                    console.log(str);
		}}
	},
	
	connected_template_text: $('#session_connected_template').html(),
	disconnected_template_text: $('#session_disconnected_template').html(),
	add_channel_template_text: $('#add_channel_template').html(),
	add_channel_edit_template_text: $('#add_channel_edit_template').html(),
	
	render_disconnected:function(){
            $('#connection_panel').html(this.disconnected_template_text);
            $('#send_button').attr('disabled', 'disabled');
            $('#main_input').attr('disabled', 'disabled');

	},
	render_connected:function(){
            $('#connection_panel').html(this.connected_template_text);
            if(this.channels.get_active_channel()){
		$('#send_button').removeAttr('disabled');
		$('#main_input').removeAttr('disabled');}
	},
	render_add_channel:function(){
            $('#add_channel_panel').html(this.add_channel_template_text);
            $('#add_channel_button').click(_.bind(this.render_add_channel_edit, this));
	},
	render_add_channel_edit:function(){
            $('#add_channel_panel').html(this.add_channel_edit_template_text);
            $('#add_channel_cancel_button').click(_.bind(this.render_add_channel, this));
            function add_channel(){
		this.client.subscribe($('#add_channel_input').val(), 'all');
		this.render_add_channel();
		this.client.get_subscribtions();
            }
            $('#add_channel_ok_button').click(_.bind(add_channel, this));
            $('#add_channel_input').keypress(_.bind(function(e){
		var k = e.which || e.keyCode;
		if(e.type=='keypress' && k==13)
                    add_channel.apply(this,[]);
            }, this)).focus();
	}
	
    };


    ////////////
    // Models //
    ////////////
    var Message = Backbone.Model.extend({
	defaults: {type: 'msg'}
    });

    var MessageCollection = Backbone.Collection.extend({
	model: Message,
	message:function(msg){
            this.add({msg: msg, type: 'msg'});
	},
	log:function(msg){
            this.add({msg: msg, type: 'log'});
	},
	error:function(msg){
            this.add({msg: msg, type: 'error'});
	}
    });

    var Channel = Backbone.Model.extend({
	defaults: function(e){
            return {is_active:false,
                    messages: new MessageCollection()};
	}
    });
    var ChannelCollection = Backbone.Collection.extend({
	model: Channel,
	get_active_channel: function(){return this.findWhere({is_active: true});},
	get_channel:function(code){return this.findWhere({code: code});}
});


    
    ///////////
    // Views //
    ///////////
    publicatorApp.ChannelsView = Backbone.View.extend({
	initialize: function(options){
            _.bindAll(this, 'render' , 'initialize');
            this.collection.bind('add', this.render);
            this.collection.bind('remove', this.render);
            this.collection.bind('change', this.render);
            this.template_text = $('#channels_template').html();
            this.channel_name_template_text = $('#channel_name_template').html();
	},
	render: function(){
            var channel_id_prefix = 'channel_list_item_';
            var channel_name_template_text = this.channel_name_template_text;
            var collection = this.collection;
            this.$el.html(
		Mustache.render(this.template_text,
				{collection:this.collection,
                                 code: function(){return this.get('code');},
                                 is_active:function(){return this.get('is_active');},
                                 channel_id_prefix: channel_id_prefix
				}));
		_.each(collection.models,
                       function(ch){
                           $('#' + channel_id_prefix + ch.id).click(
                               function(e){
                                   if(!ch.get('is_active')){
                                       collection.each(
                                           function(e){
                                               e.set('is_active',false,
                                                     {silent: true});});
                                       ch.set('is_active', true);
                                       $('#send_button').removeAttr('disabled');
                                       $('#main_input').removeAttr('disabled');
                                       // XXX set Message view collection
                                       $("#channel_name_panel").html(
                                           Mustache.render(channel_name_template_text,
                                                           {code:ch.get('code')}));
                                       var messageView = publicatorApp.messageView;
                                       messageView.set_collection(ch.get('messages'));
                                   }
                                   e.preventDefault();
                               }
                           );
                       });
        }
    });


    publicatorApp.MessageView = Backbone.View.extend({
	template_text: $('#message_template').html(),
	initialize: function(){
            _.bindAll(this, 'addMessage', 'set_collection');
            this.collection.bind('add', this.addMessage);
	},
	set_collection:function(collection){
            this.collection.unbind('add', this.addMessage);
            this.collection = collection;
            this.collection.bind('add', this.addMessage);
            this.render();
	},
	addMessage: function(model){
            this.$el.append(Mustache.render(
		this.template_text,
		{msg: model.get('msg'),
                 is_msg: function(){return model.get('type') == 'msg';},
                 is_log: function(){return model.get('type') == 'log';},
                 is_error: function(){return model.get('type') == 'error';}
		}));
            // keep the scroll to bottom
            this.$el.stop().animate({
		scrollTop: this.$el[0].scrollHeight
            }, 800);
	},
	render: function(){
            this.$el.html('');
            _.forEach(this.collection.models,
                      this.addMessage);
	}	
    });
    


    ////////////
    // Router //
    ////////////
    publicatorApp.Router = Backbone.Router.extend({
	routes: {
            '': 'start_new_session',
            ':session_id': 'connect_to_session'},
	start_new_session: function(){
            publicatorApp.log('request new session');
            publicator.get_session_id(
		_.bind(
                    function(session_id){
			publicatorApp.log('response new session - ', session_id);
			publicatorApp.router.navigate(session_id);
			this.connect_to_session(session_id);
                    }, this));
	},
	connect_to_session: function(session_id){
            publicatorApp.connect(session_id);
            $("#session_id_span").text(session_id);
	}
    });

    // render page components
    publicatorApp.render_disconnected();
    publicatorApp.render_add_channel();
    //Add main input events
    function send_message(){
	var main_input = $('#main_input');
	var msg = main_input.val();
	publicatorApp.send_message(msg);
	main_input.val('');}
    
    $('#send_button').click(send_message);
    $('#main_input').keypress(function(e){
	var k = e.which || e.keyCode;
	if(e.type=='keypress' && k==13)
            send_message();
    });
    
    // load system
    publicatorApp.router = new publicatorApp.Router();
    Backbone.history.start();

    publicatorApp.channels = new ChannelCollection();
    
    
    publicatorApp.channelsView = new publicatorApp.ChannelsView({
	collection: publicatorApp.channels,
	el: '#channels_container'
    });

    var firstCollection = new MessageCollection();
    
    publicatorApp.messageView = new publicatorApp.MessageView({
	collection: firstCollection,
	el: '#messages_container'
    });

    firstCollection.message('Welcome to publicator client.');
    firstCollection.log('Add a channel and select it to see messages in that channel.');
    
    // connect main app object to global namespace
    window.publicatorApp = publicatorApp;



});
