$(function(){
    window.enable_logging = true;
    //////////////////////
    // Create namespace //
    //////////////////////
    var publicatorApp = {
	session_id: '',
	client: null,
	subscribe:function(channel_code){},
	unsubscribe:function(channel_code){},
	send_message: function(channel_code,msg){},
	create_session: function(){return 1},
	connect:function(session_id){
	    this.log('initialize system with session id -', session_id);
	    var client = publicator.get_client(session_id);
	    this.client = client;
	    this.session_id = session_id;
	    
	    client.onheartbeat(_.bind(function(){
		this.log('on hearthbeat');
	    }, this));
	    
	    client.onmessage(_.bind(function(e){
		this.log('response',e);
		switch(e.type){
		case 'heartbeat':
		    break;
		case 'subscribtions':
		    _.each(e.data, function(code){
			publicatorApp.channels.add({id:code,
						    code:code},
						   {merge:true});
		    });
		    break;
		};//end switch
	    }, this));
	    
	    client.onopen(_.bind(function(){
		this.log('onopen');
		this.render_connected();
		// client.subscribe('channel_a');
		client.get_subscribtions();
		// client.get_subscribtions();
		// client.publish('channel_a', 'sample message text');
	    },this));

	    client.ondisconnect(_.bind(function(){
		this.log('ondisconnect');
		this.render_disconnected();
	    },this));

	    
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
	},
	render_connected:function(){
	    $('#connection_panel').html(this.connected_template_text);
	},
	render_add_channel:function(){
	    $('#add_channel_panel').html(this.add_channel_template_text);
	    $('#add_channel_button').click(_.bind(this.render_add_channel_edit, this));
	},
	render_add_channel_edit:function(){
	    $('#add_channel_panel').html(this.add_channel_edit_template_text);
	    $('#add_channel_cancel_button').click(_.bind(this.render_add_channel, this));
	    function add_channel(){
		this.client.subscribe($('#add_channel_input').val());
		this.render_add_channel();
		this.client.get_subscribtions();
	    };
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
    var Message = Backbone.Model.extend({});
    var MessageCollection = Backbone.Collection.extend({model: Message});

    var Channel = Backbone.Model.extend({
	defaults: function(e){
	    return {is_active:false,
		    messages: new MessageCollection()};
	}
    });
    var ChannelCollection = Backbone.Collection.extend({model: Channel});


    
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
				 is_active:function(){return this.get('is_active')},
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
				       // XXX set Message view collection
				       $("#channel_name_panel").html(
					   Mustache.render(channel_name_template_text,
							   {code:ch.get('code')}));
				   }
				   e.preventDefault();
			       }
			   )
		       });
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
			publicatorApp.log('response new session - ', session_id)
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
    // load system
    publicatorApp.router = new publicatorApp.Router();
    Backbone.history.start();

    publicatorApp.channels = new ChannelCollection();
    
    
    publicatorApp.channelsView = new publicatorApp.ChannelsView({
	collection: publicatorApp.channels,
	el: '#channels_container'
    });



    
    // connect main app object to global namespace
    window.publicatorApp = publicatorApp;



    
    function send_message(){
	var main_input = $('#main_input');
	chatClient.send_message({type: 'message',
				 value:main_input.val()});
	main_input.val('');}
    
    $('#send_button').click(send_message);
    $('#main_input').keypress(function(e){
	var k = e.which || e.keyCode;
	if(e.type=='keypress' && k==13)
	    send_message();
    });

});
