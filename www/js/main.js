//client debug code
var host = location.host;
window.clients = [];


function parse_session_data(session_id){
    var client = publicator.get_client(session_id);
    client.onheartbeat(function(){
	console.log('on hearthbeat');
    });
    client.onmessage(function(e){
	console.log('response',e);
    });
    client.onopen(function(){
	console.log('onopen');
	client.subscribe('channel_a');
	client.get_subscribtions();
	client.get_subscribtions();
	client.publish('channel_a', 'sample message text');
    });
    // client.send_message(1);
    window.clients.push(client)
    console.log(client);
}


publicator.get_session_id(parse_session_data);
publicator.get_session_id(parse_session_data);
publicator.get_session_id(parse_session_data);

//end of client debug code

$(function(){
    return 1;
    window.enable_logging = true;
    //////////////////////
    // Create namespace //
    //////////////////////
    var publicatorApp = {
	session_id: '',
	subscribe:function(channel_code){},
	unsubscribe:function(channel_code){},
    };

    window.publicatorApp = publicatorApp;

    ////////////
    // Models //
    ////////////
    var Channel = Backbone.Model.extend({});
    var ChannelCollection = Backbone.Collection.extend({model: Channel});

    var Message = Backbone.Model.extend({});
    var MessageCollection = Backbone.Collection.extend({model: Message});

    
    publicatorApp.channels = new ChannelCollection();


    ///////////
    // Views //
    ///////////

    publicatorApp.UsersView = Backbone.View.extend({
	initialize: function(options){
	    console.log(this)
	    _.bindAll(this, 'render' , 'initialize');
	    this.collection.bind('add', this.render);
	    this.collection.bind('remove', this.render);
	    this.collection.bind('change', this.render);
	    this.template_text = $('#channels_template').html();
	},
	render: function(){
	    this.$el.html(
		Mustache.render(this.template_text,
				{collection:this.collection,
				 code: function(){return this.get('code');}})
	    )}
    });

    publicatorApp.usersView = new publicatorApp.UsersView({
	collection: publicatorApp.channels,
	el: '#channels_container'
    });
						   
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

						   alert(1);
					     
					     
});
