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
