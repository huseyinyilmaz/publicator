Using publicator javascript client
----------------------------------

Publicator consumers connects to server with javascript client

In order to connect publicator server through websocket, publicater-client.js library can be used.

**publicator.get_session_id(callback)**

Before using the library, a session should be started on the server. This session will be used to track subscribed channels, received messages etc. In other words, session represents current consumer. Here is a sample code that requests a session from server. When a session_id is returned it will show it to user.

.. code-block:: javascript

   publicator.get_session_id(
      function(session_id){
          alert(session_id);
   });

**publicator.get_client(session_id):**

After getting a session id a client object can be created with get_client method

.. code-block:: javascript

  client = publicator.get_client(session_id);

Returned client object has various methods to work through given session.

Here is some methods on client that can be used to interact with your session.

.. code-block:: javascript

   client.subscribe(channel_code); //subscribes to given channel;
   client.unsubscribe(channel_code); //unsubscribes from given channel;
   client.get_subscribtions(); //returns the list of subscribtions;
   client.publish(channel_code, message); //publishes given message on given channel.

Client also provides some events that you can register so you can get avare of changes on the system. Here are some events that you can register handlers for.

* *client.onopen(function(){alert("opened")}):* This event is fired when a new connection is started between server and client.
* *client.ondisconnect(function(){alert("disconnected")}):* This if fired when connection is lost for various reasons like network problems.
* *onmessage(function(message){alert(message)}):* If a message received to any channel that current consumer is subscribed, this event will be fired with that message. message will have following format {type: "message", data: "msg_text", channel_code: "chn_code"}
* *client.onhearthbeat(function(){alert("hearth beat received")}):* Heartbeats are created in specific intervals. They are usefull if client does not support websockets and server had to connect an alternative way. They are probably be not really use full for clients.
