Using publicator from http interface
------------------------------------

If You need to connect publicator from an interface that is not provided. You can roll your own with simple http interface of publicator.
First thing you should do to use publictor is to get a session id from the system. You can get your session id from session/

.. code-block:: bash

   $ curl --request POST localhost:8766/session/ --include --data "{\"some_auth_data\": \"bla\"}"
   HTTP/1.1 200 OK
   connection: keep-alive
   server: Cowboy
   date: Mon, 24 Feb 2014 09:49:16 GMT
   content-length: 42
   content-type: application/json; charset=utf-8

   {"type":"session_created","data":"session123"}

Here, We send a post request to server. Body of the post request is a json that will be send to authentication backend as a "auth_info".

In result, You get a session id "session123" from the system. System will recognize You with this session id and associates your subscribed channels to You.

To subscribe a channel:

.. code-block:: bash

   $ curl --request POST http://localhost:8766/$SESSION_ID/subscribtions/$CHANNEL_CODE/ \
   --include \
   --header "Content-Type: application/json"

   HTTP/1.1 204 No Content
   connection: keep-alive
   server: Cowboy
   date: Sun, 29 Sep 2013 10:29:07 GMT
   content-length: 0
   content-type: text/html


To unscribe a channel:

.. code-block:: bash

   $ curl --request DELETE http://localhost:8766/$SESSION_ID/subscribtions/$CHANNEL_CODE/ \
   --include \
   --header "Content-Type: application/json"

   HTTP/1.1 204 No Content
   connection: keep-alive
   server: Cowboy
   date: Sun, 29 Sep 2013 10:43:00 GMT
   content-length: 0
   content-type: text/html

To send a message to a channel:

.. code-block:: bash

   $ curl --request POST http://localhost:8766/$SESSION_ID/messages/$CHANNEL_CODE/ \
   --include \
   --header "Content-Type: application/json" \
   --data "message=Message1"

   HTTP/1.1 204 No Content
   connection: keep-alive
   server: Cowboy
   date: Sun, 29 Sep 2013 10:47:38 GMT
   content-length: 0
   content-type: text/html

To check for incoming messages that is coming from your subscribed channels:

.. code-block:: bash

   $ curl --request GET http://localhost:8766/$SESSION_ID2/messages/ \
   --include \
   --header "Content-Type: application/json"

   HTTP/1.1 200 OK
   connection: keep-alive
   server: Cowboy
   date: Sun, 29 Sep 2013 10:48:46 GMT
   content-length: 25
   content-type: text/plain
   vary: accept

   {"channel1":["Message1"]}

Please beware that message publishers do not receive messages they sent. Thats why in this example we are receiving messages from different session id. Format of message url is channel_code to message list mapping. for instance

.. code-block:: bash

   {"channel_name1": ["msg1", "msg2",......],
    "channel_name2": ["msg3", "msg4",......],
    .....
   }
