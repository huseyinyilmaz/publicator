

::

    ________       ______ __________             _____              
    ___  __ \___  ____  /____  /__(_)___________ __  /______________
    __  /_/ /  / / /_  __ \_  /__  /_  ___/  __ `/  __/  __ \_  ___/
    _  ____// /_/ /_  /_/ /  / _  / / /__ / /_/ // /_ / /_/ /  /    
    /_/     \__,_/ /_.___//_/  /_/  \___/ \__,_/ \__/ \____//_/     


----
   
|build|_

in-memory pub-sub server that works on http-rest interface.

How to build a release
----------------------
To use publicator in a production system, you should package it. Your package will include all dependencies including erlang runtime itself. So you won't need erlang installed on your production system in order to run it. To generate a package first download source code from github.

::

   $ git clone git://github.com/huseyinyilmaz/publicator.git

Now you can generate a package for your system.

::
   $ make configure
   $ make

publicator.tar.gz will be generated in project root directory (current directory). You can untar it anywhere in your target system and run it with 

::

   $ bin/publicator start


How to configure for development
---------------------------------

If you want to checkout the code you can configure the system for development.

::

   $ make configure-dev

This will download development dependencies. After this you can start server in development mode with

::

   $ make start   

You can also run eunit tests with the following.

::

   $ make test

If you do not want to full compile before starting to test use following

::

   $ make eunit
   

Usage
-----

In order to use publicator you should add a publicator-session-id cookie to your requests. That way publicator will identify and associates you with your subscribed channels.

To subscribe a channel:

::

   $ curl --request POST http://localhost:8766/subscribtions/$CHANNEL_CODE/ \
	--include \
	--header "Content-Type:application/json" \
	--cookie "publicator-session-id=$SESSION_ID"

To unscribe a channel:

::

    $ curl --request DELETE http://localhost:8766/subscribtions/$CHANNEL_CODE/ \
	--include \
	--header "Content-Type:application/json" \
	--cookie "publicator-session-id=$SESSION_ID"

To check for incoming messages that is coming from your subscribed channels:

::

    curl --request GET http://localhost:8766/messages/ \
	--include \
	--header "Content-Type:application/json" \
	--cookie "publicator-session-id=$SESSION_ID"

To send a message to a channel:

::

    curl --request POST http://localhost:8766/messages/$CHANNEL_CODE/ \
	--include \
	--header "Content-Type:application/json" \
	--cookie "publicator-session-id=$CUSTOM_SESSION_ID" \
	--data "message=$MESSAGE"

Please beware that message senders do not receive messages they sent.

.. |build| image:: https://travis-ci.org/huseyinyilmaz/publicator.png
.. _build: https://travis-ci.org/huseyinyilmaz/publicator
