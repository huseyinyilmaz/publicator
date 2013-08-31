

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
   


.. |build| image:: https://travis-ci.org/huseyinyilmaz/publicator.png
.. _build: https://travis-ci.org/huseyinyilmaz/publicator
