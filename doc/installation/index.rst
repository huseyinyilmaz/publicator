Publicator Installation
=======================

For now you can use publicator only downloading the source code and building a release file.

How to build a release
----------------------
To use publicator in a production system, you should package it. Your package will include all dependencies including erlang runtime itself. So you won't need erlang installed on your production system in order to run it. To generate a package first download source code from github.

.. code-block:: bash

   $ git clone git://github.com/huseyinyilmaz/publicator.git

Now you can generate a package for your system.

.. code-block:: bash

   $ make configure
   $ make

publicator.tar.gz will be generated in project root directory (current directory). You can untar it anywhere in your target system and run it with

.. code-block:: bash

   $ bin/publicator start


How to configure for development
--------------------------------

If you want to checkout the code you can configure the system for development.

.. code-block:: bash

   $ make configure
   $ make configure-dev

This will download development dependencies. After this you can start server in development mode with

.. code-block:: bash

   $ make start

You can also run eunit tests with the following.

.. code-block:: bash

   $ make test

If you do not want to full compile before starting to test use following

.. code-block:: bash

   $ make eunit
