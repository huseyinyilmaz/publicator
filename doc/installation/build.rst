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
