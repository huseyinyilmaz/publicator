How to run for development
--------------------------

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

To do dialyzer checks first you have to create plt files. Because plt files takes to much times to generate, plt file generation seperated two different parts.

First of all you have to generate plt files for erlang standard library


.. code-block:: bash
