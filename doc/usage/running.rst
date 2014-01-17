Running publicator
==================

After building publicator you will have a file named publicator.tar.gz. extract it anywhere you want by running

.. code-block:: bash

   $ tar -xzvf publicator.tar.gz

After that you can run several commands.

.. code-block:: bash

   $ bin/publicator start # start server
   $ bin/publicator stop # stop server
   $ bin/publicator restart # restart serve
   $ bin/publicator node # get current node name
   $ bin/publicator nodes # get connected node list (for clustering)
   $ bin/publicator connect publicator2@127.0.0.1 # connect to given node's cluster
