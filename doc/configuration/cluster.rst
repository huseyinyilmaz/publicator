Clustering:
-----------

Publicator can run as a cluster. Creating a cluster is pretty easy. You can just use '/bin/publicator connect' command to connect to a node. When you connect to a node, You will automatically connect to othernodes in the cluster. So connecting to only one node will be enough.

.. code-block:: bash

   $ bin/publicator node   # return current node name
   $ bin/publicator nodes  # return connected node list
   $ bin/publicator connect other_publicator_node@127.0.0.1
   # Connect to cluster that node named other_publicator_node@127.0.0.1 belongs to.
