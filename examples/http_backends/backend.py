"""
Sample backend for publicator.
In order to run backend:
$ pip install -r requirements.txt # install requirements
$ python backend.py # start server

You can use this server to check your installation.
change returned value from True to False to deny authentication.

In order to connect your publicator installation to this sample server change
publicator settings to following

{server, [
         {auth_backend, {publicator_http_auth_backend,
                        [{url, "http://127.0.0.1:5000/auth/"}]}},
         {permission_backend,{publicator_static_permission_backend,
                              [[{consumer_code, all},
                                {extra_data, []},
                                {channel_code, all},
                                {can_publish, true},
                                {can_subscribe_messages, true},
                                {can_subscribe_all_events, true},
                                {can_create_channel, true}]]}}
         ]}

As you can see we have to set auth_backend to publicator_http_auth_backend
and give it a connection url.

since flask uses port 5000 as default port we will be using
127.0.0.1:5000/auth/ as auth_backend_url

Here is a sample debug log that this server will print:

-------------------------------------------------------------------------------
DEBUG in backend [backend.py:13]:
raw data = {"consumer_code":"1vo96p","auth_info":"publicator_client.js_test_auth","extra_data":{"host":"localhost:8766","connection":"keep-alive","accept":"text/javascript, application/javascript, application/ecmascript, application/x-ecmascript, */*; q=0.01","x-requested-with":"XMLHttpRequest","user-agent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1700.102 Safari/537.36","referer":"http://localhost:8766/","accept-encoding":"gzip,deflate,sdch","accept-language":"en-US,en;q=0.8,tr;q=0.6"}}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
DEBUG in backend [backend.py:15]:
json_data = {u'extra_data': {u'accept-language': u'en-US,en;q=0.8,tr;q=0.6', u'accept-encoding': u'gzip,deflate,sdch', u'connection': u'keep-alive', u'accept': u'text/javascript, application/javascript, application/ecmascript, application/x-ecmascript, */*; q=0.01', u'user-agent': u'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1700.102 Safari/537.36', u'host': u'localhost:8766', u'referer': u'http://localhost:8766/', u'x-requested-with': u'XMLHttpRequest'}, u'auth_info': u'publicator_client.js_test_auth', u'consumer_code': u'1vo96p'}
--------------------------------------------------------------------------------

Data that send to server has three fields.
1) consumer_code: consumer_code that we are authenticating
2) auth_info: auth_info that client send to server for authentication
3) extra_data: client request headers.
"""
import json
from flask import Flask, request
app = Flask(__name__)


@app.route("/auth/", methods=['POST'])
def authenticate():
    app.logger.debug('raw data = %s', request.data)
    data = json.loads(request.data)
    app.logger.debug('json_data = %s', data)
    return json.dumps({'authenticate': True})   # Allow
    # return json.dumps({'authenticate': False}) # Deny

if __name__ == "__main__":
    app.run(debug=True)
