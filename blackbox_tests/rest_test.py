import logging
import requests
import json
from itertools import chain
from time import sleep

logger = logging.getLogger(__name__)

HOST = 'http://localhost:8766'


def get_session():
    url = HOST + '/session/'
    resp_obj = requests.get(url)
    assert resp_obj.ok, 'get_session did not finish successfully'
    response = json.loads(resp_obj.text)
    return response['session']


def subscribe(consumer_code, channel_code):
    url = HOST + '/' + consumer_code + '/subscribtions/' + channel_code + '/'
    headers = {'content-type': 'application/json'}
    response = requests.post(url, headers=headers)
    assert response.ok, 'Subscribtions were not successfull'
    print 'subscribe %s' % url


def publish(consumer_code, channel_code, message):
    url = HOST + '/' + consumer_code + '/messages/' + channel_code + '/'
    headers = {'content-type': 'application/json'}
    payload = {'message': message}
    response = requests.post(url, headers=headers, data=payload)
    assert response.ok, 'message sending were not successfull'
    print 'publish message %s=>%s (message=%s)' % (consumer_code,
                                                   channel_code,
                                                   message)


def get_messages(consumer_code):
    url = HOST + '/' + consumer_code + '/messages/'
    response = requests.get(url)
    assert response.ok, 'Could not get back messages'
    return json.loads(response.text)


def iter_messages(message_dict):
    for consumer_dict in message_dict.values():
        for message_set in consumer_dict.values():
            for item in message_set:
                yield item


def get_all_consumer_messages(message_dict):
    while next(iter_messages(message_dict), False):
        sleep(1)
        print "sleeping for 1 sec"
        print "value = %s" % [next(iter_messages(message_dict), False)]

        for consumer_code in message_dict.keys():
            received_messages = get_messages(consumer_code)
            for new_channel_code, new_message_list \
                    in received_messages.items():
                print '%s messages received' % [len(new_message_list)]
                message_dict[consumer_code][new_channel_code].difference_update(new_message_list)


def create_channel(channel_count, consumer_count):
    channel_code_list = ['channel_' + str(i) for i in range(channel_count)]
    consumer_code_list = [get_session() for i in range(consumer_count)]
    # subscribe to whole
    for ch in channel_code_list:
        for co in consumer_code_list:
            subscribe(co, ch)
    sleep(2)
    for ch in channel_code_list:
        for co in consumer_code_list:
            publish(co, ch, co)

    #  expected messages = {consumer_code => {channel_code=> {message}}}
    expected_messages = {}
    for co in consumer_code_list:
        consumer_dict = expected_messages.setdefault(co, {})
        for ch in channel_code_list:
            consumer_dict[ch] = {code
                                 for code in consumer_code_list if code != co}
    get_all_consumer_messages(expected_messages)
    print 'got whole messsages back'

if __name__ == '__main__':
    print "test"
    create_channel(50, 100)
