#!/bin/bash

CHANNEL1=test 
SESSION_ID=$1
CUSTOM_SESSION_ID=CUSTOMSESSIONID
MESSAGE1="message_1_text"
MESSAGE2="message_2_text"
MESSAGE=$MESSAGE1

echo starting test
echo CHANNEL1 = $CHANNEL1
echo SESSION_ID = $SESSION_ID

# if [ "$SESSION_ID" ]; then
#     echo "usage = test.sh <SESSION_ID>"
#     exit 1
# fi
seperator(){
    echo --------------------------
}

list_channels(){
    seperator
    echo list channels
    
    echo curl --request GET http://localhost:8766/subscribtions/ \
	--include \
	--header \"Content-Type:application/json\" \
	--cookie \"publicator-session-id=$SESSION_ID\"

    curl --request GET http://localhost:8766/subscribtions/ \
	--include \
	--header "Content-Type:application/json" \
	--cookie "publicator-session-id=$SESSION_ID"
    echo
    echo
}

subscribe(){
    seperator
    echo subscribe to channel $CHANNEL1

    echo curl --request POST http://localhost:8766/subscribtions/$CHANNEL1/ \
	--include \
	--header \"Content-Type:application/json\" \
	--cookie \"publicator-session-id=$SESSION_ID\"
    
    curl --request POST http://localhost:8766/subscribtions/$CHANNEL1/ \
	--include \
	--header "Content-Type:application/json" \
	--cookie "publicator-session-id=$SESSION_ID"
}

unsubscribe(){
    seperator
    echo unsubscribe from channel
    echo curl --request DELETE http://localhost:8766/subscribtions/$CHANNEL1/ \
	--include \
	--header \"Content-Type:application/json\" \
	--cookie \"publicator-session-id=$SESSION_ID\"
    
    curl --request DELETE http://localhost:8766/subscribtions/$CHANNEL1/ \
	--include \
	--header "Content-Type:application/json" \
	--cookie "publicator-session-id=$SESSION_ID"
}

send_message(){
    seperator
    echo send_message
    echo curl --request POST http://localhost:8766/messages/$CHANNEL1/ \
	--include \
	--header \"Content-Type:application/json\" \
	--cookie \"publicator-session-id=$CUSTOM_SESSION_ID\" \
	--data \"message=$MESSAGE\"

    curl --request POST http://localhost:8766/messages/$CHANNEL1/ \
	--include \
	--header "Content-Type:application/json" \
	--cookie "publicator-session-id=$CUSTOM_SESSION_ID" \
	--data "message=$MESSAGE"

}

receive_messages(){
    seperator
    echo receive_messages
    echo curl --request GET http://localhost:8766/messages/ \
	--include \
	--header \"Content-Type:application/json\" \
	--cookie \"publicator-session-id=$SESSION_ID\"


    curl --request GET http://localhost:8766/messages/ \
	--include \
	--header "Content-Type:application/json" \
	--cookie "publicator-session-id=$SESSION_ID"

}

subscribe
# list_channels

send_message
receive_messages

# unsubscribe
# list_channels

echo done
