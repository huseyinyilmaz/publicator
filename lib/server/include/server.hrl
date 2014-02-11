-type code()::binary().
-type channel_handler_type()::message_only | all.
-type interface_type()::all|atom().
-type permission_type()::{Channel_code::binary()|all,
                          Dimension::interface_type(),
                          Publish::boolean(),
                          Subscribe::boolean(),
                          Can_create::boolean(),
                          Listen_all_room_events::boolean()}.


-type message_type():: message|cached_message|add_subscribtion|remove_subscribtion.

-record(message, {type :: message_type(),
                  data :: binary(),
                  channel_code :: binary()}).
