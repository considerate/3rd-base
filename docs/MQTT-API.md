# MQTT API

## Login

The client connects with username field with the user's id and the password field containing a JWT token.

### Required actions on login

### Subscribe to these topics:

- `"users/<me>/newthread"`
- `"users/<me>/threadmembers"`
- `"users/<me>/newfriend"`
- `"online/<friend>"` for all friends

### Notify others of your online presence
Publish a message to `"online/<user id>"` with the payload `1` and retain set to `true` 
as well as a Last Will Telegram (what's this?) with the payload `0` and no retain flag where `<user id>` 
is your user id.
TODO: The 'Last will telegram' is what? Can't find it here or in MQTT spec 
TODO: When a client logs out, how should the topic be notified?
TODO: What happens when battery dies, wifi stops working, mobile connection is lost? How is this topic
notified that the user is offline? 

# users/:me/newthread
When a new thread is created that you are participating in you will recieve a message with the following format:
```json
{
    "_id": "<thread id>",
    "users": [
        "<user id>",
        ...
    ]
}
```

Creation of threads is done over HTTP.

## user/:me/threadmembers
When members are added or have left a thread a message will be recieved on this channel.
```json
{
    "_id": "<thread id>",
    "users": [
        "<user id>",
        ...
    ]
}
```

The list of users will always contain all users participating in the group.

## When a new friend is added
Publish a new message to your own channel so that other clients this user is logged in to also can be notified. The format of this message should be:
```json
{
    "_id": "<user id>"
}
```

## Threads

To listen for new messages on a thread subscribe to the topic:
- `"threads/<id>/messages"`

### Publish a message

Messages of the format:
```json
{
    "body": "<body text>",
    "image": "<image url>" 
}
```

May be published to `"threads/<id>/messages"`.
