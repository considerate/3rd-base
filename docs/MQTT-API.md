# MQTT API

## Login

The client connects with username field with the user's id and the password field containing a JWT token.

### Required actions on login

### Subscibe to these topics:

- `"users/<me>/newthread"`
- `"users/<me>/newfriend"`
- `"online/<friend>"` for all friends

### Notify others of your online presence
Publish a message to `"online/<user id>"` with the payload `1` and retain set to `true` as well as a Last Will Telegram with the payload `0` and no retain flag where `<user id>` is your user id.

# users/:me/newthread
When a new thread is created that you are participating in you will recieve a message with the following format:
```json
{
    "_id": "<thread id>"
}
```

Creation of threads is done over HTTP.

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