# MQTT API

## Login

The client connects with username field with the user's id and the password field containing a JWT token.

### Required actions on login

### Listen on your own channel:
Subscribe to the topic `"users/<user id>"` where `<user id>` is your user id.

### Notify others of your online presence
Publish a message to `"system/online/<user id>"` with the payload `1` and retain set to `true` as well as a Last Will Telegram with the payload `0` and no retain flag where `<user id>` is your user id.

## Start a new thread

### Topic: "system/newthread"

Publish a message with the users to start the thread with.

```json
{
    "users": [
        "<user id>",
        ...
    ]
}
```

This will publish messages to the listed users (as well as the creator) own channels' with the format:
```json
{
    "type": "thread created",
    "thread": "<thread id>"
}
```
with the newly created thread id.