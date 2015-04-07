# HTTP API

For all routes authentication is handled by tokens.

HTTP Authorization Header has to be set to a JWT token with the format:
```plain
Authorization: Bearer <token>
```
The token has to contain a field with the key `"id"` containing the connecting user's user id.

## Fetch Thread Message History

### GET /user/me/threads

Expected Result:
All threads that you are a part of.

```json
{
    "rows": [{
        "_id": "<thread id>",
        "users": [
        "<user id>"
        ...
        ]
    },
    ...
    ]
}
```

### GET /threads/:threadid
Expected Result:
```json
{
    "_id": "<thread id>",
    "users": [
        "<user id>"
        ...
    ],
    ...
    ]
}
```


### GET /threads/:threadid/messages?after=:messageid

Expected Result:
All messages that have been posted to the given thread since the message of the given messageid. The messages are sorted in chronological order with the oldest message first.

```json
{
    "rows": [{
            "_id": "<message id>",
            "body": "<payload body text>",
            "image": "<image url>"
        },
        ...
    ],
    "links": {
        "before": "/threads/<thread id>/messages?before=<first message id>"
    }
}
```

### GET /threads/:threadid/messages?before=:messageid

Expected Result:
All messages that have been posted to the given thread before the message of the given messageid. The messages are sorted in chronological order with the oldest message first.

```json
{
    "rows": [{
            "_id": "<message id>",
            "body": "<payload body text>",
            "image": "<image url>"
        },
        ...
    ],
    "links": {
        "before": "/threads/<thread id>/messages?before=<first message id>",
        "after": "/threads/<thread id>/messages?after=<last message id>"
    }
}
```

### POST /threads
```json
{
    "users": [
        "<user id>"
        ...
    ]
}
```
Where the list of user contains id:s for both youself and all users you want to talk to. TODO: Maybe implement so that you are always added to list of users if not present.

### POST /threads/:threadid/users

Add users to a given thread.
```json
{
    "users": [
        "<user id>"
        ...
    ]
}
```

### DELETE /threads/:threadid/users/:userid

Remove a user from a given thread.
