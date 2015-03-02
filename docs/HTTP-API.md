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
        "users": [{
            "_id": "<user id>",
            "name": "<user name>"
            },
            ...
        ]
    },
    ...
    ]
}
```

### GET /threads/:threadid/messages?since=:messageid

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
    ]
}
```
