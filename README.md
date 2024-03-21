# Dumy Web Server

## Usage

Create a configuration file

```javascript
{
    "/my/path/:wildcard": {
        "status": 500, // response status
        "delay": 10000000, // artificial delay in microseconds
        "body": { /* ... */ } // JSON response body
        "headers": { /* ... */ } // additional response headers
    }
}
```

Run server with configuration

```sh
dummy-web-server config.json
```
