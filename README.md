todo
=====

An OTP application

Build
-----
    $ rebar3 compile

Start
-----
    $ docker compose up

Using the API
-----
    Using the Postman public API: https://api.postman.com/collections/1312911-166d2fd7-bf5d-4821-bfbb-80e59a88f7eb?access_key=PMAT-01H7T78MRHV05C6B1MZ06DF7FA

    or

    GET
    $ curl -v -X GET -H "Content-Type: application/json" http://localhost:8080/api/todos

    POST
    $ curl -v -X POST -H "Content-Type: application/json" http://localhost:8080/api/todos -d '{ "description": "TODO Item #1" }'

    PATCH. Note: please update the UUID in the url to the one valid for your deployment.
    $ curl -v -X PATCH -H "Content-Type: application/json" http://localhost:8080/api/todos/77890d00-5534-4fe6-9fa0-e93f49bced62 -d '{ "done": true }'

    DELETE. Note: please update the UUID in the url to the one valid for your deployment.
    $ curl -v -X DELETE -H "Content-Type: application/json" http://localhost:8080/api/todos/77890d00-5534-4fe6-9fa0-e93f49bced62

    POST Append to a file
    $ curl -v -X POST -H "Content-Type: application/json" http://localhost:8080/api/file -d '{ "filename": "file.txt", "data": "New string" }'
    Files could be found in `data/files` directory of the project root.
