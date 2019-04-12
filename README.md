## GET /:page

### Captures:

- *page*: result page

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- If you use ?page=1 (`application/json;charset=utf-8`, `application/json`):

    ```javascript
"HELLO, HASKELLER"
    ```

- If you use ?page=2 (`application/json;charset=utf-8`, `application/json`):

    ```javascript
"Hello, haskeller"
    ```

