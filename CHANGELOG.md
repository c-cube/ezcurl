## 0.2.4

- fix: global initialization logic is now hidden behind a mutex
  * depend on `thread`

## 0.2.3

- fix: workaround servers which do not understand "Expect" header
- fix: correctly set size of payload for POST
- make sure to setup 'PUT" correctly
- allow POST with non-form data

## 0.2.2

- fix: do not reset client if passed as argument

## 0.2.1

- fix setting of headers

## 0.2

- add default user agent

## 0.1

- initial release
