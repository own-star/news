$ git clone

$ make

$ make run

To do once
(news@127.0.0.1)1> create_tables:init().


GET /news - Returns the list of news
GET /news/$id - Retrieves an individual news
POST /news - Creates news
PUT /news/$id - Update (or create) news
DELETE /news/$id - Deletes news



Add news
curl -X POST -H "Accept: application/json" -d '{"content":"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/><title>Hello</title></head><body>Hello World!</body></html>", "title":"Hello"}' http://localhost:8008/news


Returns:
{"id":"$id","date":"$date $time UTC"} | {"error":"$reason"}


View news list
$ curl -X GET -H "Accept: application/json" http://localhost:8008/news

Returns:
[{"id":"$id", "date":"$utime", "title":"$title"}]


View some news bi $id
$ curl -X GET -H "Accept: application/json" http://localhost:8008/news/$id

Returns:
{"id":"$id", "date":"$utime", "title":"$title", "context":"$context"} | {"error":"$reason"}


Update news by $id
curl -X POST -H "Accept: application/json" -d '{"content":"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/><title>New WEB Title</title></head><body>New Content</body></html>", "title":"NewTitle"}' http://localhost:8008/news


Returns:
{"success":"updated"} | {"error":reason"}

Delete new by $id
$ curl -X DELETE -H "Accept: application/json" http://localhost:8008/news/$id
Returns:
{"success":"deleted"} | {"error":reason"}
