$ git clone

$ make

$ make run

To do once
(news@127.0.0.1)1> create_tables:init().

Add news
$ curl -X POST -H "Accept: application/json" -d '{"content":"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" /><title>Apache2 Debian Default Page: It works</title></head><body><p>Firts</p><p>Second</p></body></html>", "title":"Title1"}' http://localhost:8008/news

Returns:
{"id":"$id","create_time":"$date $time UTC"} | {"error":"$reason"}


View news list
$ curl -X GET -H "Accept: application/json" ttp://localhost:8008/news

Returns:
[{"id":"$id", "update_time":"$utime", "title":"$title"}]


View some news bi $id
$ curl -X GET -H "Accept: application/json" ttp://localhost:8008/news/$id

Returns:
{"id":"$id", "create_time":"$ctime", "update_time":"$utime", "title":"$title", "context":"$context"} | {"error":"$reason"}


Update news by $id
$ curl -X PUT -H "Accept: application/json" -d '{"title":"Title2"}' http://localhost:8008/news/$id

Returns:
{"success":"updated"} | {"error":reason"}

Delete new by $id
$ curl -X DELETE -H "Accept: application/json" ttp://localhost:8008/news/$id
Returns:
{"success":"deleted"} | {"error":reason"}
