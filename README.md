# news
Simple REST API

See README.txt

##Install

$ git clone

$ make

$ make run

##Methods

**GET /news** - Returns the list of news<br>
**GET /news/$id** - Retrieves an individual news<br>
**POST /news** - Creates news<br>
**PUT /news/$id** - Update (or create) news<br>
**DELETE /news/$id** - Deletes news


##Usage

Add news<br>
<code>curl -X POST -H "Accept: application/json" -d '{"content":"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/><title>Hello</title></head><body>Hello World!</body></html>", "title":"Hello"}' http://localhost:8008/news</code>

Returns:
{"id":"$id","date":"$date $time UTC"} | {"error":"$reason"}


View news list
<code>$ curl -X GET -H "Accept: application/json" http://localhost:8008/news</code>

Returns:
[{"id":"$id", "date":"$utime", "title":"$title"}]


View some news bi $id
<code>$ curl -X GET -H "Accept: application/json" http://localhost:8008/news/$id</code>

Returns:
{"id":"$id", "date":"$utime", "title":"$title", "context":"$context"} | {"error":"$reason"}


Update news by $id
<code>curl -X POST -H "Accept: application/json" -d '{"content":"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/><title>New WEB Title</title></head><body>New Content</body></html>", "title":"NewTitle"}' http://localhost:8008/news</code>

Returns:
{"success":"updated"} | {"error":reason"}

Delete new by $id
<code>$ curl -X DELETE -H "Accept: application/json" http://localhost:8008/news/$id</code>

Returns:
{"success":"deleted"} | {"error":reason"}
