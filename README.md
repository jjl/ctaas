# Ctries as a Service

This is a toy for experimenting with building concurrent applications in haskell

It implements a simple HTTP service which maps requests onto operations on a map

The requested url forms a key (e.g. /foo). GET looks up the value, POST inserts
a new value and DELETE removes a value.

If a GET is successful it will return a 200 OK and the body will be the key's value
If it is not successful, it will return a 404

A POST uses the body as the new data for the given key. It returns 401 Created

A DELETE returns 204 No Content


