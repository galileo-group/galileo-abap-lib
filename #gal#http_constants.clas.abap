class /GAL/HTTP_CONSTANTS definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  constants CONTENT_TYPE_BINARY type STRING value `application/octet-stream`. "#EC NOTEXT
  constants CONTENT_TYPE_JSON type STRING value `application/json`. "#EC NOTEXT
  constants METHOD_DELETE type STRING value `DELETE`. "#EC NOTEXT
  constants METHOD_GET type STRING value IF_HTTP_REQUEST=>CO_REQUEST_METHOD_GET. "#EC NOTEXT
  constants METHOD_POST type STRING value IF_HTTP_REQUEST=>CO_REQUEST_METHOD_POST. "#EC NOTEXT
  constants METHOD_PUT type STRING value `PUT`. "#EC NOTEXT
protected section.
private section.
ENDCLASS.



CLASS /GAL/HTTP_CONSTANTS IMPLEMENTATION.
ENDCLASS.
