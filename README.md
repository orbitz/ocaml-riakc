Protobufs Riak client in Ocaml

This client depends on Jane St Core and uses Async for the communication layer.
There are examples of using each API command in the example directory.  There is
also a minor test suite in the tests directory.

The two API entry points are Riakc.Conn and Riakc.Robj.

#### Known Issues

https://github.com/orbitz/ocaml-riakc/issues/

### Release Notes

#### 2.0.0

* Add support for setting 2i in an object.  Indices and Usermeta data types have
  been renamed.  Querying by 2i is not yet supporrted.

#### 1.0.0

* Requires Core >= 109.12.00 due to Async change and Ocaml >= 4.00

* Links are now supported in GET/PUT

* A note on the version number - this only represents that this release is not
  backwards compatible with 0.0.0, not some new level of stability.

#### 0.0.0

* Supports the following methods
    * ping
    * client_id
    * server_info
    * list_buckets
    * list_keys
    * bucket_props
    * get
    * put
    * delete
