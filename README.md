Protobufs Riak client in Ocaml

This client depends on Jane St Core and uses Async for the communication layer.
There are examples of using each API command in the example directory.  There is
also a minor test suite in the tests directory.

The two API entry points are Riakc.Conn and Riakc.Robj.

#### Known Issues

* Not heavily tested - test suite will be expanded

* Only supports GET/PUT/DELETE and a few other operations

### Release Notes

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
