# mss
MS Storage Server

# Build

* Install OTP 19.2 and ensure it is in your `PATH`

```
make
```

You may get this error, I don't yet know the cause:

```
===> Failed to solve release:
 Dependency mss is specified as a dependency but is not reachable by the system.
```

If so, the following should work:

```
make distclean; make
```

# Run

```
make run
```

# Package

```
make pkg
```

The `mss.tgz` archive can be extracted and run using `challenge/bin/challenge-executable`. It will create a directory `blobs` in the same directory as `challenge` where it writes blob data.

# Design

* `mss_sup` - main supervisor for `blob_mgr_sup` and `blob_proc_svr`
* `blob_proc_svr` - process that keeps track of what `blob_mgr` process manages a certain blob ID
* `blob_mgr_sup` - supervisor for `blob_mgr` processes.
* `blob_mgr` - `gen_server` that serializes operations for blobs on a per-ID basis.
* `blob` - module that grabs a `blob_mgr` by PID and executes the blob operation using it.
* `store_handler` - `cowboy` handlers for REST operations.
