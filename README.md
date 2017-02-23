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
