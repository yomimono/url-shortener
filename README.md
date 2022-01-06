# what is this?

This is an example project demonstrating the use of [chamelon](https://github.com/yomimono/chamelon) as a storage module in a [MirageOS](https://github.com/mirage/mirage) unikernel. It is a simple web application that accepts a path and a URL, and redirects any requests to itself at a path to that URL.

There are a few hardcoded special paths which the webapp doesn't accept as keys, and on lookup will serve hardcoded content.

## layout

* `config.ml` specifies information for which devices, software, and additional information the unikernel needs.
* `le.ml` is logic for getting certificates via Let's Encrypt at server start and after the expiration period.
* `shortener.ml` is the unikernel start point and web application logic.
* `justfile` contains commands I use frequently to set up the backing store, set up bridge networking, and start the unikernel.
