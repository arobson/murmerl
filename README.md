## Murmerl
An Erlang NIF wrapper for the native Murmurhash3 library. Why? Because murmurhash3 is crazy fast and relatively low collision. I am working on a consistent hashing implementation and this should improve the throughput.

**NOTE** This is the result of an few hours of hacking. It's not stable.

## Build

	make

or

	rebar compile

## API

** This is the only call that currently works **

hash_x86_32( Key ) -> integer
	Types:

		Key = binary | atom | list


** The 128 bit calls cause a seg fault. Because I'm a n00b.

hash_x86_128( Key ) -> integer
	Types:

		Key = binary | atom | list


hash_x64_128( Key ) -> integer
	Types:

		Key = binary | atom | list