General
=======
jsxd is a library to sanely work with nested data structures as [jsx](https://github.com/talentdeficit/jsx) produces. The data structures can represent objects (key value lists) or arrays (lists).


index

* General
  - [Keys](#Keys)
* Functions
  - [get/2](#get-2)
  - [set/3](#set-3)
  - [delete/2](#delete-2)
  - [update/3](#update-3)
  - [map/3](#map-3)
  - [reduce/3](#reduce-3)



Keys
----
Keys is the abstraction over the Path in the nested structure that jsxd uses to address a value, keys can be:
* a `integer` in the case of array addressing **(be aware that we start at 0!)**
* a `binary` in case of object addressing
* a list of `keys` when addressing a nested value

get/2
------------

jsxd exports  get/2 method to read data the parameters passed are a key or list of keys, it either returns `{ok, <value>}` or `not_found`.

```erlang
Object = [{<<"a">>, 1}, {<<"b">>, [10, 20, 30]}].
{ok, 1} = jsxd:get(<<"a">>, Object).
{ok, 20} = jsxd:get([<<"b">>, 1], Object).
not_found = jsxd:get([<<"b">>, 1,2], Object).
```

set/3
------------

Writing data works using the set/3 method, it follows the same rules for keys as get/2 does, in addition it will create nested objects based on the Path on the following rules:

* If the key is a non existing object `integer` it will create an array.
* If the key is a non existing object `binary` it will create an object.
* If a array index is given that isn't existing yet it will padded with `null` values.

```erlang
[null, <<"value">>] = jsxd:set(1, <<"value">>, []).
[null, [{<<"key">>, <<"value">>}]] = jsxd:set([1, <<"key">>], <<"value">>, []).
[{<<"a">>, 1}, {<<"b">>, [10, 99, 30]}] = jsxd:set([<<"b">>, 1], 99, [{<<"a">>, 1}, {<<"b">>, [10, 20, 30]}]).
```

delete/2
--------
This function simply deletes data from an object

update/3
-------------
Updating works the same as setting data with the difference that instead of a new value a funciton is defined. You can add a default value that gets set if the key is not found otherwise the object is returned unchanged.

```erlang
[{<<"a">>, 1}, {<<"b">>, [10, 21, 30]}] = jsxd:update([<<"b">>, 1], fun(X) -> X+1 end, [{<<"a">>, 1}, {<<"b">>, [10, 20, 30]}]).

[{<<"a">>, 1}, {<<"b">>, [10, 20, 30]}] = jsxd:update([<<"b">>, 3], fun(X) -> X+1 end, [{<<"a">>, 1}, {<<"b">>, [10, 20, 30]}]).

[{<<"a">>, 1}, {<<"b">>, [10, 21, 30]}] = jsxd:update([<<"b">>, 1], fun(X) -> X+1 end, [{<<"a">>, 1}, {<<"b">>, [10, 20, 30]}]).
```

map/3
-----
Maps over an object. The mapping function gets two values, the first being the key (or index) and the second the value. Returned is the new value.

reduce/4
--------
Reduces over an object, in the way `foldl` or `foldr` does, it's called reduced since order isn't guaranteed in objects, internally so it uses foldl. The reduce function gets three arguments, the key or index, the value and the accumulator, it returns a new accumulator.