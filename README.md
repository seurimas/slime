[Docs](http://package.elm-lang.org/packages/seurimas/slime/latest).

# slime
An ECS library for Elm. Slime provides an easy way to build a full game, using common Entity-Component-System architecture.


# Examples
Pong is implemented in the examples directory. [Link](https://seurimas.github.io/slime/examples/pong/)

To run locally:

```
git clone https://github.com/Seurimas/slime.git
cd slime/examples/pong
elm reactor

... Open http://localhost:8000/src/pong.elm ...
```

# Change log
* 2.0.0 -> 2.1.0
  * Added `componentSpec` for a type-ignorant constructor. Use that instead of { getter, setter } records.
  * Added `stepEntitiesAndThen`, for steppers who want to have side effects on the world. This won't work for every case, but it will cover a lot.
  * Added `forNewEntities`, for spawning several entities with particular parameters.
* 1.1.0 -> 2.0.0
	* Revise system creation. Use `untimedSystem`, `timedSystem`, and `systemWith` in Slime.Engine.
	* Revise steppers. Use `stepEntities`, and `entities`/`entities2`/`entities3`.
	* Revise getters. `entities(/2/3)` -> `getEntities(/2/3)` or `(world) &. entities(/2/3)`
	* Improve entity creation API. Use `forNewEntity` and `&=>`.
		* See also: `forEntityById` and `forEntityByUid` for entity updates.

* 1.1.0
	* Initial release.
