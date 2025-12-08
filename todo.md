
* add texture atlas
* remove binary and bytestring for template haskell dependency?
* make juicypixels and module optional
* add render to texture
* add other glsl functions (e.g. matrix ops)
* add function to cover renders over other renders through masks
* add flexible parameters to compile

* write optimizer to turn multiused asts into shared variables
* integrate Data.Bits operations as Expr

* extending tuple instances
* integrate Data.Boolean (boolean package)

* have GArray clear itself up on losing reference
  * move garray content into a ioref and add finalizer
* possibly clean up everything else too.
* be reminded that shader programs running inside another,
  will have to be independent of the inner postshader monad

* rewrite window to track pressed keys, by ensuring which keys are pressed by asking all after tabbing back in

* rewrite VArray to delete itself after losing reference
* rewrite shaders to delete themself




