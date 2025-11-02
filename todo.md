
* add textures
* convert uses of array to vector package
* remove binary and bytestring for template haskell dependency
* make juicypixels and Texture module optional
* add render to texture
* add other glsl functions (e.g. matrix ops)
* add function to cover renders over other renders through masks
* add flexible parameters to compile

* write optimizer to turn multiused asts into shared variables
* maybe break vectors back down to Expr e (V4 Float) and provide a means to access the floats
* integrate Data.Bits

* extending tuple instances
* integrate Data.Boolean (boolean package)

* have GArray clear itself up on losing reference
  * move garray content into a ioref and add finalizer
* possibly clean up everything else too.
* be reminded that shader programs running inside another,
  will have to be independent of the inner postshader monad









