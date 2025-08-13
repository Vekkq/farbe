

* have GArray clear itself up on losing reference
  * move garray content into a ioref and add finalizer
* possibly clean up everything else too.
* be reminded that shader programs running inside another,
  will have to be independent of the inner postshader monad









