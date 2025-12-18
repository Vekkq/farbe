
* add texture atlas
* remove binary and bytestring for template haskell dependency?
* make juicypixels and module optional
* add render to texture
* add other glsl functions (e.g. matrix ops, boolean)
* add function to cover renders over other renders through masks
* add variadic var/expr parameters to compile

* write optimizer to turn multiused asts into shared variables
* integrate Data.Bits operations as Expr

* extending tuple instances

* rewrite window to track pressed keys, by ensuring which keys are pressed by asking all after tabbing back in - done, but untested

* rewrite VArray to delete itself after losing reference - done, rewritten to work with threadsafe ogl
* rewrite shaders to delete themself - done
* delete attached shaders with glDeleteShader - done
* rewrite textures to delete themself - done


* add or do timing analyses
* write a variant of deepseq, which will apply deepseq on every parameter of a function
* lift instances from base class and remove unused derived instances from all other classes
* maybe add id's to vertexarrays, to distinguish pager entries from deleted old ones.



