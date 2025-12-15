
* add texture atlas
* remove binary and bytestring for template haskell dependency?
* make juicypixels and module optional
* add render to texture
* add other glsl functions (e.g. matrix ops, boolean)
* add function to cover renders over other renders through masks
* add flexible parameters to compile

* write optimizer to turn multiused asts into shared variables
* integrate Data.Bits operations as Expr

* extending tuple instances

* rewrite window to track pressed keys, by ensuring which keys are pressed by asking all after tabbing back in

* rewrite VArray to delete itself after losing reference - done
* rewrite shaders to delete themself
* rewrite textures to delete themself

* add or do timing analyses
* write a variant of deepseq, which will apply deepseq on every parameter of a function
* lift instances from base class and remove unused derived instances from all other classes
* maybe add id's to vertexarrays, to distinguish pager entries from deleted old ones.
