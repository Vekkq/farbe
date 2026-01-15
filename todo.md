
* add texture atlas 
	* fixed by GL_UNPACK_ALIGNMENT for npot to work
	* pot texture atlas may now be unnecessary 
			* except for staying below texture limit
			* or maybe alignment
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
* rewrite textures to delete themself - almost done


* add or do timing analyses
* write a variant of deepseq, which will apply deepseq on every parameter of a function
* lift instances from base class and remove unused derived instances from all other classes
* maybe add id's to vertexarrays, to distinguish pager entries from deleted old ones.
* make window not resizable
* consider to apply always `up` to the vertex shader position value
  and add same same instances, like V3 -> V3

* add ShaderM again and replace Defer Shdr

* the gl part should track binds and ignore bind calls when they are not necessary
* the gl part should separate todos in immediate and delayed tasks

* see what the stencil render looks like on a printed out texture via juicypixels

* fix juicypixels module - get some order in there

* enable split sections for ghc in cabal

* ease `compile` to not depend on Defer for making prerender and have the returned monads be separate



info:
new framebuffers need a depth buffer in order to render in respect to depth.

what now? 

