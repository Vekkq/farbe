
* remove binary and bytestring for template haskell dependency?
* make juicypixels and module optional
* add other glsl functions (e.g. matrix ops, boolean)
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

* make eglMakeCurrent available through the outside - this function is for drawing without display

* skip the multithreads. make it work for one thread, have heavy gl work when swapbuffers is running.
* compile function is entirely run in gl backlog.
	* this makes it possible that expr monads on shaders can take all their time needed for loading external data.
	* alternative is to extend the shader monad for access to the backlogger. with heavy work it is the preferred method
	* give the work monad a ReaderT (m Double) for time left for processing
* add function for filtering for pressed keys in events

info:
new framebuffers need a depth buffer in order to render in respect to depth.
renderbuffers are for when you do need depth or stencil, but without directly accessing them. rendering color will access depth and depending on settings also stencil.
stencil settings need to be reset, directly after use or it messes up the following frames. glStencilOp says how stencil is written. glStencilFunc says how stencil is used. if GL_REPLACE, glStencilFunc also writes.
