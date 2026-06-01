
* add other glsl functions (e.g. matrix ops, boolean)
* add variadic var/expr parameters to compile

* write optimizer to turn multiused asts into shared variables
* integrate Data.Bits operations as Expr

* extending tuple instances

* rewrite window to track pressed keys, by ensuring which keys are pressed by asking all after tabbing back in - done, but untested

* rewrite window to start window hidden and show on first render
* as well as hiding the window on exit, before the window closes

* rewrite VArray to delete itself after losing reference - untested
* rewrite shaders to delete themself - done
* delete attached shaders with glDeleteShader - done
* rewrite textures to delete themself - done, need testing

* make window not resizable
* consider to apply always `up` to the vertex shader position value

* the gl part should track binds and ignore bind calls when they are not necessary
	* lots of writing, so uhh

* see what the stencil render looks like on a printed out texture via juicypixels

* enable split sections for ghc in cabal


* make eglMakeCurrent available through the outside - this function is for drawing without display

* add function for filtering for pressed keys in events

* free up the definition for textures, such that more types can be created
	* e.g. with options to have yes/no mipmaps
	* repeating layouts or no

* maybe use HasCallStack for tracking shader definitions


* direct varray file access using paths

* expr variable for screen ratio
* function for defining expr by IO
	* provide expr for access to time, etc

* provide function for videoModeRefreshRate access of glfw
	* use it in Farbe to set work duration
	-> data WorkDuration = Automatic | Manual Int
	-> when automatic, will obtain fps and work duration calculated periodically


* Load STL in background
* Write HandShdr
* Write debugs

* remove mvar from texture function. it has to be used by the time-confusing juice function

* rewrite shaders to record Shdr by name, to run it separately and only once

* try alternative window creation, if creation fails.
	see ContextCreationAPI - GLFW_OSMESA_CONTEXT_API



info:
new framebuffers need a depth buffer in order to render in respect to depth.
renderbuffers are for when you do need depth or stencil, but without directly accessing them. rendering color will access depth and depending on settings also stencil.
stencil settings need to be reset, directly after use or it messes up the following frames. glStencilOp says how stencil is written. glStencilFunc says how stencil is used. if GL_REPLACE, glStencilFunc also writes.

