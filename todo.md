
* add other glsl functions (e.g. matrix ops, boolean)
* write optimizer to turn multiple uses of asts into shared variables
* integrate Data.Bits operations as Expr
* extending tuple instances

* rewrite window to start window hidden and show on first render
* as well as hiding the window on exit, before the window closes
* rewrite VArray to delete itself after losing reference - untested
* rewrite shaders to delete themself
* make window not resizable
* the gl part should track binds and ignore bind calls when they are not necessary

* enable split sections for ghc in cabal?
* see what the stencil render looks like on a printed out texture via juicypixels

* make eglMakeCurrent available through the outside - this function is for drawing without display
* add function for filtering for pressed keys in events

* free up the definition for textures, such that more types can be created
	* e.g. with options to have yes/no mipmaps
	* repeating layouts or no
* expr variable for screen ratio

* function for defining expr by IO
	* provide expr for access to time, etc


* provide function for videoModeRefreshRate access of glfw
	* use it in Farbe to set work duration
	-> data WorkDuration = Automatic | Manual Int
	-> when automatic, will obtain fps and work duration calculated periodically

* write generic windows class, for the functions used in Farbe
	-> reverse dependency
* add OBJ type that loads and holds mvar with the data. it allows for checks if it is loaded. extend generic vertexarray for those checks

* upgrade Window with Vec

* rewrite textures to delete themself - done, need testing

* try alternative window creation, if creation fails.
	see ContextCreationAPI - GLFW_OSMESA_CONTEXT_API

* save arbitrary shader functions without typeable

# DONE

* add variadic var/expr parameters to compile

* rewrite window to track pressed keys, by ensuring which keys are pressed by asking all after tabbing back in - done, but untested



# info

new framebuffers need a depth buffer in order to render in respect to depth.
renderbuffers are for when you do need depth or stencil, but without directly accessing them. rendering color will access depth and depending on settings also stencil.
stencil settings need to be reset, directly after use or it messes up the following frames. glStencilOp says how stencil is written. glStencilFunc says how stencil is used. if GL_REPLACE, glStencilFunc also writes.



