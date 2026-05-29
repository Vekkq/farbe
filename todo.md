
* add other glsl functions (e.g. matrix ops, boolean)
* add variadic var/expr parameters to compile

* write optimizer to turn multiused asts into shared variables
* integrate Data.Bits operations as Expr

* extending tuple instances

* rewrite window to track pressed keys, by ensuring which keys are pressed by asking all after tabbing back in - done, but untested

* rewrite VArray to delete itself after losing reference
* rewrite shaders to delete themself - done
* delete attached shaders with glDeleteShader - done
* rewrite textures to delete themself - done, need testing

* make window not resizable
* consider to apply always `up` to the vertex shader position value
  and add same same instances, like V3 -> V3

* add ShaderM again and replace Defer Shdr

* the gl part should track binds and ignore bind calls when they are not necessary
* the gl part should separate todos in immediate and delayed tasks
	* done - not necessary since the api is generally blockfree until render ops are called

* see what the stencil render looks like on a printed out texture via juicypixels

* enable split sections for ghc in cabal

* ease `compile` to not depend on Defer for making prerender and have the returned monads be separate

* make eglMakeCurrent available through the outside - this function is for drawing without display

* add function for filtering for pressed keys in events

* free up the definition for textures, such that more types can be created
	* e.g. with options to have yes/no mipmaps
	* repeating layouts or no
	*** done

* fix cleansing of VArray with Delayed

* have DMap fix itself, when the first lookup fails
	* done - DMap removed

* looks like stablenames are unusable for functions - will have to do with hashes.

* maybe use HasCallStack for tracking shader definitions


* direct varray file access using paths

* expr variable for screen ratio
* function for defining expr by IO

* provide function for videoModeRefreshRate access of glfw
	* use it in Farbe to set work duration
	-> data WorkDuration = Automatic | Manual Int
	-> when automatic, will obtain fps and work duration calculated periodically


info:
new framebuffers need a depth buffer in order to render in respect to depth.
renderbuffers are for when you do need depth or stencil, but without directly accessing them. rendering color will access depth and depending on settings also stencil.
stencil settings need to be reset, directly after use or it messes up the following frames. glStencilOp says how stencil is written. glStencilFunc says how stencil is used. if GL_REPLACE, glStencilFunc also writes.
shaders are compiled entirely concurrently and a get gives its status.

question:
I have a big stack of mostly StateT transformers, each StateT wrapped in their respective newtype, next to functions that operate on it. I ran into the issue that I have to write a lot of boilerplate instances and increasingly long deriving lists. are there any shortcuts to it, that dont involve flattening the transformers?
I particular see an issue when building ontop of the stack, when the next top has to derive a huge list of classes to keep the below working. arguably one could write a dedicated class to abstract all that is to be pushed out.



## Restructuring

Outermost: Uniform, Use/Upload, Expr, Vec, Window,


## Pr to-dos

Write TextureExpr
Load STL in background
rewrite loading of shader to recognize loading state of required textures
Write HandShdr
Write debugs
Write window to start invisible
write code optimization for shaders

add rdelay again, with handtex instead of farbe constraint
remove mvar from texture function. it has to be used by the time-confusing juice function

rewrite shaders to record Shdr by name, to run it separately and only once




