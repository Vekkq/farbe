### OpenGL

OpenGL is an API for using a graphics card to render images. The API revolves around arranging buffers, textures and shaders. Shaders are computations running on the graphics card that apply a user-defined function to every element of an array. Shaders are combined into a shader program to be able to run. The complete program includes a mandatory rasterization pass.

The major shader types are vertex and fragment shaders. Vertex shaders apply once to every element of a user-given array. The shader is written to calculate a vertex, a coordinate. The vertex is used for rasterization. Rasterization turns every 3 vertices into a triangle.
In the fragment shader, for every drawn fragment, also called pixel, a user-defined function is applied. The shader generally applies texture bits to fragments and delivers a rendered image.

Several renders are commonly combined to achieve more than one render operation could do. E.g. shadows are usually done by rendering only depth information from a light source point of view and using this in a final render to apply shadows over fragments.


The OpenGL API is implemented by their respective graphics card vendor. Render results may differ between graphics cards.

