(* [generate_polys w h n] creates [n] different polygons in an area with width
 * [w] and height [h]. These represent the regions the various tribes inhabit. 
 *)
val generate_polys : int -> int -> int -> (int*int) array array

