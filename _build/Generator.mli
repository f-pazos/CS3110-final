open State 

type point = int * int 

(* Here is a type representing the side of a polygon. 
 * AF: A side (p0, p1) represents a side of a polygon with points at p0 & p1. 
 * RI: None. A side may be given with either point first. (p0, p1) and (p1,p0)
 * would be considered equal sides. *)
type side = point * point 

(* Here is a type that maintains the points and sides of a polygon. 
 * Note: The type point array is synonymous with Graphics.polygon.
 *
 * points AF: [| p0, p1, ..., pn |] represents the polygon with n sides
 * having sides with endpoint (p[i], p[i+1]). Throughout, [] is mod n. 
 *
 * sides AF: [| s0, ..., sn |] represents the n sides of a polygon. Note, 
 *   each s_i and s_(i+1) must share one point. This also applies for [s0] and
 *   s[n], so the polygon is actually connected. 
 *
 * outline RI : sides and points are values for the same polygon. Further, 
 *   each sides[i] must be found equal to (points[i], points[i+1]). *)
type outline = {size : int; points : point array; sides : side array}


(* A type that outlines a "world" object. 
 * - [regions] : maintains the individual borders of the regions in the world. 
 *
 * RI: A valid world object satisfies the following conditions: 
 * 
 * 1 : All map regions are mutually exclusive. This means no two regions may 
 * overlap. That is, if regions r1 and r2 both bound a point p, then r1=r2. 
 *
 * 2 : A map is connected. Every side s found in regions(i).sides can either 
 * ONLY be found in the border, or be found in exactly two regions. These two
 * regions are considered adjacent. 
 * 
 * 3 : A map m's border is exactly encompassing. What is meant by this is
 * that for every point p in m, p is either
 * in exactly 1 region OR on the border of 2 regions OR exactly at the 
 * intersection of  regions. *)
type world = {size : int; regions: outline array}

(* [generate_state size attitude scarceness] Creates a new world based off of
 * the parameters given. [size] determines how many tribes there are, [attitude] * is a measure of how aggressive or generous tribes will be, and [scarceness]
 * is a measure of how limited resources are in the world. 
*)
val generate_state : int -> int -> int -> state 

