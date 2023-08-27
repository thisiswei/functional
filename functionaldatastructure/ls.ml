
fun concat([], ys) = ys
  | concat((x::xs), ys) = x::concat(xs, ys)

fun append([], y) = [y]
  | append((x::xs), y) = x::append(xs, y)


fun update([], i, y) = raise Empty
  | update(x::xs, 0, y) = y::xs
  | update(x::xs, i, y) = x::update(xs, i-1, y)


(* suffixes [1,2,3,4] = [[1,2,3,4], [2,3,4], [3,4], [4], [ ] ] *)
fun suffixes [] = [[]]
  | suffixes (x::xs) = concat([(x::xs)], suffixes xs)


datatype 'a tree = E
                 | T of 'a tree * 'a * 'a tree


(* val t1 = T (T (E,1,E),3,T (E,4,E)); *)
(* ismember(1, t1); *)


fun ismember (x, E) = false
  | ismember (x, T (a, y, b)) =
      if x < y then ismember(x, a)
      else if x > y then ismember(x, b)
      else true


fun insert (x, E) = T (E, x, E)
  | insert (x, s as T (lf, y, rt)) =
      if x > y then T (lf, y, insert(x, rt))
      else if x < y then T (insert(x, lf), y, rt)
      else s

(*
   Exercise 2.2 (Andersson [And91D In the worst case, member performs approximately 2d comparisons,
   where d is the depth of the tree.  Rewrite member to take no more than d + 1 comparisons by keeping track of a candidate element
   that might be equal to the query element (say, the last element for which < returned false or < returned true) and
   checking for equality only when you hit the bottom of the tree.
*)


(* Exercise 2.3 Inserting an existing element into a binary search tree copies the entire search path even though the copied nodes are indistinguishable from the originals.
 Rewrite insert using exceptions to avoid this copying. Establish only one handler per insertion rather than one handler per iteration. *)


(* Exercise 2.4 Combine the ideas of the previous two exercises to obtain a ver- sion of insert that performs no unnecessary copying and uses no more than d+ 1comparisons. *)

(* Exercise 2.5 Sharing can also be useful within a single object, not just be- tween objects. For example, if the two subtrees of a given node are identical, then they can be represented by the same tree. *)

(* (a) Using this idea, write a function complete of type Elem x int -> Tree where complete (x, d) creates a complete binary tree of depth d with x stored in every node. (Of course, this function makes no sense for the set abstraction, but it can be useful as an auxiliary function for other abstrac- tions, such as bags.) This function should run in O(d) time. *)

(* (b) Extend this function to create balanced trees of arbitrary size. These trees will not always be complete binary trees, but should be as balanced as possible: for any given node, the two subtrees should differ in size by at most one. This function should run in 0(log n) time. (Hint: use a helper function create2 that, given a size m, creates a pair of trees, one of size m and one of size m+1.) *)


(* Exercise 2.6 Adapt the UnbalancedSet functor to support finite maps rather than sets. Figure 2.10 gives a minimal signature for finite maps. (Note that the NOTFOUND exception is not predefined in Standard ML—you will have to de- fine it yourself. Although this exception could be made part of the FINITEMAP signature, with every implementation defining its own NOTFOUND exception, it is convenient for all finite maps to use the same exception.) *)

