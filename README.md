
Complete Binary Tree (CST)
====

A complete binary tree is implemented as an instance of

* **Monoid** => The associative operation will be implemented as appending each element of the second operand to the first operand (not so useful I shall admit); the empty element is `Nil`.
* **Functor** => Applying a function (`a -> b`) to a CST is applying that function to each element of it.
* **Applicate** => Each function of certain position in the CST is applied to the node of the respective position in the other CST.
* **Monad** => Somewhat same as the Functor instance implementation: for any node `Node x lch rch`, apply `f` to `x` (call the result `fx`), then merge `fx` with the result of binding f to `lch` and `rch` (, which obviously shows that binding a function to a CST gets you another CST, possibly carrying data of another type). This implementation is not very useful as well.
* **Foldable** => For each `Node x lch rch`, fold on `x` first, then fold on left subtree, lastly fold on right subtree.

All the laws that these instances should obey haven't been tested yet.


Min Heap
====

A min heap consists of a CST (which makes it hard to implement an instance of monad for min heaps).

A min heap is an instance of

* **Monoid** => Combining two min heap should result in another min heap with elements from both the min heaps; the empty element is an empty min heap.


License
====

This library is distributed under the Troll License.

