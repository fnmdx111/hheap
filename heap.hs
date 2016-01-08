
data CBT a = Nil | Node a (CBT a) (CBT a)
  deriving (Show, Read, Eq)

depth :: CBT a -> Int
depth Nil = 0
depth (Node _ l r) = max (depth l + 1) (depth r + 1)

instance Monoid (CBT a) where
  mempty = Nil
  Nil `mappend` r = r
  Node a l r `mappend` x =
    if depth l > depth r
    then Node a l (mappend r x)
    else Node a (mappend l x) r

instance Functor CBT where
  fmap f Nil = Nil
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Applicative CBT where
  pure x = Node x Nil Nil
  Nil <*> x = Nil
  Node f fl fr <*> Nil = Nil
  Node f fl fr <*> Node a l r = Node (f a) (fl <*> l) (fr <*> r)

instance Monad CBT where
  return x = Node x Nil Nil
  Nil >>= f = Nil
  Node x l r >>= f = (f x) `mappend` (l >>= f) `mappend` (r >>= f)

instance Foldable CBT where
  f `foldMap` Node a Nil Nil = f a
  f `foldMap` Node a l r = foldMap f l `mappend` f a `mappend` foldMap f r
  foldr f acc Nil = acc
  foldr f acc (Node a l r) = foldr f (f a (foldr f acc l)) r

pop_last :: CBT a -> CBT a
pop_last Nil = error "Popping empty complete binary tree."
pop_last (Node _ Nil Nil) = Nil
pop_last (Node a lch Nil) = Node a (pop_last lch) Nil
pop_last (Node a lch rch) = Node a lch (pop_last rch)

data MinHeap a = MkMinHeap (CBT a)
  deriving (Show, Eq)

mk_min_heap :: MinHeap a
mk_min_heap = MkMinHeap Nil

heapify :: Ord a => CBT a -> MinHeap a
heapify x = MkMinHeap $ heapify' x
  where heapify' :: Ord a => CBT a -> CBT a
        heapify' Nil = Nil

        heapify' all@(Node a Nil Nil) = all

        heapify' all@(Node a l@(Node b lb rb) Nil) =
          let hl@(Node hb lhb lrb) = heapify' l
          in if a > hb
             then Node hb (Node a lb rb) Nil
             else Node a hl Nil

        heapify' all@(Node a l@(Node b lb rb) r@(Node c lc rc)) =
          let hl@(Node hb lhb rhb) = heapify' l
              hr@(Node hc lhc rhc) = heapify' r
          in let m = minimum [a, hb, hc]
             in if a == m
                then Node a hl hr
                else if hb == m
                     then Node hb (Node a lhb rhb) hr
                     else if hc == m
                          then Node hc hl (Node a lhc rhc)
                          else error "Impossible."

append :: Ord a => MinHeap a -> a -> MinHeap a
append h x = heapify $ append' h x
  where append' :: Ord a => MinHeap a -> a -> CBT a
        (MkMinHeap cbt) `append'` x = cbt `mappend` (Node x Nil Nil)

instance Ord a => Monoid (MinHeap a) where
  mempty = MkMinHeap Nil
  m1@(MkMinHeap mh1) `mappend` m2@(MkMinHeap mh2) =
    case (mh1, mh2) of
          (Nil, Nil) -> MkMinHeap Nil
          (Node _ _ _, Nil) -> m1
          (Nil, Node _ _ _) -> m2
          (Node a _ _, Node b _ _) -> if a < b
                                      then foldr (\x acc -> append acc x)
                                           m1 mh2
                                      else foldr (\x acc -> append acc x)
                                           m2 mh1

root :: MinHeap a -> a
root (MkMinHeap Nil) = error "Empty heap."
root (MkMinHeap (Node x _ _)) = x

pop :: Ord a => MinHeap a -> MinHeap a
pop (MkMinHeap Nil) = error "Empty heap."
pop (MkMinHeap all@(Node x l r)) =
  heapify $ pop_last $ Node (find_last all) l r
  where find_last :: CBT a -> a
        find_last Nil = error "Finding last element in an empty CBT."
        find_last (Node x Nil Nil) = x
        find_last (Node x lch Nil) = find_last lch
        find_last (Node x lch rch) = find_last rch

(<+>) :: Ord a => MinHeap a -> a -> MinHeap a
(<+>) = append

a = mk_min_heap

heap_sort :: Ord a => [a] -> [a]
heap_sort xs =
  let h = foldr (\x acc -> append acc x) mk_min_heap xs
  in pop_all h []
  where pop_all :: Ord a => MinHeap a -> [a] -> [a]
        pop_all heap xs | heap == mempty = reverse xs
        pop_all heap xs = pop_all (pop heap) ((root heap):xs)

