a rhizome is a graph where each edge is addressable, they can be linked to. 

> data Relator a = Relation (Relator a, Relator a) | Terminal (Relata a)

> data Relata a = Value a

> foldRelator :: Relator a -> (b -> b -> b) -> (Relata a -> b) -> b
> foldRelator (Terminal x) relation value = value x
> foldRelator (Relation (rx,ry)) relation value = relation (foldRelator rx relation value) (foldRelator ry relation value) 
