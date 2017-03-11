# universe-2d

2D universe generation prototype.

## Usage

You can run the visualization using the below command. 

    $ java -jar universe-2d-0.1.0-standalone.jar [n-systems]
    
## Options

`n-systems` specifies the number of solar systems to generate.

## Algorithm 

The [Fruchterman & Reingold](proto/universe-2d/doc/reingold_graph_drawing_by_force_directed_placement.pdf)
algorithm is used for positioning.  

This quote from the paper demonstrates why I think this is the best choice for our use case:

> Our algorithm will resemble molecular or planetary simulations, some-times called n-body problems.

### Future plans

In the future I want to tweak the algorithm slightly so that it resembles the 
mechanics of the big bang, rather than those of springs. Currently, the fructherman
& reingold (F&R) algorithm requires all nodes to be placed in random positions 
from the beginning. If all nodes are centered at (0,0) then there will be no
displacement.  

I propose we instead gather all nodes at (0,0) and apply a large "impulse", 
that slowly cools down. This is similar to the temperature/cooldown part of the
F&R algorithm, but all nodes will begin with a large initial velocity that eventually
slows down (but may continue for a large amount of time). It will iterate for 
a certain period of time (found with experimentation) and then halt. I imagine this
will seem similar to the current state of the universe.  

This way nodes (or solar systems in this case) won't be as symetric as in the F&R
algorithm, making it more visually applealing in this case while still using
the same basis.
