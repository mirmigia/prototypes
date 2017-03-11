# Universe 2D

Prototype for how to render and navigate a 2D visual representation of the universe.  

It uses an undirected graph data structure to represent points and generates
their exact X and Y positions using the (subject to change) [fruchterman and reingold](http://citeseer.ist.psu.edu/viewdoc/download;jsessionid=19A8857540E8C9C26397650BBACD5311?doi=10.1.1.13.8444&rep=rep1&type=pdf)
force-directed layout algorithm.

## Requirements

* [SBCL](http://sbcl.org)
* [Quicklisp](https://www.quicklisp.org)
* [SDL 2](https://libsdl.org)

## Todo

- [x] actually get algorithm working
- [ ] use JSON as an exchange format
- [ ] render JSON data

## Lessons learned

### 2016-12-15 

The graph will need to be pre-rendered for players. I was thinking of
just adding a simple "Generate" button with map size options of mmall,
medium, large for demo purposes. On the backend, standard (slightly randomized)
size will be generated from before.  

However, it looks like the graph data structure is a good fit for us. We generate 
a set of solar systems and pre-simulate the force-directed layout algorithm, generating
the X and Y positions of each. This will be done on the backend.

## Questions remaining

* How large should the map be?
