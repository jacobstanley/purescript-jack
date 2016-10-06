# purescript-jack

Testing library in the spirit of Hughes & Classen's Haskell QuickCheck,
the key improvement being that shrinking is baked in to the `Gen` monad,
so you get it for free.

There isn't much here yet, but I'll get it on to bower once it's in
a usable state.


## Developing

Ensure Purescript is installed and available on your path.  [Getting started instructions](http://www.purescript.org/learn/getting-started/) are available on the purescript site.

Then run bower to install the purescript packages

     bower install

Run pulp to build purescript-jack

     pulp build
     pulp test
