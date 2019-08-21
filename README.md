# gambit-podofo

[gambit-podofo](https://github.com/pcerman/gambit-podofo) is
[gambit](http://gambitscheme.org) scheme binding to the
[podofo](http://podofo.sourceforge.net) library. There are only implemented
bindings to functions working on low level PDF data structure. I am not planning
to add all functions from this library.

Original source code of this library is published in github
[resository](https://github.com/pcerman/gambit-podofo).

Library is developed on **debian 9** linux by using  gcc compiler. It works also
on **ubuntu 18.04** linux. Probably it works on other linux distributions too.
This is early release therefor lot of things are still missing there. For example
there is no documentation yet, error reporting ...

It requires lot of information about gambit scheme, podofo library and internal
structure of PDF to understand and to use it.

## Pre-requisites

It is necessary to install **gambit** scheme and **podofo** library to build it.
I have used _gambit version 4.9.2_ and _podofo version 0.9.4_. It probably works
with other versions too.

## How to build

Next commands copy files **podofo.scm**, **ffi-podofo#.scm** and **ffi-podofo.o1**
into directory **$GAMBIT/lib/pdf**.

``` bash
cd src
make
sudo make install
```

There are examples in the example subdirectory. It requires to build and install
this library to run them.

``` bash
cd examples

./get-pages.scm
./struct-tree.scm
./create-pdf.scm

gsi test.scm
```

_**Template of a script**_

``` scheme
#!/usr/bin/env gsi-script

(include "~~lib/pdf/podofo.scm")

(define (main . args)
   ;; rest of the program
   )
```

## License

Copyright (c) 2019 Peter Čerman (https://github.com/pcerman)

This library is released under GNU General Public License v3.0

It is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU Library General Public License along
with this program.
