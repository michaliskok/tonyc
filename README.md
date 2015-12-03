tonyc
=====

This is the source code for tonyc, a compiler for the language Tony, implemented for Compilers Course,
8th Semester, National Technical University of Athens.

Author: Michalis Kokologiannakis.
Course info and details: http://courses.softlab.ntua.gr/compilers/2015a/

* [Licence](#licence)
* [Dependencies](#dependencies)
* [Installing](#installing)
* [Usage](#usage)
* [Features](#features)
* [Notes](#notes)

Licence
-------

This program is distributed under the GPL, version 3 or later. Please see
the COPYING file for details.

Dependencies
------------

In order to use the tool, you need the following programs:

1. **OCaml and ocamlbuild**

	Download the latest release from the [official website](https://ocaml.org/), or
	from your package manager.

3. **_[Optional]_ dosbox**

   Only if you want to run the .asm files in a Unix-like environment.

Installing
----------

* Download tonyc's sources or clone this repository:

		git clone https://github.com/michalis-/tonyc.git

* For a default build:

		make

Usage
-----

* If you just want to generate the assembly files, use:

	./tonyc &lt;your-file&gt;

* Alternatively, if you just want to execute a .tony file, use:

	./run.sh &lt;your-file&gt;

	This action requires a working dosbox installation.


Features
--------

Some features of the Tony language are:

* Simple structure and syntax of statements
* All basic data types including booleans, characters, integers, one dimensional arrays and *lists*
* Simple functions, by value or by reference parameter passing with left-to-right evaluation.
* Pascal-like variable scope
* A small standard library of functions
* Simple code optimizations
* *Dynamic memory allocation*
* *Garbage Collection*

Notes
-----

* The quad numbering in intermediate code output is not always increasing. There may be some
swapped quads. This is done in order to preserve the evaluation order.
* Garbage collector *does not* work well with derived pointers.
* Full language specifications can be found at the course's site (in Greek).
