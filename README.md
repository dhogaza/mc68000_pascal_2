# mc68000_pascal_2

Once upon a time a company called Oregon Software sold a suite of optimizing compiler
products based on compiler techology that I began developing around 1977.

These eventually grew to include compilers for Pascal, Modula-2, and C/C++

We targeted the PDP-11 for our first version, and eventually also targeted the VAX,
X86, MC68000, SPARC, and NS32000 architectures.

We hosted compilers on a large variety of operating systems including VMS, MSDOS,
VersaDOS, and a wide variety of BSD- and Bell-based Unix OSes (there was no linux).

We also provided cross compilers for some environments.

Binaries for the RSX PDP-11 version of Pascal-2 (as we called it, since we had earlier
written and sold OMSI Pascal-1, written in assembly and used to bootstrap Pascal-2)
can be found, but there are no sources.

The code found here is for our VAX/VMS-hosted MC68000 cross-compiler.  VAX/VMS binaries
exist for the compiler, assembler, and library.  Programs can be run under VersaDOS or
standalone, and the library supports concurrancy primitives.

Only sources for the compiler, some utilities, and part of the library (written in assembly
exist).  

Unfortunately source code for the Modula-2 and C/C++ frontends seem to no longer exist, nor
do code generators for any architecture other than the MC68000.

As the person who wrote the Pascal-2 and the first version of the Modula-2 front ends, and
most of the PDP-11, VAX, MC68000, and SPARC code generators, this saddens me.  But I'm glad
that an old customer dug up sources they had purchased and sent a copy along to me.

There are scary copyright statements in the source.  As the majority owner of Oregon Software
while it existed, and primary author of the technology, I'm not too worried about suing myself
for putting them up here for public viewing.  I may switch sources over to the GPL.

Compiler sources are in cross/sources and are self-hosting (i.e. written in Pascal-2), as were
all of our compilers though the C/C++ front end was eventually machine-translated to C.

In cross/fpc_sources there's ongoing work to rehost the code in Free Pascal.  The primary reason
for doing this is because I'm interested in comparing the quality of code our compiler generated
vs modern technology like LLVM.

Some notes on the structure:

The compiler originally ran computers as small as a PDP-11 running RT-11 with a maximum of 56KB
RAM available.  Yes, KB, not MB much less GB.  And it could compile itself in that environment
with only two 8" floppy disks for mass storage.   This compiler was the basis of DEC's MicroPower
Pascal and their RSX-11 Pascal.  Our contract called for the compiler to compile itself in the
above environment at 100 lines per minute.  Slow!  While we met the goal, DEC realized that this
was stupid and only supported MicroPower Pascal on PDP-11s with at least an RK05 1.2MB hard drive.

To make this possible, the compiler is split up into several physical pieces.

Architecturally it is a two-pass compiler.   First pass consists of a lexical scanner and
syntactic/sematic parser.  The second pass consisted of an optimizer and code generator
working on one procedure at a time.

However on a small computer it was configured to run in four passes.  When running as two
passes, it could cache necessary data structures on the disk using an LRU approach.  If the
environment included enough RAM, the compiler could be statically configured to keep all
necessary data in RAM.

So if you do dare to dig in, you'll see a lot of code conditionalized by "if bigcompiler"
(all data stored in memory), "if needcaching", "if scanalys" and "if travcode" (number
of passes) and the like.  I'm slowly ripping that out of the Free Pascal-hosted version
but the original sources give some idea of the effort required to support a wide range
of computers back in those days.

