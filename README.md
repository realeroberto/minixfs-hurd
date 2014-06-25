minixfs
=======

A MinixFS translator for the Hurd operating system.

(Pre-alpha release 0.03)

### INTRODUCTION

  This is a simple translator for the MINIX file system.  Its code has been
borrowed from the Ext2 translator and subsequently adapted as needed.  
  It is still very unstable, yet it will (hopefully) become by degrees more
mature and reliable, as I have a lively interest in the project.  There are
many things to be done: the following is but a brief reminder of (some among)
the most momentous ones.

  * Rewriting such parts as are inherent to the Ext2 file system and only
    by dint of ugly botch-works have been adapted to the MINIX fs.

  * Polishing the code and commenting it in a more orderly manner (many
    functions have comments which are but relics of the Ext2 code).

  * Rewriting the Makefile.

  * Re-implementing better inode- and zone-allocation policies (in simplifying
    the algorithms used by the Ext2 translator, I made them probably less
    efficient).

  * Devising a good solution to the problem of conciliating the limitations
    of the MINIX file system with the needs of the GNU OS, without hindering
    compatibility with other environments.

  * Writing some documentation on the MINIX file system itself.

  * Developing a simple test suite.

### COMPILATION

To compile the translator, just enter
```
$ make
```
at the prompt, perhaps after having adjusted something in the Makefile, which
is, by the way, a very clumsy specimen of its species.
  As to the installation, I would recommend not to do it at all, for the
translator is still inclined to behave itself in a rather whimsical manner.
This is, after all, a pre-alpha release and is intended for testing only.
Hence, the wisest thing to do is of course to experiment with it in a safe
place.
  To create a new MINIX file system, you may want to avail yourself of the
`util-linux' package, which can be found at

  http://packages.debian.org/util-linux

(thanks to Robert Millan for pointing me that).

### BUG-REPORTS

  I shall be very glad to receive bug-reports as well as suggestions for
improvement.  Please contact me at <roberto.reale82@gmail.com>.

  Roberto Reale

-*- Text -*-
