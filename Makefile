# NOTE: I should definitely make up my mind to rewrite all this 
#       from scratch!!

MINIXFS_VERSION = 0.03
MINIXFS_NAME = minixfs

srcdir = .

CC = gcc
RM = rm -f
CP = cp
MV = mv
SHELL = /bin/sh

CFLAGS = -Wall -g -O2
INCLUDES = -I.

HURD_CPPFLAGS = -D_GNU_SOURCE -D_IO_MTSAFE_IO -D_FILE_OFFSET_BITS=64
MINIXFS_CPPFLAGS = -DMINIXFS_DEBUG -DMINIXFS_VERSION='"$(MINIXFS_VERSION)"' \
                   -DMINIXFS_NAME='"$(MINIXFS_NAME)"'
CPPFLAGS = -DMINIXFS_DEBUG $(INCLUDES) $(HURD_CPPFLAGS) $(MINIXFS_CPPFLAGS)

SRCS = $(srcdir)/bugaddr.c $(srcdir)/dir.c $(srcdir)/getblk.c \
       $(srcdir)/ialloc.c $(srcdir)/inode.c \
       $(srcdir)/minixfs.c $(srcdir)/msg.c $(srcdir)/pager.c \
       $(srcdir)/pokel.c $(srcdir)/storeinfo.c $(srcdir)/super.c \
       $(srcdir)/truncate.c $(srcdir)/zalloc.c
OBJS = $(SRCS:.c=.o)
HDRS = $(srcdir)/minixfs.h $(srcdir)/minix_fs.h $(srcdir)/bitmap.c
HURDLIBS = diskfs pager iohelp fshelp store threads ports ihash shouldbeinlibc

# More useful version of HURDLIBS
LIBS = $(foreach lib, $(HURDLIBS), -l$(lib))

# Main rule to link executables
define link-executable
$(CC) $(rpath) $(CFLAGS) $($*-CFLAGS) $(LDFLAGS) $($*-LDFLAGS) \
      $(BUGADDR_REF) \
      -o $@
endef

# Building the target
minixfs: $(OBJS)
	$(link-executable) $(filter %.o,$^) $(LIBS)

# Just build all the object files.
$(OBJS): $(SRCS) $(HDRS)

$(srcdir)/truncate.c: $(srcdir)/truncate.c.in $(srcdir)/function-tailoring.sh
	$(SHELL) function-tailoring.sh -o truncate.c truncate.c.in

$(srcdir)/getblk.c: $(srcdir)/getblk.c.in $(srcdir)/function-tailoring.sh
	$(SHELL) function-tailoring.sh -o getblk.c getblk.c.in

clean:
	rm -f *.o minixfs core getblk.c truncate.c