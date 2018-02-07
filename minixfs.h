/* Common definitions for the MINIX file system translator

   Copyright (C) 1995, 1996, 1999, 2002 Free Software Foundation, Inc.

   Written by Miles Bader <miles@gnu.ai.mit.edu>

   Modified for the MINIX file system from the original for the ext2
   file system by Roberto Reale <rober.reale@gmail.com>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <mach.h>
#include <hurd.h>
#include <hurd/ports.h>
#include <hurd/pager.h>
#include <hurd/fshelp.h>
#include <hurd/iohelp.h>
#include <hurd/diskfs.h>
#include <hurd/store.h>
#include <assert.h>
#include <stdint.h>
#include <limits.h>
#include <rwlock.h>
#include <sys/mman.h>

#include <hurd/version.h>
#if HURD_INTERFACE_VERSION != 20020609
# warning "The code has not been tested against your version of the Hurd library!"
#endif

/* hurd/hurd/hurd-types.h neglects us... */
#define FSTYPE_MINIXFS 0x00001001

#define __hurd__		/* Enable some hurd-specific fields.  */

/* Types used by the MINIX header files.  */
typedef uint32_t __u32;
typedef uint16_t __u16;
typedef uint8_t  __u8;

#include "minix_fs.h"

#define MINIXFS_ROOT_INO	MINIX_ROOT_INO

/* First non-reserved inode.  */
#define MINIXFS_FIRST_INO	(MINIXFS_ROOT_INO + 1)

#define MINIXFS_NDIR_ZONES 	sblock_info->s_ndzones
#define MINIXFS_IND_ZONE 	MINIXFS_NDIR_ZONES
#define MINIXFS_DIND_ZONE 	(MINIXFS_IND_ZONE + 1)
#define MINIXFS_TIND_ZONE 	(MINIXFS_DIND_ZONE + 1)
#define ADDR_PER_BLOCK		sblock_info->s_nindirs

/* No minixfs_debug routine is defined in minix_fs.h, so let us define
   our own.  */

#ifdef MINIXFS_DEBUG

extern int minixfs_debug_flag;
#define minixfs_debug(f, a...)						\
  do {									\
    if (minixfs_debug_flag) 						\
      printf ("minixfs: (debug) %s: " f "", __FUNCTION__ , ## a); 	\
  } while (0)
#else
#define minixfs_debug(f, a...)	(void)0
#endif /* MINIXFS_DEBUG */

#undef __hurd__

/* Define this if memory objects should not be cached by the kernel.
   Normally, don't define it, but defining it causes a much greater rate
   of paging requests, which may be helpful in catching bugs.  */

#undef DONT_CACHE_MEMORY_OBJECTS

int printf (const char *fmt, ...);

/* A block number and a zone number.  */
typedef uint32_t block_t;
typedef uint32_t zone_t;

/* ---------------------------------------------------------------- */
/*
 * MINIX inode data in memory
 */
struct minix_inode_info
{
  union
  {
    uint16_t i1[16];
    uint32_t i2[16];
  } i_data;
};
#define i_zone_V1 i_data.i1
#define i_zone_V2 i_data.i2

/* ---------------------------------------------------------------- */

struct poke
{
  vm_offset_t offset;
  vm_size_t length;
  struct poke *next;
};

struct pokel
{
  struct poke *pokes, *free_pokes;
  spin_lock_t lock;
  struct pager *pager;
  void *image;
};

void pokel_init (struct pokel *pokel, struct pager *pager, void *image);
/* Clean up any state associated with POKEL (but don't free POKEL).  */
void pokel_finalize (struct pokel *pokel);

/* Remember that data here on the disk has been modified. */
void pokel_add (struct pokel *pokel, void *loc, vm_size_t length);

/* Sync all the modified pieces of disk */
void pokel_sync (struct pokel *pokel, int wait);

/* Flush (that is, drop on the ground) all pending pokes in POKEL.  */
void pokel_flush (struct pokel *pokel);

/* Transfer all regions from FROM to POKEL, which must have the same pager. */
void pokel_inherit (struct pokel *pokel, struct pokel *from);

#ifndef MINIXFS_EI
#define MINIXFS_EI extern inline
#endif

/* ---------------------------------------------------------------- */
/* Bitmap routines.  */

/* Returns TRUE if bit NUM is set in BITMAP.  */
MINIXFS_EI int
test_bit (unsigned num, char *bitmap)
{
  const uint32_t *const bw = (uint32_t *) bitmap + (num >> 5);
  const uint_fast32_t mask = 1 << (num & 31);
  return *bw & mask;
}

/* Sets bit NUM in BITMAP, and returns the previous state of the bit.  Unlike
   the linux version, this function is NOT atomic!  */
MINIXFS_EI int
set_bit (unsigned num, char *bitmap)
{
  uint32_t *const bw = (uint32_t *) bitmap + (num >> 5);
  const uint_fast32_t mask = 1 << (num & 31);
  return (*bw & mask) ?: (*bw |= mask, 0);
}

/* Clears bit NUM in BITMAP, and returns the previous state of the bit.
   Unlike the linux version, this function is NOT atomic!  */
MINIXFS_EI int
clear_bit (unsigned num, char *bitmap)
{
  uint32_t *const bw = (uint32_t *) bitmap + (num >> 5);
  const uint_fast32_t mask = 1 << (num & 31);
  return (*bw & mask) ? (*bw &= ~mask, mask) : 0;
}

/* ---------------------------------------------------------------- */

/* MINIXfs specific per-file data.  */
struct disknode
{
  /* For a directory, this array holds the number of directory entries in
     each piece of the directory of size BLOCK_SIZE. */
  int *dirents;

  /* Links on hash list. */
  struct node *hnext, **hprevp;

  /* Lock to lock while fiddling with this inode's block allocation info.  */
  struct rwlock alloc_lock;

  /* Where changes to our indirect blocks are added.  */
  struct pokel indir_pokel;

  /* Random extra info used by the MINIXfs routines.  */
  struct minix_inode_info info;

  /* This file's pager.  */
  struct pager *pager;

  /* True if the last page of the file has been made writable, but is only
     partially allocated.  */
  int last_page_partially_writable;

  /* Index to start a directory lookup at.  */
  int dir_idx;
};

struct user_pager_info
{
  enum pager_type
    {
      DISK,
      FILE_DATA,
    } type;
  struct node *node;
  vm_prot_t max_prot;
};

/* ---------------------------------------------------------------- */
/* pager.c */

#include <hurd/diskfs-pager.h>

/* Set up the disk pager.  */
void create_disk_pager (void);

/* Call this when we should turn off caching so that unused memory object
   ports get freed.  */
void drop_pager_softrefs (struct node *node);

/* Call this when we should turn on caching because it's no longer
   important for unused memory object ports to get freed.  */
void allow_pager_softrefs (struct node *node);

/* Invalidate any pager data associated with NODE.  */
void flush_node_pager (struct node *node);

/* ---------------------------------------------------------------- */

/* The physical media.  */
extern struct store *store;

/* What the user specified.  */
extern struct store_parsed *store_parsed;

/* Mapped image of the disk.  */
extern void *disk_image;

/* Our in-core copy of the super-block (pointer into the disk_image).  */
struct minix_super_block *sblock;

/* A handy structure to remember some information extracted from the
   super-block.  */
struct minix_super_block_info
{
  /* To remember the size of the file system (expressed in zones) the MINIX
     super-block has two fields, namely S_NZONES and S_ZONES.  While the
     first one is meaningful only in V1 file systems, V2 file systems use the 
     latter one.  Let us hide this peculiarity.  */
  zone_t s_nzones;

  unsigned long s_free_inodes_count;
  zone_t s_free_zones_count;

  size_t s_namelen;

  /* The size of a directory entry.  Such value cannot be obtained from
     the size of the MINIX_DIR_ENTRY structure (cf. its declaration in
     minix_fs.h), since it depends on the version of the file system being
     actually used.  */
  size_t s_dirsize;

  int s_dir_slots_per_fs_block;

  int s_link_max;

  /* Number of inodes per block.  */
  unsigned int s_inodes_per_block;

  /* Has the super-block been modified?  */
  unsigned short s_mount_state;

  /* Version of the file system.  */
  unsigned short s_version;

  /* Number of direct zones referenced to in an inode.  */
  unsigned int s_ndzones;

  /* Number of direct zones + blocks of indirections referenced to in
     an inode.  */
  unsigned int s_n_zones;

  /* How many addresses are contained in a block of indirection?  */
  unsigned int s_nindirs;

  /* Has the file system double or even triple indirection?  */
  unsigned short s_has_dind;	/* XXX not yet used */
  unsigned short s_has_tind;

  zone_t s_max_addressable_zone;
};

struct minix_super_block_info *sblock_info;

/* True if sblock has been modified.  */
int sblock_dirty;

/* Offsets of each section of the file system layout, expressed in file system
   blocks.  */
#define BOOT_BLOCK_BOFFS 	0
#define SUPER_BLOCK_BOFFS 	1
#define I_MAP_BOFFS 		2
#define Z_MAP_BOFFS 		(I_MAP_BOFFS + sblock->s_imap_blocks)
#define INODES_AREA_BOFFS 	(Z_MAP_BOFFS + sblock->s_zmap_blocks)
#define FIRST_DATAZONE_BOFFS 	(sblock->s_firstdatazone)

#define SBLOCK_SIZE	(sizeof (struct minix_super_block))
/* Byte offset of super-block.  */
#define SBLOCK_OFFS	(SUPER_BLOCK_BOFFS << log2_block_size)

/* The file system block-size and zone-size in bytes.  */
size_t block_size;
#define DEFAULT_BLOCK_SIZE 1024
unsigned int log2_block_size;
size_t zone_size;
unsigned int log2_zone_size;

/* log2 of the number of device blocks in a file system block.  */
unsigned log2_dev_blocks_per_fs_block;

/* log2 of the number of stat blocks (512 bytes) in a file system block.  */
unsigned log2_stat_blocks_per_fs_block;

/* log2 of the number of file system blocks in a zone.  */
unsigned log2_fs_blocks_per_zone;

/* The number of blocks in the file system.  */
unsigned int blocks_count;

#define LOG2_BITS_PER_BLOCK (log2_block_size * CHAR_BIT)
#define BITS_PER_BLOCK (1 << LOG2_BITS_PER_BLOCK)

/* A handy page of page-aligned zeros.  */
vm_address_t zerozone;

/* Get the superblock from the disk, & setup various global info from it.  */
void get_hypermetadata ();

/* ---------------------------------------------------------------- */

spin_lock_t node_to_page_lock;

spin_lock_t generation_lock;
unsigned long next_generation;

/* ---------------------------------------------------------------- */
/* Functions for looking inside disk_image */

#define trunc_block(offs) ((offs) & ~(block_size - 1))
#define round_block(offs) (((offs) + block_size - 1) & ~(block_size - 1))
#define trunc_zone(offs)  ((offs) & ~(zone_size - 1))
#define round_zone(offs)  (((offs) + zone_size - 1) & ~(zone_size - 1))

/* block num --> byte offset on disk */
#define boffs(block) ((block) << log2_block_size)
/* zone num --> byte offset on disk */
#define zoffs(zone) ((zone) << log2_zone_size)
/* byte offset on disk --> block num */
#define boffs_block(offs) ((offs) >> log2_block_size)
/* byte offset on disk --> zone num */
#define boffs_zone(offs) ((offs) >> log2_zone_size)

/* byte offset on disk --> pointer to in-memory block or zone */
#define boffs_ptr(offs) (((char *) disk_image) + (offs))
/* pointer to in-memory block or zone --> byte offset on disk */
#define ptr_boffs(ptr)  ((char *) (ptr) - ((char *) disk_image))

/* block num --> pointer to in-memory block */
#define bptr(block) boffs_ptr(boffs(block))
/* zone num --> pointer to in-memory zone */
#define zptr(zone) boffs_ptr(zoffs(zone))
/* pointer to in-memory block --> block num */
#define bptr_block(ptr) boffs_block(ptr_boffs(ptr))
/* pointer to in-memory zone --> zone num */
#define zptr_zone(ptr) zoffs_zone(ptr_boffs(ptr))

/* Convert an inode number to the dinode on disk.  */
MINIXFS_EI struct minix_inode *
dino_v1 (ino_t inum)
{
  block_t block = INODES_AREA_BOFFS + 
    ((inum - 1) / sblock_info->s_inodes_per_block);
  return ((struct minix_inode *) bptr (block)) + 
    (inum - 1) % sblock_info->s_inodes_per_block;
}
MINIXFS_EI struct minix2_inode *
dino_v2 (ino_t inum)
{
  block_t block = INODES_AREA_BOFFS + 
    ((inum - 1) / sblock_info->s_inodes_per_block);
  return ((struct minix2_inode *) bptr (block)) +
    (inum - 1) % sblock_info->s_inodes_per_block;
}

/* ---------------------------------------------------------------- */
/* inode.c */

/* Write all active disknodes into the inode pager. */
void write_all_disknodes ();

/* Lookup node INUM (which must have a reference already) and return it
   without allocating any new references. */
struct node *ifind (ino_t inum);

void inode_init (void);

/* ---------------------------------------------------------------- */

/* What to lock if changing global data data (e.g., the superblock or block
   group descriptors or bitmaps).  */
spin_lock_t global_lock;

/* Where to record such changes.  */
struct pokel global_pokel;

/* If the block size is less than the page size, then this bitmap is used to
   record which disk blocks are actually modified, so we don't stomp on parts
   of the disk which are backed by file pagers.  */
char *modified_global_blocks;
spin_lock_t modified_global_blocks_lock;

/* Marks the global block BLOCK as being modified, and returns true if we
   think it may have been clean before (but we may not be sure).  Note that
   this isn't enough to cause the block to be synced; you must call
   record_global_poke to do that.  */
MINIXFS_EI int
global_block_modified (block_t block)
{
  if (modified_global_blocks)
    {
      int was_clean;
      spin_lock (&modified_global_blocks_lock);
      was_clean = !set_bit(block, modified_global_blocks);
      spin_unlock (&modified_global_blocks_lock);
      return was_clean;
    }
  else
    return 1;
}

/* This records a modification to a non-file block.  */
MINIXFS_EI void
record_global_poke (void *ptr)
{
  int boffs = trunc_block (ptr_boffs (ptr));
  global_block_modified (boffs_block (boffs));
  pokel_add (&global_pokel, boffs_ptr(boffs), block_size);
}

/* This syncs a modification to a non-file block.  */
MINIXFS_EI void
sync_global_ptr (void *bptr, int wait)
{
  vm_offset_t boffs = trunc_block (ptr_boffs (bptr));
  global_block_modified (boffs_block (boffs));
  pager_sync_some (diskfs_disk_pager, trunc_page (boffs), vm_page_size, wait);
}

/* This records a modification to one of a file's indirect blocks.  */
MINIXFS_EI void
record_indir_poke (struct node *node, void *ptr)
{
  int boffs = trunc_block (ptr_boffs (ptr));
  global_block_modified (boffs_block (boffs));
  pokel_add (&node->dn->indir_pokel, boffs_ptr(boffs), block_size);
}

/* ---------------------------------------------------------------- */

MINIXFS_EI void
sync_global (int wait)
{
  pokel_sync (&global_pokel, wait);
}

/* Sync all allocation information and node NP if diskfs_synchronous.  */
MINIXFS_EI void
alloc_sync (struct node *np)
{
  if (diskfs_synchronous)
    {
      if (np)
	{
	  diskfs_node_update (np, 1);
	  pokel_sync (&np->dn->indir_pokel, 1);
	}
      diskfs_set_hypermetadata (1, 0);
    }
}

/* ---------------------------------------------------------------- */
/* getblk.c */

/* Returns in DISK_BLOCK the disk block correspding to BLOCK in NODE.  If
   there is no such block yet, but CREATE is true, then it is created,
   otherwise EINVAL is returned.  */
error_t minixfs_getblk (struct node *node, block_t block, int create, block_t *disk_block);

/* ---------------------------------------------------------------- */
/* zalloc.c */

void minixfs_free_zones (zone_t zones, unsigned long count);

zone_t minixfs_new_zone (struct node *node, int zero);
unsigned long minixfs_count_free_zones (void);

/* ---------------------------------------------------------------- */
/* ialloc.c */

unsigned long minixfs_count_free_inodes (void);

/* ---------------------------------------------------------------- */

/* Write disk block ADDR with DATA of LEN bytes, waiting for completion.  */
error_t dev_write_sync (block_t addr, vm_address_t data, long len);

/* Write diskblock ADDR with DATA of LEN bytes; don't bother waiting
   for completion. */
error_t dev_write (block_t addr, vm_address_t data, long len);

/* Read disk block ADDR; put the address of the data in DATA; read LEN
   bytes.  Always *DATA should be a full page no matter what.   */
error_t dev_read_sync (block_t addr, vm_address_t *data, long len);

/* ---------------------------------------------------------------- */

#define minixfs_error(fmt, args...) _minixfs_error (__FUNCTION__, fmt , ##args)
extern void _minixfs_error (const char *, const char *, ...)
     __attribute__ ((format (printf, 2, 3)));

#define minixfs_panic(fmt, args...) _minixfs_panic (__FUNCTION__, fmt , ##args)
extern void _minixfs_panic (const char *, const char *, ...)
     __attribute__ ((format (printf, 2, 3)));

extern void minixfs_warning (const char *, ...)
     __attribute__ ((format (printf, 1, 2)));
