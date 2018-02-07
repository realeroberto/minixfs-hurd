/* Inode management routines

   Copyright (C) 1994,95,96,97,98,99,2000,01,02 Free Software Foundation, Inc.

   Converted for ext2fs by Miles Bader <miles@gnu.org>

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

#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <sys/statvfs.h>
#include "minixfs.h"

#define	INOHSZ	512
#if	((INOHSZ&(INOHSZ-1)) == 0)
#define	INOHASH(ino)	((ino)&(INOHSZ-1))
#else
#define	INOHASH(ino)	(((unsigned)(ino))%INOHSZ)
#endif

static struct node *nodehash[INOHSZ];

static error_t read_node (struct node *np);

/* Initialize the inode hash table.  */
void
inode_init ()
{
  int n;
  for (n = 0; n < INOHSZ; n++)
    nodehash[n] = 0;
}

/* Fetch inode INUM, set *NPP to the node structure;
   gain one user reference and lock the node.  */
error_t
diskfs_cached_lookup (ino_t inum, struct node **npp)
{
  error_t err;
  struct node *np;
  struct disknode *dn;

  spin_lock (&diskfs_node_refcnt_lock);
  for (np = nodehash[INOHASH(inum)]; np; np = np->dn->hnext)
    if (np->cache_id == inum)
      {
	np->references++;
	spin_unlock (&diskfs_node_refcnt_lock);
	mutex_lock (&np->lock);
	*npp = np;
	return 0;
      }

  /* Format specific data for the new node.  */
  dn = malloc (sizeof (struct disknode));
  if (! dn)
    {
      spin_unlock (&diskfs_node_refcnt_lock);
      return ENOMEM;
    }
  dn->dirents = 0;
  dn->dir_idx = 0;
  dn->pager = 0;
  rwlock_init (&dn->alloc_lock);
  pokel_init (&dn->indir_pokel, diskfs_disk_pager, disk_image);

  /* Create the new node.  */
  np = diskfs_make_node (dn);
  np->cache_id = inum;

  mutex_lock (&np->lock);

  /* Put NP in NODEHASH.  */
  dn->hnext = nodehash[INOHASH(inum)];
  if (dn->hnext)
    dn->hnext->dn->hprevp = &dn->hnext;
  dn->hprevp = &nodehash[INOHASH(inum)];
  nodehash[INOHASH(inum)] = np;

  spin_unlock (&diskfs_node_refcnt_lock);

  /* Get the contents of NP off disk.  */
  err = read_node (np);

  if (err)
    return err;
  else
    {
      *npp = np;
      return 0;
    }
}

/* Lookup node INUM (which must have a reference already) and return it
   without allocating any new references.  */
struct node *
ifind (ino_t inum)
{
  struct node *np;

  spin_lock (&diskfs_node_refcnt_lock);
  for (np = nodehash[INOHASH(inum)]; np; np = np->dn->hnext)
    {
      if (np->cache_id != inum)
	continue;

      assert (np->references);
      spin_unlock (&diskfs_node_refcnt_lock);
      return np;
    }
  assert (0);
}

/* The last reference to a node has gone away; drop
   it from the hash table and clean all state in the dn structure.  */
void
diskfs_node_norefs (struct node *np)
{
  *np->dn->hprevp = np->dn->hnext;
  if (np->dn->hnext)
    np->dn->hnext->dn->hprevp = np->dn->hprevp;

  if (np->dn->dirents)
    free (np->dn->dirents);
  assert (!np->dn->pager);

  /* Move any pending writes of indirect blocks.  */
  pokel_inherit (&global_pokel, &np->dn->indir_pokel);
  pokel_finalize (&np->dn->indir_pokel);

  free (np->dn);
  free (np);
}

/* The last hard reference to a node has gone away; arrange to have
   all the weak references dropped that can be.  */
void
diskfs_try_dropping_softrefs (struct node *np)
{
  drop_pager_softrefs (np);
}

/* The last hard reference to a node has gone away.  */
void
diskfs_lost_hardrefs (struct node *np)
{
}

/* A new hard reference to a node has been created; it's now OK to
   have unused weak references.  */
void
diskfs_new_hardrefs (struct node *np)
{
  allow_pager_softrefs (np);
}

/* Read stat information out of the minix_inode.  */
static error_t
read_node (struct node *np)
{
  error_t err;
  struct stat *st = &np->dn_stat;
  struct disknode *dn = np->dn;
  struct minix_inode_info *info = &dn->info;

  err = diskfs_catch_exception ();
  if (err)
    return err;

  st->st_fstype = FSTYPE_MINIXFS;
  st->st_fsid = getpid ();	/* This call is very cheap.  */
  st->st_ino = np->cache_id;
  st->st_blksize = vm_page_size * 2;
  st->st_gen = 0; /* XXX */

  if (sblock_info->s_version == MINIX_V1)
    {
      struct minix_inode *di = dino_v1 (np->cache_id);

      st->st_nlink = di->i_nlinks;
      st->st_size = di->i_size;
      st->st_atime = st->st_mtime = st->st_ctime = di->i_time;

      /*      st->st_mode = di->i_mode & ~S_ITRANS;*/
      st->st_mode = di->i_mode;
      st->st_uid = di->i_uid;
      st->st_gid = di->i_gid;
      st->st_author = st->st_uid;
      np->author_tracks_uid = 1;

      /* Set to a conservative value.  */
      dn->last_page_partially_writable = 0;

      if (S_ISCHR (st->st_mode) || S_ISBLK (st->st_mode))
	st->st_rdev = di->i_zone[0];
      else
	{
	  memcpy (info->i_data.i1, di->i_zone,
		  sblock_info->s_n_zones * sizeof (info->i_data.i1[0]));
	  st->st_rdev = 0;
	}
    }
  else
    {
      struct minix2_inode *di = dino_v2 (np->cache_id);

      st->st_nlink = di->i_nlinks;
      st->st_size = di->i_size;
      st->st_atime = di->i_atime;
      st->st_mtime = di->i_mtime;
      st->st_ctime = di->i_ctime;

      /*      st->st_mode = di->i_mode & ~S_ITRANS;*/
      st->st_mode = di->i_mode;
      st->st_uid = di->i_uid;
      st->st_gid = di->i_gid;
      st->st_author = st->st_uid;
      np->author_tracks_uid = 1;

      /* Set to a conservative value.  */
      dn->last_page_partially_writable = 0;

      if (S_ISCHR (st->st_mode) || S_ISBLK (st->st_mode))
	st->st_rdev = di->i_zone[0];
      else
	{
	  memcpy (info->i_data.i2, di->i_zone,
		  sblock_info->s_n_zones * sizeof (info->i_data.i2[0]));
	  st->st_rdev = 0;
	}
    }

  diskfs_end_catch_exception ();

  if (S_ISREG (st->st_mode) || S_ISDIR (st->st_mode)
      || (S_ISLNK (st->st_mode)))
    {
      unsigned offset;

      np->allocsize = np->dn_stat.st_size;

      /* Round up to a block multiple.  */
      offset = np->allocsize & (block_size - 1);
      if (offset > 0)
	np->allocsize += block_size - offset;
    }
  else
    /* Allocsize should be zero for anything except directories, files, and
       long symlinks.  These are the only things allowed to have any blocks
       allocated as well, although st_size may be zero for any type (cases
       where st_blocks=0 and st_size>0 include fast symlinks, and, under
       linux, some devices).  */
    {
      assert (np->dn_stat.st_size == 0);
      np->allocsize = 0;
    }

  st->st_blocks = np->allocsize
    >> (log2_block_size - log2_stat_blocks_per_fs_block);

  return 0;
}

/* Return 0 if NP's owner can be changed to UID; otherwise return an error
   code.  */
error_t
diskfs_validate_owner_change (struct node *np, uid_t uid)
{
  return ((uid & ~0xFFFF) == 0 ? 0 : EINVAL);
}

/* Return 0 if NP's group can be changed to GID; otherwise return an error
   code.  */
error_t
diskfs_validate_group_change (struct node *np, gid_t gid)
{
  if (sblock_info->s_version == MINIX_V1)
    return ((gid & ~0x00FF) == 0 ? 0 : EINVAL);
  else
    return ((gid & ~0xFFFF) == 0 ? 0 : EINVAL);
}

/* Return 0 if NP's mode can be changed to MODE; otherwise return an error
   code.  It must always be possible to clear the mode; diskfs will not ask
   for permission before doing so.  */
error_t
diskfs_validate_mode_change (struct node *np, mode_t mode)
{
  return ((mode & ~0xFFFF) == 0 ? 0 : EINVAL);
}

/* Return 0 if NP's author can be changed to AUTHOR; otherwise return an
   error code.  */
error_t
diskfs_validate_author_change (struct node *np, uid_t author)
{
  /* For the MINIX file system, the author & owner are the same.  */
  return (author == np->dn_stat.st_uid) ? 0 : EINVAL;
}

/* The user may define this function.  Return 0 if NP's flags can be
   changed to FLAGS; otherwise return an error code.  It must always
   be possible to clear the flags.  */
error_t
diskfs_validate_flags_change (struct node *np, int flags)
{
  /* No flags are allowed in a MINIX inode!  */
  return EINVAL;
}

/* Writes everything from NP's inode to the disk image, and returns a pointer
   to it, or NULL if nothing need be done.  */
static struct minix2_inode *
write_node (struct node *np)
{
  error_t err;
  struct stat *st = &np->dn_stat;

  if (np->dn_stat_dirty)
    {
      assert (!diskfs_readonly);

      minixfs_debug ("writing inode %d to disk", np->cache_id);

      err = diskfs_catch_exception ();
      if (err)
	return NULL;

      /* No Hurd extensions should be turned on.  */
      assert ((st->st_uid & ~0xFFFF) == 0);
      assert ((st->st_mode & ~0xFFFF) == 0);
      assert (np->author_tracks_uid && st->st_author == st->st_uid);

      if (sblock_info->s_version == MINIX_V1)
	{
	  struct minix_inode *di = dino_v1 (np->cache_id);

	  assert ((st->st_gid & ~0x00FF) == 0);

	  /*di->i_mode = st->st_mode & 0xFFFF & ~S_ITRANS;*/
	  di->i_mode = st->st_mode;
	  di->i_uid = st->st_uid;
	  di->i_gid = st->st_gid;

	  di->i_nlinks = st->st_nlink;
	  di->i_time = st->st_mtime; /* XXX */

	  if (st->st_mode == 0)
	    di->i_size = 0; /* XXX */
	  else
	    di->i_size = st->st_size;

	  if (S_ISCHR(st->st_mode) || S_ISBLK(st->st_mode))
	    di->i_zone[0] = st->st_rdev;
	  else
	    memcpy (di->i_zone, np->dn->info.i_zone_V1,
		    sblock_info->s_n_zones * sizeof (di->i_zone[0]));

	  diskfs_end_catch_exception ();

	  np->dn_stat_dirty = 0;

	  return (struct minix2_inode *) di;
	}
      else
	{
	  struct minix2_inode *di = dino_v2 (np->cache_id);

	  assert ((st->st_gid & ~0xFFFF) == 0);

	  /*di->i_mode = st->st_mode & 0xFFFF & ~S_ITRANS;*/
	  di->i_mode = st->st_mode;
	  di->i_uid = st->st_uid;
	  di->i_gid = st->st_gid;
	  di->i_nlinks = st->st_nlink;
	  di->i_atime = st->st_atime;
	  di->i_mtime = st->st_mtime;
	  di->i_ctime = st->st_ctime;

	  if (st->st_mode == 0)
	    di->i_size = 0; /* XXX */
	  else
	    di->i_size = st->st_size;

	  if (S_ISCHR(st->st_mode) || S_ISBLK(st->st_mode))
	    di->i_zone[0] = st->st_rdev;
	  else
	    memcpy (di->i_zone, np->dn->info.i_zone_V2,
		    sblock_info->s_n_zones * sizeof (di->i_zone[0]));

	  diskfs_end_catch_exception ();

	  np->dn_stat_dirty = 0;

	  return (struct minix2_inode *) di;
	}
    }
  else
    return NULL;
}

/* Reload all data specific to NODE from disk, without writing anything.
   Always called with DISKFS_READONLY true.  */
error_t
diskfs_node_reload (struct node *node)
{
  struct disknode *dn = node->dn;

  if (dn->dirents)
    {
      free (dn->dirents);
      dn->dirents = 0;
    }
  pokel_flush (&dn->indir_pokel);
  flush_node_pager (node);
  read_node (node);

  return 0;
}

/* For each active node, call FUN.  The node is to be locked around the call
   to FUN.  If FUN returns non-zero for any node, then immediately stop, and
   return that value.  */
error_t
diskfs_node_iterate (error_t (*fun)(struct node *))
{
  error_t err = 0;
  int n, num_nodes = 0;
  struct node *node, **node_list, **p;

  spin_lock (&diskfs_node_refcnt_lock);

  /* We must copy everything from the hash table into another data structure
     to avoid running into any problems with the hash-table being modified
     during processing (normally we delegate access to hash-table with
     diskfs_node_refcnt_lock, but we can't hold this while locking the
     individual node locks).  */

  for (n = 0; n < INOHSZ; n++)
    for (node = nodehash[n]; node; node = node->dn->hnext)
      num_nodes++;

  node_list = alloca (num_nodes * sizeof (struct node *));
  p = node_list;
  for (n = 0; n < INOHSZ; n++)
    for (node = nodehash[n]; node; node = node->dn->hnext)
      {
	*p++ = node;
	node->references++;
      }

  spin_unlock (&diskfs_node_refcnt_lock);

  p = node_list;
  while (num_nodes-- > 0)
    {
      node = *p++;
      if (!err)
	{
	  mutex_lock (&node->lock);
	  err = (*fun)(node);
	  mutex_unlock (&node->lock);
	}
      diskfs_nrele (node);
    }

  return err;
}

/* Write all active disknodes into the minix_inode pager. */
void
write_all_disknodes ()
{
  error_t write_one_disknode (struct node *node)
    {
      /* Sync the indirect blocks here; they'll all be done before any
	 inodes.  Waiting for them shouldn't be too bad.  */
      pokel_sync (&node->dn->indir_pokel, 1);

      diskfs_set_node_times (node);

      /* Update the inode image.  */
      if (sblock_info->s_version == MINIX_V1)
	{
	  struct minix_inode *di = (struct minix_inode *) write_node (node);
	  if (di)
	    record_global_poke (di);
	}
      else
	{
	  struct minix2_inode *di = (struct minix2_inode *) write_node (node);
	  if (di)
	    record_global_poke (di);
	}

      return 0;
    }

  diskfs_node_iterate (write_one_disknode);
}

/* Sync the info in NP->dn_stat and any associated format-specific
   information to disk.  If WAIT is true, then return only after the
   physicial media has been completely updated.  */
void
diskfs_write_disknode (struct node *np, int wait)
{
  if (sblock_info->s_version == MINIX_V1)
    {
      struct minix_inode *di = (struct minix_inode *) write_node (np);
      if (di)
	{
	  if (wait)
	    sync_global_ptr (di, 1);
	  else
	    record_global_poke (di);
	}
    }
  else
    {
      struct minix2_inode *di = (struct minix2_inode *) write_node (np);
      if (di);
	{
	  if (wait)
	    sync_global_ptr (di, 1);
	  else
	    record_global_poke (di);
	}
    }
}

/* Set *ST with appropriate values to reflect the current state of the
   filesystem.  */
error_t
diskfs_set_statfs (struct statfs *st)
{
  /* XXX perhaps lock */
  st->f_type = FSTYPE_MINIXFS;
  st->f_bsize = block_size;
  st->f_blocks = ((sblock_info->s_nzones - sblock->s_firstdatazone)
		  << log2_fs_blocks_per_zone);
  st->f_bfree = sblock_info->s_free_zones_count << log2_fs_blocks_per_zone;
  st->f_bavail = st->f_bfree;
  st->f_files = sblock->s_ninodes;
  st->f_ffree = sblock_info->s_free_inodes_count;
  st->f_fsid = getpid ();
  /* st->f_namelen is set by the library XXX */
  st->f_favail = st->f_ffree;
  /* st->f_frsize = st->f_bsize; XXX */
  return 0;
}

/* Implement the diskfs_set_translator callback from the diskfs
   library; see <hurd/diskfs.h> for the interface description. */
error_t
diskfs_set_translator (struct node *np, const char *name, unsigned namelen,
		       struct protid *cred)
{
  assert (! diskfs_readonly);
  /* No Hurd-specific feature can be supported by the MINIX filesystem. */
  return EOPNOTSUPP;
}

/* Implement the diskfs_get_translator callback from the diskfs library.
   See <hurd/diskfs.h> for the interface description. */
error_t
diskfs_get_translator (struct node *np, char **namep, unsigned *namelen)
{
  /* This function should be never called, because no Hurd-specific feature
     is supported by the MINIX file system.  */
  assert (0);
  return EOPNOTSUPP;
}

/* Called when all hard ports have gone away.  */
void
diskfs_shutdown_soft_ports ()
{
  /* Should initiate termination of internally held pager ports
     (the only things that should be soft) XXX */
}
