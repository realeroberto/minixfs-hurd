/* Inode allocation routines.

   Copyright (C) 1995 Free Software Foundation, Inc.

   Converted to work under the hurd by Miles Bader <miles@gnu.org>

   Modified for the MINIX file system from the original for the ext2
   file system by Roberto Reale <rober.reale@gmail.com>.

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

#include "minixfs.h"
#include "bitmap.c"

/*
 *  linux/fs/minix/bitmap.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

/* Free node NP; the on disk copy has already been synced with
   diskfs_node_update (where NP->dn_stat.st_mode was 0).  Its
   mode used to be OLD_MODE.  */
void
diskfs_free_node (struct node *np, mode_t old_mode)
{
  char *bh;
  ino_t inum = np->cache_id;
  block_t imap_block;

  assert (!diskfs_readonly);

  spin_lock (&global_lock);

  if (inum < MINIXFS_FIRST_INO || inum > sblock->s_ninodes)
    {
      minixfs_error ("trying to free a reserved or nonexistent inode: %Lu",
		     inum);
      spin_unlock (&global_lock);
      return;
    }

  minixfs_debug ("freeing inode %Lu", inum);

  imap_block = inum >> LOG2_BITS_PER_BLOCK;
  if (imap_block >= sblock->s_imap_blocks) 
    {
      minixfs_error ("nonexistent imap in superblock: %u", imap_block);
      spin_unlock (&global_lock);
      return;
    }
  
  bh = bptr (I_MAP_BOFFS + imap_block);
  if (! clear_bit (inum & (BITS_PER_BLOCK - 1), bh))
    minixfs_warning ("bit already cleared for inode %Lu", inum);
  else
    {
      record_global_poke (bh);
      sblock_info->s_free_inodes_count++;
    }

  spin_unlock (&global_lock);
  alloc_sync (0);
}

ino_t
minixfs_new_inode (void)
{
  char *bh = bptr (I_MAP_BOFFS);
  ino_t inum;
  unsigned long bits = sblock->s_ninodes;

  spin_lock (&global_lock);

 repeat:
  inum = find_first_zero_bit ((unsigned long *) bh, bits);
  if (inum < bits)
    {
      assert (sblock_info->s_free_inodes_count > 0);
      minixfs_debug ("eligible bit found at position %Lu", inum);

      if (set_bit (inum, bh))
	{
	  /* shouldn't happen */
	  minixfs_warning ("bit already set for inode %Lu", inum);
	  goto repeat;
	}
      record_global_poke (bh);
    }
  else
    {
      assert (sblock_info->s_free_inodes_count == 0);
      minixfs_debug ("no more free inodes");
      inum = 0;
      goto sync_out;
    }

  if (inum < MINIXFS_FIRST_INO)
    {
      minixfs_error ("reserved inode");
      inum = 0;
      goto sync_out;
    }

  sblock_info->s_free_inodes_count--;

 sync_out:
  spin_unlock (&global_lock);
  alloc_sync (0);

  return inum;
}

/* ---------------------------------------------------------------- */

/* The user must define this function.  Allocate a new node to be of
   mode MODE in locked directory DP (don't actually set the mode or
   modify the dir, that will be done by the caller); the user
   responsible for the request can be identified with CRED.  Set *NP
   to be the newly allocated node.  */
error_t
diskfs_alloc_node (struct node *dir, mode_t mode, struct node **node)
{
  error_t err;
  int zone_ptr, nzones = sblock_info->s_n_zones;
  struct node *np;
  struct stat *st;
  ino_t inum;

  assert (!diskfs_readonly);

  inum = minixfs_new_inode ();

  if (inum == 0)
    return ENOSPC;

  err = diskfs_cached_lookup (inum, &np);
  if (err)
    return err;

  st = &np->dn_stat;

  if (st->st_blocks)
    {
      st->st_blocks = 0;
      np->dn_set_ctime = 1;
    }

  /* Zero out the zone pointers in case there's some noise left on disk.  */
  if (sblock_info->s_version == MINIX_V1)
    {
      for (zone_ptr = 0; zone_ptr < nzones; zone_ptr++)
	if (np->dn->info.i_zone_V1[zone_ptr] != 0)
	  {
	    np->dn->info.i_zone_V1[zone_ptr] = 0;
	    np->dn_set_ctime = 1;
	  }
    }
  else
    {    
      for (zone_ptr = 0; zone_ptr < nzones; zone_ptr++)
	if (np->dn->info.i_zone_V2[zone_ptr] != 0)
	  {
	    np->dn->info.i_zone_V2[zone_ptr] = 0;
	    np->dn_set_ctime = 1;
	  }
    }

/*   if (np->dn->info_i_translator != 0) */
/*     { */
/*       np->dn->info_i_translator = 0; */
/*       np->dn_set_ctime = 1; */
/*     } */
/*   st->st_mode &= ~S_IPTRANS; */
  if (np->allocsize)
    {
      st->st_size = 0;
      np->allocsize = 0;
      np->dn_set_ctime = 1;
    }

  st->st_flags = 0;
  st->st_gen = 0;

  alloc_sync (np);

  *node = np;
  return 0;
}

/* ---------------------------------------------------------------- */

unsigned long
minixfs_count_free_inodes ()
{
  unsigned long bitmap_count;

  /*spin_lock (&global_lock);*/

  bitmap_count = count_free (bptr (I_MAP_BOFFS),
			     sblock->s_imap_blocks << log2_block_size);

  /*spin_unlock (&global_lock);*/

  return bitmap_count;
}
