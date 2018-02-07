/* Zone allocation routines (from `hurd/ext2fs/balloc.c').

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

/*
 *  linux/fs/minix/bitmap.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

/*#include <string.h>*/
#include "minixfs.h"
#include "bitmap.c"

void
minixfs_free_zones (zone_t zone, unsigned long count)
{
  assert (!diskfs_readonly);

  spin_lock (&global_lock);

  if (zone < sblock->s_firstdatazone || zone + count >= sblock_info->s_nzones)
    {
      minixfs_error ("trying to free zones not in datazone [%u-%u] - "
		     "zone %u, count %lu", (zone_t) sblock->s_firstdatazone,
		     sblock_info->s_nzones, zone, count);
      spin_unlock (&global_lock);
      return;
    }

  minixfs_debug ("freeing zones %u[%lu]", zone, count);

  zone -= sblock->s_firstdatazone + 1;

  do
    {
      unsigned long bcount = ((zone + count) & (BITS_PER_BLOCK - 1)) - zone;
      block_t zmap_block = zone >> LOG2_BITS_PER_BLOCK;
      unsigned long bit = zone & (BITS_PER_BLOCK - 1);
      char *bh;
      int i;

      if (zmap_block >= sblock->s_zmap_blocks)
	{
	  minixfs_error ("nonexistent zmap in superblock: %u", zmap_block);
	  spin_unlock (&global_lock);
	  return;
	}

      bh = bptr (Z_MAP_BOFFS + zmap_block); 
      for (i = 0; i < bcount; i++)
	{
	  if (! clear_bit (bit + i, bh))
	    minixfs_warning ("bit already cleared for zone %u",
			     (zone_t) (bit + i + sblock->s_firstdatazone - 1));
	  else
	    sblock_info->s_free_zones_count++;
	}

      record_global_poke (bh);
      zone += bcount;
      count -= bcount;
    } while (count > 0);

  spin_unlock (&global_lock);
  alloc_sync (0);
}

/* Allocate a new zone for the file NODE, and return it, or 0 if none
   could be had.  If ZERO is true, then zero the zone (and add it to
   NODE's list of modified indirect blocks).  */
zone_t
minixfs_new_zone (struct node *node, int zero)
{
  char *bh = bptr (Z_MAP_BOFFS);
  zone_t zone;
  unsigned long bits = sblock_info->s_nzones - sblock->s_firstdatazone;

  spin_lock (&global_lock);

 repeat:
  zone = find_first_zero_bit ((unsigned long *) bh, bits);
  if (zone < bits)
    {
      assert (sblock_info->s_free_zones_count > 0);
      minixfs_debug ("eligible bit found at position %u", zone);

      if (set_bit (zone, bh))
	{
	  /* shouldn't happen */
	  minixfs_warning ("bit already set for zone %u",
			   zone + sblock->s_firstdatazone - 1);
	  goto repeat;
	}
      record_global_poke (bh);
    }
  else
    {
      assert (sblock_info->s_free_zones_count == 0);
      minixfs_debug ("no more free zones");
      zone = 0;
      goto sync_out;
    }

  zone += sblock->s_firstdatazone - 1;
  if (zone < sblock->s_firstdatazone)
    {
      minixfs_error ("trying to allocate zone outsize datazone [%u-%u] - "
		     "zone = %u", (zone_t) sblock->s_firstdatazone,
		     sblock_info->s_nzones, zone);
      zone = 0;
      goto sync_out;
    }

  /* Since due to bletcherousness block-modified bits are never turned off
     when writing disk-pager pages, make sure they are here, in case this
     block is being allocated to a file (see pager.c).  */
  if (modified_global_blocks)
    {
      block_t tmp = zone << log2_fs_blocks_per_zone;
      int i;
      
      spin_lock (&modified_global_blocks_lock);
      for (i = 0; i < 1 << log2_fs_blocks_per_zone; i++)
	clear_bit (tmp, modified_global_blocks);
      spin_unlock (&modified_global_blocks_lock);
    }

  sblock_info->s_free_zones_count--;
  if (zero)
    {
      char *zonep = zptr (zone);
      int i;

      bzero (zonep, zone_size);
      for (i = 0; i < 1 << log2_fs_blocks_per_zone; i++)
	record_indir_poke (node, zonep + (i << log2_block_size));
    }
      
 sync_out:
  spin_unlock (&global_lock);
  alloc_sync (0);

  return zone;
}

unsigned long 
minixfs_count_free_zones ()
{
  unsigned long bitmap_count;

  /*spin_lock (&global_lock);*/

  bitmap_count = count_free (bptr (Z_MAP_BOFFS),
			     sblock->s_zmap_blocks << log2_block_size);

  /*spin_unlock (&global_lock);*/

  /*bitmap_count <<= sblock->s_log_zone_size;*/
  return bitmap_count;
}
