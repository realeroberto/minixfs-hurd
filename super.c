/* Fetching and storing the hypermetadata (superblock info) (from
   `hurd/ext2fs/hyper.c').

   Copyright (C) 1994,95,96,99,2001,02 Free Software Foundation, Inc.
   Written by Miles Bader <miles@gnu.org>

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

#include <string.h>
#include <stdio.h>
#include <error.h>
#include "minixfs.h"

vm_address_t zerozone;
char *modified_global_blocks;

static void
allocate_mod_map (void)
{
  static vm_size_t mod_map_size;

  if (modified_global_blocks && mod_map_size)
    /* Get rid of the old one.  */
    munmap (modified_global_blocks, mod_map_size);

 if (!diskfs_readonly && block_size < vm_page_size)
    /* If the block size is too small, we have to take extra care when
       writing out pages from the global pager, to make sure we don't
       stomp on any file pager blocks.  In this case use a bitmap to
       record which global blocks are actually modified so the pager
       can write only them.  */
    {
      /* One bit per file system block.  */
      mod_map_size = ((sblock_info->s_nzones << log2_fs_blocks_per_zone)
		      / CHAR_BIT);
      modified_global_blocks = mmap (0, mod_map_size, PROT_READ|PROT_WRITE,
				     MAP_ANON, 0, 0);
      assert (modified_global_blocks != (void *) -1);
    }
  else
    modified_global_blocks = 0;
}

unsigned int sblock_block = SUPER_BLOCK_BOFFS;

static int minixfs_clean;	/* fs clean before we started writing? */

void
get_hypermetadata (void)
{
  error_t err = diskfs_catch_exception ();
  if (err)
    minixfs_panic ("can't read superblock: %s", strerror (err));

  if (! block_size)
    block_size = DEFAULT_BLOCK_SIZE;

  log2_block_size = 0;
  while ((1 << log2_block_size) < block_size)
    log2_block_size++;

  sblock = (struct minix_super_block *) bptr (sblock_block);

  spin_lock (&global_lock);

  sblock_info = (struct minix_super_block_info *)
    malloc (sizeof (struct minix_super_block_info));
  if (! sblock_info)
    {
      spin_unlock (&global_lock);
      minixfs_panic ("no more memory!");
    }
    
  if (sblock->s_magic == MINIX_SUPER_MAGIC)
    {
      sblock_info->s_version = MINIX_V1;
      sblock_info->s_dirsize = 16;
      sblock_info->s_namelen = 14;
    } 
  else if (sblock->s_magic == MINIX_SUPER_MAGIC2)
    {    
      sblock_info->s_version = MINIX_V1;
      sblock_info->s_dirsize = 32;
      sblock_info->s_namelen = 30;
    } 
  else if (sblock->s_magic == MINIX2_SUPER_MAGIC)
    {
      sblock_info->s_version = MINIX_V2;
      sblock_info->s_dirsize = 16;
      sblock_info->s_namelen = 14;
    } 
  else if (sblock->s_magic == MINIX2_SUPER_MAGIC2)
    {
      sblock_info->s_version = MINIX_V2;
      sblock_info->s_dirsize = 32;
      sblock_info->s_namelen = 30;
    } 
  else
    minixfs_panic ("bad magic number %#x", sblock->s_magic);

  sblock_info->s_ndzones = 7;

  if (sblock_info->s_version == MINIX_V1)
    {
      sblock_info->s_nzones = sblock->s_nzones;
      sblock_info->s_link_max = MINIX_LINK_MAX;
      sblock_info->s_inodes_per_block = 
	block_size / sizeof (struct minix_inode);
      sblock_info->s_nindirs = block_size / sizeof (uint16_t);
      /* XXX No triple indirection in V1 inodes.  */
      sblock_info->s_has_tind = 0;
      sblock_info->s_n_zones = MINIXFS_DIND_ZONE + 1;
    }
  else
    {
      sblock_info->s_nzones = sblock->s_zones;
      sblock_info->s_link_max = MINIX2_LINK_MAX;
      sblock_info->s_inodes_per_block = 
	block_size / sizeof (struct minix2_inode);
      sblock_info->s_nindirs = block_size / sizeof (uint32_t);
      sblock_info->s_has_tind = 1;
      sblock_info->s_n_zones = MINIXFS_TIND_ZONE + 1;
    }

  sblock_info->s_dir_slots_per_fs_block = block_size / sblock_info->s_dirsize;
  sblock_info->s_has_dind = 1; /* XXX this is not correct for all versions */
  sblock_info->s_max_addressable_zone = MINIXFS_NDIR_ZONES + /* XXX */
    ADDR_PER_BLOCK + ADDR_PER_BLOCK * ADDR_PER_BLOCK +
    ADDR_PER_BLOCK * ADDR_PER_BLOCK * ADDR_PER_BLOCK;

  sblock_info->s_free_inodes_count = minixfs_count_free_inodes ();
  sblock_info->s_free_zones_count = minixfs_count_free_zones ();

  spin_unlock (&global_lock);

  log2_dev_blocks_per_fs_block = log2_block_size - store->log2_block_size;
  if (log2_dev_blocks_per_fs_block < 0)
    minixfs_panic ("block size %zd isn't a power-of-two multiple of the device"
		   " block size (%zd)!",
		   block_size, store->block_size);

  log2_fs_blocks_per_zone = sblock->s_log_zone_size;
  if (log2_fs_blocks_per_zone < 0)
    minixfs_panic ("zone size %zd isn't a power-of-two multiple of the "
		   "file system block size (%zd)!",
		   zone_size, block_size);

  log2_zone_size = log2_block_size + log2_fs_blocks_per_zone;
  zone_size = 1 << log2_zone_size;

  log2_stat_blocks_per_fs_block = 0;
  while ((512 << log2_stat_blocks_per_fs_block) < block_size)
    log2_stat_blocks_per_fs_block++;
  if ((512 << log2_stat_blocks_per_fs_block) != block_size)
    minixfs_panic ("block size %zd isn't a power-of-two multiple of 512!",
		   block_size);

  if ((store->size >> log2_zone_size) < sblock_info->s_nzones)
    minixfs_panic ("disk size (%qd bytes) too small; superblock says"
		   " we need %qd",
		   (long long int) store->size,
		   (long long int) sblock_info->s_nzones << log2_zone_size);
  if (log2_dev_blocks_per_fs_block != 0
      && (store->size & ((1 << log2_dev_blocks_per_fs_block) - 1)) != 0)
    minixfs_warning ("%Ld (%zd byte) device blocks "
		     " unused after last file system (%zd byte) block",
		     (store->size & ((1 << log2_dev_blocks_per_fs_block) - 1)),
		     store->block_size, block_size);

  minixfs_clean = sblock->s_state & MINIX_VALID_FS;

  minixfs_debug ("MINIX file system version %s, %d char names, "
		 "with %s indirection; "
		 "block size %zd, zone size %zd, device block size %zd, "
		 "%u zones (%u free), %u blocks (%u free), "
		 "%lu inodes (%lu free), %d inodes per block, "
		 "max file size %zd, max addressable zone %u, "
		 "%d block(s) in inode bitmap, %d block(s) in zone bitmap, "
		 "firstdatazone %u.",
		 sblock_info->s_version == MINIX_V1 ? "1" : "2",
		 sblock_info->s_namelen,
		 (sblock_info->s_has_dind
		  ? (sblock_info->s_has_tind
		     ? "both double and triple" : "double, but without triple")
		  : "neither double nor triple"),
		 block_size, zone_size, store->block_size,
		 sblock_info->s_nzones, sblock_info->s_free_zones_count,
		 sblock_info->s_nzones << log2_fs_blocks_per_zone,
		 sblock_info->s_free_zones_count << log2_fs_blocks_per_zone,
		 (unsigned long) sblock->s_ninodes,
		 sblock_info->s_free_inodes_count,
		 sblock_info->s_inodes_per_block,
		 sblock->s_max_size, sblock_info->s_max_addressable_zone,
		 sblock->s_imap_blocks, sblock->s_zmap_blocks,
		 (zone_t) sblock->s_firstdatazone);

  if (! minixfs_clean)
    {
      minixfs_warning ("FILE SYSTEM NOT UNMOUNTED CLEANLY; PLEASE fsck");
      if (! diskfs_readonly)
	{
	  diskfs_readonly = 1;
	  minixfs_warning ("MOUNTED READ-ONLY; MUST USE `fsysopts --writable'");
	}
    }
      
  allocate_mod_map ();

  diskfs_end_catch_exception ();

  /* A handy source of page-aligned zeros.  */
  if (zerozone == 0)
    zerozone = (vm_address_t) mmap (0, zone_size, PROT_READ, MAP_ANON, 0, 0);
}

error_t
diskfs_set_hypermetadata (int wait, int clean)
{
  if (clean && minixfs_clean && !(sblock->s_state & MINIX_VALID_FS))
    /* The filesystem is clean, so we need to set the clean flag.  */
    {
      sblock->s_state |= MINIX_VALID_FS;
      sblock_dirty = 1;
    }
  else if (!clean && (sblock->s_state & MINIX_VALID_FS))
    /* The file system just became dirty, so clear the clean flag.  */
    {
      sblock->s_state &= ~MINIX_VALID_FS;
      sblock_dirty = 1;
      wait = 1;
    }

 if (sblock_dirty)
   {
     sblock_dirty = 0;
     record_global_poke (sblock);
   }

  sync_global (wait);

  /* Should check writability here and return EROFS if necessary. XXX */
  return 0;
}

void
diskfs_readonly_changed (int readonly)
{
  allocate_mod_map ();

  (*(readonly ? store_set_flags : store_clear_flags)) (store, STORE_READONLY);

  mprotect (disk_image, store->size, PROT_READ | (readonly ? 0 : PROT_WRITE));

  if (!readonly && !(sblock->s_state & MINIX_VALID_FS))
    minixfs_warning ("UNCLEANED FILE SYSTEM NOW WRITABLE");
}
