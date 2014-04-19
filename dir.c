/* Directory management routines

   Copyright (C) 1994,95,96,97,98,99,2000,01,02 Free Software Foundation, Inc.
   Converted for ext2fs by Miles Bader <miles@gnu.org>

   Modified for the MINIX file system from the original for the ext2
   file system by Roberto Reale <rreale@iol.it>.

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
#include <dirent.h>
#include <stddef.h>
#include "minixfs.h"

#define FSBLK_TRUNC(size) ((size) >> log2_block_size)
#define FSBLK_ROUND(size)						\
  FSBLK_TRUNC((((size) + (block_size - 1)) & ~(block_size - 1)))

enum slot_status
{
  /* This means we haven't yet found room for a new entry.  */
  LOOKING,

  /* This means that the specified entry is free and should be used.  */
  TAKE,

  /* This means that the directory will have to be grown to hold the
     entry.  */
  EXTEND,

  /* For removal and rename, this means that this is the location
     of the entry found.  */
  HERE_TIS,
};

static const char *lookup_type_description[] =
{
  [LOOKUP]	= "LOOKUP",
  [CREATE]	= "CREATE",
  [REMOVE]	= "REMOVE",
  [RENAME]	= "RENAME",
};
#define LOOKUP_TYPE_MAX (sizeof lookup_type_description		\
  			 / sizeof lookup_type_description[0])

struct dirstat
{
  /* Type of followp operation expected.  */
  enum lookup_type type;

  /* One of the statuses above.  */
  enum slot_status stat;

  /* Mapped address and length of directory.  */
  vm_address_t mapbuf;
  vm_size_t mapextent;

  /* Index of this directory block.  */
  int idx;

  /* For stat HERE_TIS and TAKE, these are the entry referenced and its
     index into this directory block, respectively.  */
  struct minix_dir_entry *entry;
  int entry_index;

  /* For stat HERE_TIS, type REMOVE, this is the address of the immediately
     previous direct in this directory block, or zero if this is the first.  */
  struct minix_dir_entry *preventry;
};

const size_t diskfs_dirstat_size = sizeof (struct dirstat);

/* Initialize DS such that diskfs_drop_dirstat will ignore it.  */
void
diskfs_null_dirstat (struct dirstat *ds)
{
  ds->type = LOOKUP;
}

static error_t
dirscanblock (vm_address_t blockoff, size_t amt, struct node *dp, int idx,
	      const char *name, int namelen, enum lookup_type type,
	      struct dirstat *ds, ino_t *inum);

/* XXX comment and rewrite */
static inline size_t
direntry_len (char *s)
{
  char *p = memchr (s, '\0', sblock_info->s_namelen);

  if (! p)
    return sblock_info->s_namelen;

#ifdef MINIXFS_ROBUST
  /* XXX comment */
  if (minix_robust_flag)
    {
      for (;
	   p <= s + sblock_info->s_namelen && ! *p;
	   p++)
	;
      if (p <= s + sblock_info->s_namelen)
	minixfs_warning ("corrupted directory entry: garbage after "
			 "the terminating NUL char: inode: %Ld "
			 "offset: %zd",
			 dp->cache_id, curoff);
    }
#endif

  return p - s;
}

/* Implement the diskfs_lookup from the diskfs library.  See
   <hurd/diskfs.h> for the interface specification.  */
error_t
diskfs_lookup_hard (struct node *dp, const char *name, enum lookup_type type,
		    struct node **npp, struct dirstat *ds, struct protid *cred)
{
  error_t err;
  ino_t inum;
  int namelen;
  int spec_dotdot;
  struct node *np = 0;
  int retry_dotdot = 0;
  vm_prot_t prot =
    (type == LOOKUP) ? VM_PROT_READ : (VM_PROT_READ | VM_PROT_WRITE);
  memory_object_t memobj;
  vm_address_t buf = 0;
  vm_size_t buflen = 0;
  int blockaddr;
  int idx, lastidx;
  int looped;

  minixfs_debug ("looking up name '%s' (operation %s)", name,
		 (type < LOOKUP_TYPE_MAX ? lookup_type_description[type]
		  : "UNDEFINED"));

  if ((type == REMOVE) || (type == RENAME))
    assert (npp);

  if (npp)
    *npp = 0;

  spec_dotdot = type & SPEC_DOTDOT;
  type &= ~SPEC_DOTDOT;

  namelen = strlen (name);

  if (namelen > sblock_info->s_namelen)
    {
      if (ds)
	diskfs_null_dirstat (ds);
      return ENAMETOOLONG;
    }

 try_again:
  if (ds)
    {
      ds->type = LOOKUP;
      ds->mapbuf = 0;
      ds->mapextent = 0;
    }
  if (buf)
    {
      munmap ((caddr_t) buf, buflen);
      buf = 0;
    }
  if (ds && (type == CREATE || type == RENAME))
    ds->stat = LOOKING;

  /* Map in the directory contents.  */
  memobj = diskfs_get_filemap (dp, prot);

  if (memobj == MACH_PORT_NULL)
    return errno;

  buf = 0;
  /* We allow extra space in case we have to do an EXTEND.  */
  buflen = round_page (dp->dn_stat.st_size + block_size);
  err = vm_map (mach_task_self (),
		&buf, buflen, 0, 1, memobj, 0, 0, prot, prot, 0);
  mach_port_deallocate (mach_task_self (), memobj);

  inum = 0;

  if (!diskfs_check_readonly ())
    dp->dn_set_atime = 1;

  /* Start the lookup at DP->dn->dir_idx.  */
  idx = dp->dn->dir_idx;
  if (idx << log2_block_size > dp->dn_stat.st_size)
    idx = 0;			/* just in case */
  blockaddr = buf + (idx << log2_block_size);
  looped = (idx == 0);
  lastidx = idx;
  if (lastidx == 0)
    lastidx = FSBLK_ROUND (dp->dn_stat.st_size);

  minixfs_debug ("starting the lookup at block index %d", idx);

  while (!looped || idx < lastidx)
    {
      size_t amt;

      if (type != CREATE && idx == lastidx - 1)
	amt = dp->dn_stat.st_size & (block_size - 1);
      else
	amt = block_size;

      err = dirscanblock (blockaddr, amt, dp, idx, name, namelen,
			  type, ds, &inum);
      if (!err)
	{
	  dp->dn->dir_idx = idx;
	  break;
	}
      if (err != ENOENT)
	{
	  munmap ((caddr_t) buf, buflen);
	  return err;
	}

      blockaddr += amt;
      idx++;
      if (blockaddr - buf >= dp->dn_stat.st_size && !looped)
	{
	  /* We've gotten to the end; start back at the beginning */
	  looped = 1;
	  blockaddr = buf;
	  idx = 0;
	}
    }

  if (!diskfs_check_readonly ())
    dp->dn_set_atime = 1;
  if (diskfs_synchronous)
    diskfs_node_update (dp, 1);

  /* If err is set here, it's ENOENT, and we don't want to
     think about that as an error yet. */
  err = 0;

  if (inum && npp)
    {
      if (namelen != 2 || name[0] != '.' || name[1] != '.')
	{
	  if (inum == dp->cache_id)
	    {
	      np = dp;
	      diskfs_nref (np);
	    }
	  else
	    {
	      err = diskfs_cached_lookup (inum, &np);
	      if (err)
		goto out;
	    }
	}

      /* We are looking up .. */
      /* Check to see if this is the root of the file system.  */
      else if (dp->cache_id == MINIXFS_ROOT_INO)
	{
	  err = EAGAIN;
	  goto out;
	}

      /* We can't just do diskfs_cached_lookup, because we would then deadlock.
	 So we do this.  Ick.  */
      else if (retry_dotdot)
	{
	  /* Check to see that we got the same answer as last time.  */
	  if (inum != retry_dotdot)
	    {
	      /* Drop what we *thought* was .. (but isn't any more) and
		 try *again*.  */
	      diskfs_nput (np);
	      mutex_unlock (&dp->lock);
	      err = diskfs_cached_lookup (inum, &np);
	      mutex_lock (&dp->lock);
	      if (err)
		goto out;
	      retry_dotdot = inum;
	      goto try_again;
	    }
	  /* Otherwise, we got it fine and np is already set properly.  */
	}
      else if (!spec_dotdot)
	{
	  /* Lock them in the proper order, and then
	     repeat the directory scan to see if this is still
	     right.  */
	  mutex_unlock (&dp->lock);
	  err = diskfs_cached_lookup (inum, &np);
	  mutex_lock (&dp->lock);
	  if (err)
	    goto out;
	  retry_dotdot = inum;
	  goto try_again;
	}

      /* Here below are the spec dotdot cases.  */
      else if (type == RENAME || type == REMOVE)
	np = ifind (inum);

      else if (type == LOOKUP)
	{
	  diskfs_nput (dp);
	  err = diskfs_cached_lookup (inum, &np);
	  if (err)
	    goto out;
	}
      else
	assert (0);
    }

  if ((type == CREATE || type == RENAME) && !inum && ds && ds->stat == LOOKING)
    {
      /* We didn't find any room, so mark ds to extend the dir */
      ds->type = CREATE;
      ds->stat = EXTEND;
      ds->idx = FSBLK_ROUND (dp->dn_stat.st_size);
    }

  /* Return to the user; if we can't, release the reference
     (and lock) we acquired above.  */
 out:
  /* Deallocate or save the mapping.  */
  if ((err && err != ENOENT)
      || !ds
      || ds->type == LOOKUP)
    {
      munmap ((caddr_t) buf, buflen);
      if (ds)
	ds->type = LOOKUP;	/* set to be ignored by drop_dirstat */
    }
  else
    {
      ds->mapbuf = buf;
      ds->mapextent = buflen;
    }

  if (np)
    {
      assert (npp);
      if (err)
	{
	  if (!spec_dotdot)
	    {
	      /* Normal case */
	      if (np == dp)
		diskfs_nrele (np);
	      else
		diskfs_nput (np);
	    }
	  else if (type == RENAME || type == REMOVE)
	    /* We just did ifind to get np; that allocates
	       no new references, so we don't have anything to do */
	    ;
	  else if (type == LOOKUP)
	    /* We did diskfs_cached_lookup */
	    diskfs_nput (np);
	}
      else
	*npp = np;
    }

  return err ? : inum ? 0 : ENOENT;
}

/* Scan AMT bytes of block at address BLKADDR (of node DP; block index
   IDX), for name NAME of length NAMELEN.  Args TYPE, DS are as for
   diskfs_lookup.  If found, set *INUM to the inode number, else
   return ENOENT.  */
static error_t
dirscanblock (vm_address_t blockaddr, size_t amt, struct node *dp, int idx,
	      const char *name, int namelen, enum lookup_type type,
	      struct dirstat *ds, ino_t *inum)
{
  vm_address_t currentoff, prevoff;
  struct minix_dir_entry *entry = 0;
  int nentries = 0;
  int looking = 0;
  int index;

  if (ds && ds->stat == LOOKING)
    looking = 1;

  for (currentoff = blockaddr, prevoff = 0, index = 0;
       currentoff < blockaddr + amt;
       prevoff = currentoff, currentoff += sblock_info->s_dirsize, index++)
    {
      entry = (struct minix_dir_entry *) currentoff;

      if (entry->inode == 0)
	{
	  if (looking)
	    {
	      minixfs_debug ("found a free slot at index %d (block index %d)",
			     idx * sblock_info->s_dir_slots_per_fs_block
			     + index, idx);

	      ds->type = CREATE;
	      ds->stat = TAKE;
	      ds->entry = entry;
	      ds->entry_index = index;
	      ds->idx = idx;
	      looking = 0;
	    }
	}
      else
	{
	  nentries++;
	  if (direntry_len (entry->name) == namelen
	      && entry->name[0] == name[0]
	      && !bcmp (entry->name, name, namelen))
	    break;
	}
    }

  if (currentoff >= blockaddr + amt)
    {
      int i;
      /* The name is not in this block.  */

      /* Because we scanned the entire block, we should write
	 down how many entries there were.  XXX update comment
	 (maybe we scanned only a part of the block) */
      if (!dp->dn->dirents)
	{
	  dp->dn->dirents = malloc (FSBLK_ROUND (dp->dn_stat.st_size)
				    * sizeof (int));
	  for (i = 0; i < FSBLK_ROUND (dp->dn_stat.st_size); i++)
	    dp->dn->dirents[i] = -1;
	}
      /* Make sure the count is correct if there is one now.  */
      assert (dp->dn->dirents[idx] == -1
	      || dp->dn->dirents[idx] == nentries);
      dp->dn->dirents[idx] = nentries;

      return ENOENT;
    }

  /* We have found the required name.  */

  if (ds && type == CREATE)
    ds->type = LOOKUP;		/* it's invalid now */
  else if (ds && (type == REMOVE || type == RENAME))
    {
      ds->type = type;
      ds->stat = HERE_TIS;
      ds->entry = entry;
      ds->entry_index = index;
      ds->idx = idx;
      ds->preventry = (struct minix_dir_entry *) prevoff;
    }

  *inum = entry->inode;
  return 0;
}

/* Following a lookup call for CREATE, this adds a node to a directory.
   DP is the directory to be modified; NAME is the name to be entered;
   NP is the node being linked in; DS is the cached information returned
   by lookup; CRED describes the user making the call.  This call may
   only be made if the directory has been held locked continuously since
   the preceding lookup call, and only if that call returned ENOENT.  */
error_t
diskfs_direnter_hard (struct node *dp, const char *name, struct node *np,
		      struct dirstat *ds, struct protid *cred)
{
  struct minix_dir_entry *new;
  int namelen = strlen (name);
  error_t err;
  size_t oldsize = 0;

  assert (ds->type == CREATE);

  assert (!diskfs_readonly);

  dp->dn_set_mtime = 1;

  /* Select a location for the new directory entry.  Each branch of this
     switch is responsible for setting NEW to point to the on-disk
     directory entry being written.  */

  switch (ds->stat)
    {
    case TAKE:
      /* We are supposed to consume this slot.  */
      assert (ds->entry->inode == 0);
      minixfs_debug ("consuming a free slot at index %d (block index %d) "
		     "of directory %Ld",
		     (ds->idx * sblock_info->s_dir_slots_per_fs_block
		      + ds->entry_index),
		     ds->idx, dp->cache_id);

      new = ds->entry;

      /* If we are possessing ourselves of a slot in the last block of
	 the directory, perhaps we may need to increase the
	 user-visible size of the latter.  */
      if (FSBLK_TRUNC (dp->dn_stat.st_size) == ds->idx)
	{
	  int assigned_slots_in_this_block =
	    ((dp->dn_stat.st_size / sblock_info->s_dir_slots_per_fs_block)
	     % sblock_info->s_dir_slots_per_fs_block);

	  if (ds->entry_index >= assigned_slots_in_this_block)
	    {
	      /* We do need to adjust the user-visible size of the directory
		 (without actually allocating anything, though).  */

	      /* This slot cannot be the first one in its block, because the
		 first one is assigned when extending the directory by one
		 entire block (see below).  */
	      assert (assigned_slots_in_this_block != 0
		      && ds->entry_index == assigned_slots_in_this_block);

	      minixfs_debug ("adjusting the size of directory %Ld to %qd "
			     "bytes (from %qd)", dp->cache_id,
			     dp->dn_stat.st_size + sblock_info->s_dirsize,
			     dp->dn_stat.st_size);
	      dp->dn_stat.st_size += sblock_info->s_dirsize;
	    }
	}

      break;

    case EXTEND:
      /* Extend the file.  */
      minixfs_debug ("extending directory %Ld", dp->cache_id);

      oldsize = dp->dn_stat.st_size;
      if ((off_t)(oldsize + block_size) != (dp->dn_stat.st_size + block_size))
	{
	  /* We can't possibly map the whole directory in.  */
	  munmap ((caddr_t) ds->mapbuf, ds->mapextent);
	  return EOVERFLOW;
	}
      while (oldsize + block_size > dp->allocsize)
	{
	  err = diskfs_grow (dp, oldsize + block_size, cred);
	  if (err)
	    {
	      munmap ((caddr_t) ds->mapbuf, ds->mapextent);
	      return err;
	    }
	}

      new = (struct minix_dir_entry *) (ds->mapbuf + oldsize);

      minixfs_debug ("adjusting the size of directory %Ld to %zd bytes "
		     "(from %zd)", dp->cache_id,
		     oldsize + sblock_info->s_dirsize, oldsize);
      dp->dn_stat.st_size = oldsize + sblock_info->s_dirsize;

      break;

    default:
      new = 0;
      assert (! "impossible: bogus status field in dirstat");
    }

  /* NEW points to the directory entry being written.  */
  new->inode = np->cache_id;
  memcpy (new->name, name, namelen);

  /* All the entries have a fixed length, so do not forget to write zeroes
     past the end of the name.  */
  memset (new->name + namelen, 0, sblock_info->s_namelen - namelen);

  /* Mark the directory inode has having been written.  */
  dp->dn_set_mtime = 1;

  munmap ((caddr_t) ds->mapbuf, ds->mapextent);

  if (ds->stat != EXTEND)
    {
      /* If we are keeping count of this block, then keep the count up
	 to date.  */
      if (dp->dn->dirents && dp->dn->dirents[ds->idx] != -1)
	dp->dn->dirents[ds->idx]++;
    }
  else
    {
      int i;
      /* It's cheap, so start a count here even if we aren't counting
	 anything at all.  */
      if (dp->dn->dirents)
	{
	  dp->dn->dirents = realloc (dp->dn->dirents,
				     FSBLK_ROUND (dp->dn_stat.st_size)
				     * sizeof (int));
	  for (i = FSBLK_ROUND (oldsize);
	       i < FSBLK_ROUND (dp->dn_stat.st_size);
	       i++)
	    dp->dn->dirents[i] = -1;

	  dp->dn->dirents[ds->idx] = 1;
	}
      else
	{
	  dp->dn->dirents = malloc (FSBLK_ROUND (dp->dn_stat.st_size)
				    * sizeof (int));
	  for (i = 0; i < FSBLK_ROUND (dp->dn_stat.st_size); i++)
	    dp->dn->dirents[i] = -1;
	  dp->dn->dirents[ds->idx] = 1;
	}
    }

  diskfs_file_update (dp, 1);

  return 0;
}

/* Following a lookup call for REMOVE, this removes the link from the
   directory.  DP is the directory being changed and DS is the cached
   information returned from lookup.  This call is only valid if the
   directory has been locked continously since the call to lookup, and
   only if that call succeeded.  */
error_t
diskfs_dirremove_hard (struct node *dp, struct dirstat *ds)
{
  assert (ds->type == REMOVE);
  assert (ds->stat == HERE_TIS);

  assert (!diskfs_readonly);

  ds->entry->inode = 0;

  dp->dn_set_mtime = 1;

  munmap ((caddr_t) ds->mapbuf, ds->mapextent);

  /* If we are keeping count of this block, then keep the count up
     to date.  */
  if (dp->dn->dirents && dp->dn->dirents[ds->idx] != -1)
    dp->dn->dirents[ds->idx]--;
  diskfs_file_update (dp, 1);

  return 0;
}

/* Following a lookup call for RENAME, this changes the inode number
   on a directory entry.  DP is the directory being changed; NP is
   the new node being linked in; DP is the cached information returned
   by lookup.  This call is only valid if the directory has been locked
   continuously since the call to lookup, and only if that call
   succeeded.  */
error_t
diskfs_dirrewrite_hard (struct node *dp, struct node *np, struct dirstat *ds)
{
  assert (ds->type == RENAME);
  assert (ds->stat == HERE_TIS);

  assert (!diskfs_readonly);

  ds->entry->inode = np->cache_id;
  dp->dn_set_mtime = 1;

  munmap ((caddr_t) ds->mapbuf, ds->mapextent);

  diskfs_file_update (dp, 1);

  return 0;
}

/* Tell if DP is an empty directory (has only "." and ".." entries).
   This routine must be called from inside a catch_exception ().  */
int
diskfs_dirempty (struct node *dp, struct protid *cred)
{
  error_t err;
  vm_address_t buf = 0, curoff;
  struct minix_dir_entry *entry;
  int hit = 0;			/* Found something in the directory.  */
  memory_object_t memobj = diskfs_get_filemap (dp, VM_PROT_READ);

  if (memobj == MACH_PORT_NULL)
    /* XXX should reflect error properly. */
    {
      minixfs_debug ("memobj == MACH_PORT_NULL");
      return 0;
    }

  err = vm_map (mach_task_self (), &buf, dp->dn_stat.st_size,
		0, 1, memobj, 0, 0, VM_PROT_READ, VM_PROT_READ, 0);

  mach_port_deallocate (mach_task_self (), memobj);
  assert (!err);

  if (! diskfs_check_readonly ())
    dp->dn_set_atime = 1;

  for (curoff = buf;
       !hit && curoff < buf + dp->dn_stat.st_size;
       curoff += sblock_info->s_dirsize)
    {
      entry = (struct minix_dir_entry *) curoff;

      if (entry->inode != 0
	  && (direntry_len (entry->name) > 2
	      || entry->name[0] != '.'
	      || (entry->name[1] != '.'
		  && entry->name[1] != '\0')))
	hit = 1;
    }

  if (! diskfs_check_readonly ())
    dp->dn_set_atime = 1;
  if (diskfs_synchronous)
    diskfs_node_update (dp, 1);

  munmap ((caddr_t) buf, dp->dn_stat.st_size);

  return !hit;
}

/* Make DS an invalid dirstat. */
error_t
diskfs_drop_dirstat (struct node *dp, struct dirstat *ds)
{
  if (ds->type != LOOKUP)
    {
      assert (ds->mapbuf);
      munmap ((caddr_t) ds->mapbuf, ds->mapextent);
      ds->type = LOOKUP;
    }
  return 0;
}

/* Count the entries in directory block NB for directory DP and write
   the answer down in its dirents array.  As a side affect fill BUF
   with the whole block, or with a portion thereof.  *AMTREAD is
   filled with the amount actually read.  */
static error_t
count_dirents (struct node *dp, int nb, char *buf, size_t *amtread)
{
  size_t amt;
  char *offinblk;
  struct minix_dir_entry *entry;
  int count = 0;
  error_t err;

  minixfs_debug ("counting entries in directory block %d", nb);

  assert (dp->dn->dirents);
  assert (nb < FSBLK_ROUND (dp->dn_stat.st_size));

  err = diskfs_node_rdwr (dp, buf, nb << log2_block_size, block_size,
			  0, 0, &amt);
  if (err)
    return err;
  assert (amt <= block_size);

  minixfs_debug ("portion of block actually read: %zd bytes", amt);

  for (offinblk = buf;
       offinblk < buf + amt;
       offinblk += sblock_info->s_dirsize)
    {
      entry = (struct minix_dir_entry *) offinblk;
      if (entry->inode)
	count++;
    }

  minixfs_debug ("%d entr%s found", count, count == 1 ? "y" : "ies");

  bzero (buf + amt, block_size - amt);

  if (amtread)
    *amtread = amt;
  else
    amtread = &amt;

  assert (dp->dn->dirents[nb] == -1 || dp->dn->dirents[nb] == count);
  dp->dn->dirents[nb] = count;
  return 0;
}

/* Returned directory entries are aligned to blocks this many bytes long.
   Must be a power of two.  */
#define DIRENT_ALIGN 4

/* Return NENTRIES directory entries starting at ENTRY from locked
   directory node DP.  Fill *DATA with the entries; that pointer
   currently points to *DATACNT bytes.  If it isn't big enough,
   vm_allocate into *DATA.  Set *DATACNT with the total size used.
   Fill AMT with the number of entries copied.  Regardless, never copy
   more than BUFSIZ bytes.  If BUFSIZ is 0, then there is no limit on
   *DATACNT; if NENTRIES is -1, then there is no limit on AMT.  */
error_t
diskfs_get_directs (struct node *dp,
		    int entry,
		    int nentries,
		    char **data,
		    size_t *datacnt,
		    vm_size_t bufsiz,
		    int *amt)
{
  int blkno;
  int nblks;
  int curentry;
  char buf[block_size];
  char *bufp;
  int bufvalid;
  error_t err;
  int i;
  char *datap;
  struct minix_dir_entry *entryp;
  int allocsize;
  size_t checklen;
  struct dirent *userp;

  nblks = FSBLK_ROUND (dp->dn_stat.st_size);

  if (!dp->dn->dirents)
    {
      dp->dn->dirents = malloc (nblks * sizeof (int));
      for (i = 0; i < nblks; i++)
	dp->dn->dirents[i] = -1;
    }

  /* Scan through the entries to find ENTRY.  If we encounter
     a -1 in the process then stop to fill it.  When we run
     off the end, ENTRY is too big.  */
  curentry = 0;
  bufvalid = 0;
  for (blkno = 0; blkno < nblks; blkno++)
    {
      if (dp->dn->dirents[blkno] == -1)
	{
	  err = count_dirents (dp, blkno, buf, &checklen);
	  if (err)
	    return err;
	  bufvalid = 1;
	}

      if (curentry + dp->dn->dirents[blkno] > entry)
	/* ENTRY starts in this block.  */
	break;

      curentry += dp->dn->dirents[blkno];

      bufvalid = 0;
    }

  if (blkno == nblks)
    {
      /* We reached the end of the directory without seeing ENTRY.
	 This is treated as an EOF condition, meaning we return
	 success with empty results.  */
      *datacnt = 0;
      *amt = 0;
      return 0;
    }

  /* Allocate enough space to hold the maximum we might return */
  if (!bufsiz || bufsiz > dp->dn_stat.st_size)
    /* Allocate enough to return the entire directory.  Since the
       directory format of the MINIX file system is different than the
       format used to return the entries, we allocate enough to hold
       the on disk directory plus whatever extra would be necessary in
       the worst-case.  */
    {
      /* The minimum size of a returned dirent entry.  The +1 is for '\0'.  */
      size_t min_dirent_size = offsetof (struct dirent, d_name) + 1;
      /* The maximum possible number of MINIXfs dir entries in this dir.  */
      size_t max_entries = dp->dn_stat.st_size / sblock_info->s_dirsize;
      /* The maximum difference in size per directory entry.  */
      size_t entry_extra =
	DIRENT_ALIGN
	  + (min_dirent_size > sblock_info->s_dirsize
	     ? min_dirent_size - sblock_info->s_dirsize : 0);

      allocsize = round_page (dp->dn_stat.st_size + max_entries * entry_extra);
    }
  else
    allocsize = round_page (bufsiz);

  if (allocsize > *datacnt)
    *data = mmap (0, allocsize, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);

  /* Set bufp appropriately */
  bufp = buf;
  if (curentry != entry)
    {
      /* Look through the block to find out where to start,
	 setting bufp appropriately.  */
      if (!bufvalid)
	{
	  err = diskfs_node_rdwr (dp, buf, blkno << log2_block_size, 
 				  block_size, 0, 0, &checklen);
	  if (err)
	    return err;
	  assert (checklen <= block_size);
	  bufvalid = 1;
	}

      for (i = 0, bufp = buf;
	   i < entry - curentry && bufp - buf < checklen;
	   bufp += sblock_info->s_dirsize, i++)
	;
      /* Make sure we didn't run off the end.  */
      assert (bufp - buf <= checklen);
    }

  i = 0;
  datap = *data;

  /* Copy the entries, one at a time.  */
  while (((nentries == -1) || (i < nentries))
	 && (!bufsiz || (datap - *data < bufsiz))
	 && blkno < nblks)
    {
      if (!bufvalid)
	{
	  err = diskfs_node_rdwr (dp, buf, blkno << log2_block_size,
				  block_size, 0, 0, &checklen);
	  if (err)
	    return err;
	  assert (checklen <= block_size);
	  bufvalid = 1;
	  bufp = buf;
	}

      entryp = (struct minix_dir_entry *) bufp;

      if (entryp->inode)
	{
	  int rec_len;
	  int name_len = direntry_len (entryp->name);

	  assert (name_len > 0);

	  userp = (struct dirent *) datap;

	  /* Length is structure before the name + the name + '\0', all
	     padded to a four-byte alignment.  */
	  rec_len =
	    ((offsetof (struct dirent, d_name)
	      + name_len + 1
	      + (DIRENT_ALIGN - 1))
	     & ~(DIRENT_ALIGN - 1));

	  /* See if this record would run over the end of the return
	     buffer.  */
	  if (bufsiz == 0)
	    /* It shouldn't ever, as we calculated the worst case size.  */
	    assert (datap + rec_len <= *data + allocsize);
	  else
	    /* It's ok if it does, just leave off returning this entry.  */
	    if (datap + rec_len > *data + allocsize)
	      break;

	  userp->d_fileno = entryp->inode;
	  userp->d_reclen = rec_len;
	  userp->d_namlen = name_len;

	  /* XXX
	     For complex reasons it might not be correct to return
	     the filesystem's d_type value to the user.  */
	  userp->d_type = DT_UNKNOWN;

	  memcpy (userp->d_name, entryp->name, name_len);
	  userp->d_name[name_len] = '\0';

	  datap += rec_len;
	  i++;
	}

#if 0
      if (entryp->rec_len == 0)
	{
	  ext2_warning ("zero length directory entry: inode: %Ld offset: %zd",
			dp->cache_id,
			blkno * DIRBLKSIZ + bufp - buf);
	  return EIO;
	}
#endif

      bufp += sblock_info->s_dirsize;
      if (bufp - buf == checklen)
	{
	  blkno++;
	  bufvalid = 0;
	}
#if 0
      else if (bufp - buf > DIRBLKSIZ)
	{
	  ext2_warning ("directory entry too long: inode: %Ld offset: %zd",
			dp->cache_id,
			blkno * DIRBLKSIZ + bufp - buf - entryp->rec_len);
	  return EIO;
	}
#endif
    }

  /* We've copied all we can.  If we allocated our own array
     but didn't fill all of it, then free whatever memory we didn't use.  */
  if (allocsize > *datacnt)
    {
      if (round_page (datap - *data) < allocsize)
	munmap ((caddr_t) (*data + round_page (datap - *data)),
		allocsize - round_page (datap - *data));
    }

  /* Set variables for return */
  *datacnt = datap - *data;
  *amt = i;
  return 0;
}

