/* Main entry point for the MINIX file system translator

   Copyright (C) 1994,95,96,97,98,99,2002 Free Software Foundation, Inc.

   Converted for ext2fs by Miles Bader <miles@gnu.ai.mit.edu>

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

#include <stdarg.h>
#include <stdio.h>
#include <device/device.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <error.h>
#include <argz.h>
#include <argp.h>
/*#include <version.h>*/
#include <hurd/version.h>
#include "minixfs.h"

/* ---------------------------------------------------------------- */
int diskfs_link_max = 0;
int diskfs_name_max = 0;
int diskfs_maxsymlinks = 8;
int diskfs_shortcut_symlink = 1;
int diskfs_shortcut_chrdev = 1;
int diskfs_shortcut_blkdev = 1;
int diskfs_shortcut_fifo = 1;
int diskfs_shortcut_ifsock = 1;

char *diskfs_server_name = MINIXFS_NAME;
char *diskfs_server_version = MINIXFS_VERSION;
char *diskfs_extra_version = ""/*"MINIXfs " MINIXFS_VERSION " on GNU Hurd " HURD_VERSION*/;

int diskfs_synchronous;

struct node *diskfs_root_node;

struct store *store;
struct store_parsed *store_parsed;

char *diskfs_disk_name;

#ifdef MINIXFS_DEBUG
int minixfs_debug_flag;
#endif

/* MINIXfs-specific options.  */
static const struct argp_option
options[] =
{
  {"debug", 'D', 0, 0, "Toggle debugging output"
#ifndef MINIXFS_DEBUG
   " (not compiled in)"
#endif
  },
  {"block-size", 'B', "BLOCKSIZE", 0,
   ""}, /* XXX: add explanation */
  {0}
};

/* Parse a command line option.  */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  /* We save our parsed values in this structure, hung off STATE->hook.
     Only after parsing all options successfully will we use these values.  */
  struct
  {
    int debug_flag;
    int block_size_specified;
    size_t block_size;
  } *values = state->hook;

  switch (key)
    {
    case 'B':
      values->block_size = strtoul (arg, &arg, 0);
      if (!arg || *arg != '\0')
	{
	  argp_error (state, "invalid value for --block-size");
	  return EINVAL;
	}
      values->block_size_specified = 1;
      break;
    case 'D':
      values->debug_flag = 1;
      break;

    case ARGP_KEY_INIT:
      state->child_inputs[0] = state->input;
      values = malloc (sizeof *values);
      if (values == 0)
	return ENOMEM;
      state->hook = values;
      bzero (values, sizeof *values);
      values->block_size_specified = 0;
      values->block_size = 0;
      break;

    case ARGP_KEY_SUCCESS:
      /* All options parse successfully, so implement ours if possible.  */
      if (values->debug_flag)
	{
#ifdef MINIXFS_DEBUG
	  minixfs_debug_flag = !minixfs_debug_flag;
#else
	  argp_failure (state, 2, 0, "debugging support not compiled in");
	  return EINVAL;
#endif
	}
      if (values->block_size_specified)
	{
	  switch (values->block_size)
	    {
	    case 512:
	    case 1024:
	    case 2048:
	    case 4096:
	    case 8192:
	      block_size = values->block_size;
	      break;
	    default:
	      argp_failure (state, 2, 0, "invalid value for --block-size");
	      return EINVAL;
	    }
	}
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}

/* Override the standard diskfs routine so we can add our own output.  */
error_t
diskfs_append_args (char **argz, size_t *argz_len)
{
  error_t err;

  /* Get the standard things.  */
  err = diskfs_append_std_options (argz, argz_len);

#ifdef MINIXFS_DEBUG
  if (!err && minixfs_debug_flag)
    err = argz_add (argz, argz_len, "--debug");
#endif
  if (! err)
    err = store_parsed_append_args (store_parsed, argz, argz_len);

  return err;
}

/* Add our startup arguments to the standard diskfs set.  */
static const struct argp_child startup_children[] =
  {{&diskfs_store_startup_argp}, {0}};
static struct argp startup_argp = {options, parse_opt, 0, 0, startup_children};

/* Similarly at runtime.  */
static const struct argp_child runtime_children[] =
  {{&diskfs_std_runtime_argp}, {0}};
static struct argp runtime_argp = {options, parse_opt, 0, 0, runtime_children};

struct argp *diskfs_runtime_argp = (struct argp *)&runtime_argp;

int
main (int argc, char **argv)
{
  error_t err;
  mach_port_t bootstrap;

  /* Initialize the diskfs library, parse arguments, and open the store.
     This starts the first diskfs thread for us.  */
  store = diskfs_init_main (&startup_argp, argc, argv,
			    &store_parsed, &bootstrap);

  if (store->size < SBLOCK_OFFS + SBLOCK_SIZE)
    minixfs_panic ("device too small for superblock (%Ld bytes)", store->size);
  if (store->log2_blocks_per_page < 0)
    minixfs_panic ("device block size (%zu) greater than page size (%zd)",
		 store->block_size, vm_page_size); /* XXX */

  /* Map the entire disk.  */
  create_disk_pager ();

  pokel_init (&global_pokel, diskfs_disk_pager, disk_image);

  get_hypermetadata();

  /* Libdiskfs needs to know the following values.  */
  diskfs_link_max = sblock_info->s_link_max;
  diskfs_name_max = sblock_info->s_namelen; 

  inode_init ();

  /* Set diskfs_root_node to the root inode. */
  err = diskfs_cached_lookup (MINIXFS_ROOT_INO, &diskfs_root_node);

  if (err)
    minixfs_panic ("can't get root: %s", strerror (err));
  else if ((diskfs_root_node->dn_stat.st_mode & S_IFMT) == 0)
    minixfs_panic ("no root node!");
  mutex_unlock (&diskfs_root_node->lock);

  /* Now that we are all set up to handle requests, and diskfs_root_node is
     set properly, it is safe to export our fsys control port to the
     outside world.  */
  diskfs_startup_diskfs (bootstrap, 0);

  /* and so we die, leaving others to do the real work.  */
  cthread_exit (0);
  /* NOTREACHED */
  return 0;
}

error_t
diskfs_reload_global_state ()
{
  pokel_flush (&global_pokel);
  pager_flush (diskfs_disk_pager, 1);
  get_hypermetadata ();
  return 0;
}
