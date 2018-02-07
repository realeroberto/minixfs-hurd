/* Message printing functions

   Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

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

/* Define this if you want debug info to be written to a log file.  */
#undef USE_LOG_FILE

#include <stdio.h>
#include <stdarg.h>
#ifdef USE_LOG_FILE
  #define LOG_FILE_PATH "minixfs.debug"
  #include <fcntl.h>
  #include <errno.h>
  #include <sys/stat.h>
#endif

#include "minixfs.h"

struct mutex printf_lock = MUTEX_INITIALIZER; /* XXX */
static char error_buf[2048];

#ifdef USE_LOG_FILE
FILE *log_file; 
struct mutex open_log_file_lock = MUTEX_INITIALIZER;
int use_log_file = 1;
int log_file_opened = 0;

static void 
open_log_file (void)
{
  int fd;

  if (log_file_opened)
    return;

  mutex_lock (&open_log_file_lock);

  fd = open (LOG_FILE_PATH, O_WRONLY | O_CREAT | O_APPEND | O_FSYNC,
	     S_IRUSR | S_IWUSR);
  if (fd < 0)
    goto error;

  log_file = fdopen (fd, "a");
  if (log_file == NULL)
    goto error;

  log_file_opened = 1;

  mutex_unlock (&open_log_file_lock);
  return;

 error:
  minixfs_error ("cannot open log file %s: %s", LOG_FILE_PATH,
		 strerror (errno));
  if (fd <= 0)
    close (fd);
  mutex_unlock (&open_log_file_lock);
  use_log_file = !use_log_file;
  return;
}

static void inline
update_log_file (char *error_buf)
{
  if (! use_log_file)
    return;

  if (! log_file_opened)
    open_log_file ();

  flockfile (log_file);

  while (*error_buf != '\0')
    putc_unlocked (*error_buf++, log_file);
  putc_unlocked ('\n', log_file);

  fflush_unlocked (log_file);
  funlockfile (log_file);
}
#endif

int printf (const char *fmt, ...)
{
  va_list arg;
  int done;

  mutex_lock (&printf_lock);

  va_start (arg, fmt);
  done = vsprintf (error_buf, fmt, arg);
  va_end (arg);

  fprintf (stderr, "%s\n", error_buf);

#ifdef USE_LOG_FILE
  update_log_file (error_buf);
#endif

  mutex_unlock (&printf_lock);
  return done;
}
  
void _minixfs_error (const char * function, const char * fmt, ...)
{
  va_list args;

  mutex_lock (&printf_lock);

  sprintf (error_buf, "%s: %s: ", diskfs_disk_name, function);

  va_start (args, fmt);
  vsprintf (strchr (error_buf, '\0'), fmt, args);
  va_end (args);

  fprintf (stderr, "minixfs: %s\n", error_buf);

#ifdef USE_LOG_FILE
  update_log_file (error_buf);
#endif

  mutex_unlock (&printf_lock);
}

void _minixfs_panic (const char * function, const char * fmt, ...)
{
  va_list args;

  mutex_lock(&printf_lock);

  sprintf (error_buf, "%s: panic: %s: ", diskfs_disk_name, function);

  va_start (args, fmt);
  vsprintf (strchr (error_buf, '\0'), fmt, args);
  va_end (args);

  fprintf (stderr, "minixfs: %s\n", error_buf);

#ifdef USE_LOG_FILE
  update_log_file (error_buf);
#endif

  mutex_unlock (&printf_lock);

  exit (1);
}

void minixfs_warning (const char * fmt, ...)
{
  va_list args;

  mutex_lock (&printf_lock);

  sprintf (error_buf, "%s: warning: ", diskfs_disk_name);

  va_start (args, fmt);
  vsprintf (strchr (error_buf, '\0'), fmt, args);
  va_end (args);

  fprintf (stderr, "minixfs: %s\n", error_buf);

#ifdef USE_LOG_FILE
  update_log_file (error_buf);
#endif

  mutex_unlock (&printf_lock);
}
