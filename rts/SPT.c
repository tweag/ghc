/*
 * (c)2014 Tweag I/O
 */

#include "PosixSource.h"
#include "Rts.h"
#include "Hash.h"

/*
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
*/

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


static HashTable * spt = NULL;

void
hs_spt_module(void *spe[])
{
  if (spt == NULL) {
    spt = allocStrHashTable();
  }

  void *next=NULL;
  for (next=*spe; next; next+=2) {
    getStablePtr(next+1);
    insertHashTable(next, (StgWord)next, next+1);
  }
}
