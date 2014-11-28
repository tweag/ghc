/*
 * (c)2014 Tweag I/O
 */

#include "PosixSource.h"
#include "Rts.h"
#include "Hash.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

// DEBUG:
#include <inttypes.h>

static HashTable * spt = NULL;

void
hs_spt_module_init(void *spe[])
{
  if (spt == NULL) {
    spt = allocFpHashTable();
  }

  size_t i;
  for (i=0; spe[i]; i+=2) {
    getStablePtr(spe[i+1]);
    insertHashTable(spt, (StgWord)spe[i], spe[i+1]);
  }
}

StgPtr hs_spt_lookup(char* key) {
    return lookupHashTable(spt, (StgWord)key);
}
