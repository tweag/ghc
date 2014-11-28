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
hs_spt_module(void *spe[])
{
  if (spt == NULL) {
    spt = allocFpHashTable();
  }

  size_t i;
  for (i=0; spe[i]; i+=2) {
	printf("save [%" PRIu64 ",%" PRIu64 ",%" PRIu64 "] : %p\n", ((uint64_t*)spe[i])[0], ((uint64_t*)spe[i])[1], ((uint64_t*)spe[i])[2] , spe[i+1]);
    getStablePtr(spe[i+1]);
    insertHashTable(spt, (StgWord)spe[i], spe[i+1]);
  }
}

StgPtr hs_spt_lookup(char* key) {
    	uint64_t *spe =(uint64_t*)key;
	printf("load [%" PRIu64 ",%" PRIu64 ",%" PRIu64 "]", spe[0], spe[1], spe[2]);
    StgPtr res = lookupHashTable(spt, (StgWord)key);
    
	return res;
}
