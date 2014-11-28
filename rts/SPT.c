/*
 * (c)2014 Tweag I/O
 */

#include "Rts.h"
#include "Hash.h"

static HashTable * spt = NULL;

void hs_spt_module_init(void *spe[]) {
  if (spt == NULL)
    spt = allocFpHashTable();

  size_t i;
  for (i=0; spe[i]; i+=2) {
    getStablePtr(spe[i+1]);
    insertHashTable(spt, (StgWord)spe[i], spe[i+1]);
  }
}

StgPtr hs_spt_lookup(char* key) {
    return lookupHashTable(spt, (StgWord)key);
}
