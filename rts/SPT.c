/*
 * (c)2014 Tweag I/O
 */

#include "Rts.h"
#include "Hash.h"

static HashTable * spt = NULL;

void hs_spt_insert(StgWord64 key[2],void *spe_closure) {
  if (spt == NULL)
    spt = allocFpHashTable();

  getStablePtr(spe_closure);
  insertHashTable(spt, (StgWord)key, spe_closure);
}

StgPtr hs_spt_lookup(StgWord64 key[2]) {
    return lookupHashTable(spt, (StgWord)key);
}
