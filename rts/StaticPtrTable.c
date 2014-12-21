/*
 * (c)2014 Tweag I/O
 *
 * The Static Pointer Table implementation.
 *
 * https://ghc.haskell.org/trac/ghc/wiki/StaticPointers
 * https://ghc.haskell.org/trac/ghc/wiki/StaticPointers/ImplementationPlan
 *
 */

#include "Rts.h"
#include "StaticPtrTable.h"
#include "Hash.h"
#include "RWLock.h"

static HashTable * spt = NULL;

#ifdef THREADED_RTS
static RWLock* spt_lock;
#endif

/// Hash function for the SPT.
static int hashFingerprint(HashTable *table, StgWord64 key[2]) {
  // Take half of the key to compute the hash.
  return hashWord(table, (StgWord)key[1]);
}

/// Comparison function for the SPT.
static int compareFingerprint(StgWord64 ptra[2], StgWord64 ptrb[2]) {
  return ptra[0] == ptrb[0] && ptra[1] == ptrb[1];
}

void hs_spt_insert(StgWord64 key[2],void *spe_closure) {
  // hs_spt_insert is called from constructor functions, so
  // the SPT needs to be initialized here.
  if (spt == NULL) {
    spt = allocHashTable_( (HashFunction *)hashFingerprint
                         , (CompareFunction *)compareFingerprint
                         );
#ifdef THREADED_RTS
    spt_lock = allocRWLock();
#endif
  }

  getStablePtr(spe_closure);
  wlockRWLock(spt_lock);
  insertHashTable(spt, (StgWord)key, spe_closure);
  wunlockRWLock(spt_lock);
}

StgPtr hs_spt_lookup(StgWord64 key[2]) {
  if (spt) {
    rlockRWLock(spt_lock);
    const StgPtr ret = lookupHashTable(spt, (StgWord)key);
    runlockRWLock(spt_lock);
    return ret;
  } else
    return NULL;
}

int hs_spt_keys(StgPtr keys[], int szKeys) {
  if (spt) {
    rlockRWLock(spt_lock);
    const int ret = keysHashTable(spt, (StgWord*)keys, szKeys);
    runlockRWLock(spt_lock);
    return ret;
  } else
    return 0;
}

int hs_spt_key_count() {
  return spt ? keyCountHashTable(spt) : 0;
}

void exitStaticPtrTable() {
  if (spt) {
    freeHashTable(spt, NULL);
    spt = NULL;
    freeRWLock(spt_lock);
  }
}
