/*-----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1999
 *
 * Prototypes for Hash.c
 *
 * -------------------------------------------------------------------------- */

#ifndef HASH_H
#define HASH_H

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "BeginPrivate.h"

typedef struct hashtable HashTable; /* abstract */

/* Hash table access where the keys are StgWords */
HashTable * allocHashTable    ( void );
void *      lookupHashTable ( HashTable *table, StgWord key );
void        insertHashTable ( HashTable *table, StgWord key, void *data );
void *      removeHashTable ( HashTable *table, StgWord key, void *data );

int keyCountHashTable (HashTable *table);

/* Hash table access where the keys are C strings (the strings are
 * assumed to be allocated by the caller, and mustn't be deallocated
 * until the corresponding hash table entry has been removed).
 */
HashTable * allocStrHashTable ( void );

/* Hash table access where the keys are fingerprints {uint64_t[2]}
 */
HashTable * allocFpHashTable ( void );

#define lookupStrHashTable(table, key)  \
   (lookupHashTable(table, (StgWord)key))

#define insertStrHashTable(table, key, data)  \
   (insertHashTable(table, (StgWord)key, data))

#define removeStrHashTable(table, key, data) \
   (removeHashTable(table, (StgWord)key, data))

/* Hash tables for arbitrary keys */
typedef int HashFunction(HashTable *table, StgWord key);
typedef int CompareFunction(StgWord key1, StgWord key2);
HashTable * allocHashTable_(HashFunction *hash, CompareFunction *compare);
int hashWord(HashTable *table, StgWord key);
int hashStr(HashTable *table, char *key);
int hashFingerprint(HashTable *table, uint64_t* key);


/* Freeing hash tables
 */
void freeHashTable ( HashTable *table, void (*freeDataFun)(void *) );

void exitHashTable ( void );

#include "EndPrivate.h"

#endif /* HASH_H */
