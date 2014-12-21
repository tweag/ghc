/*
 * (c)2014 Tweag I/O
 *
 * A readers-writer lock implementation.
 *
 * Invariants:
 *  * No reader accesses the shared resource if a writer is using it.
 *  * No pair of writers accesses the shared resource concurrently.
 *  * Readers don't prevent each other from using the shared resource.
 *  * Writers don't access the shared resource while readers are accessing it.
 *  * Readers do not access the resource if writers are queued to read it.
 *
 * */

#ifndef RWLOCK_H
#define RWLOCK_H

#include "BeginPrivate.h"

#ifdef THREADED_RTS

typedef struct rwlock RWLock;

RWLock* allocRWLock ( void);
void freeRWLock(RWLock* lock);

void rlockRWLock(RWLock* lock);
void runlockRWLock(RWLock* lock);

void wlockRWLock(RWLock* lock);
void wunlockRWLock(RWLock* lock);

#else

#define freeRWLock(lock)

#define rlockRWLock(lock)
#define runlockRWLock(lock)

#define wlockRWLock(lock)
#define wunlockRWLock(lock)

#endif // #ifdef THREADED_RTS

void exitRWLock ( void );

#include "EndPrivate.h"

#endif /* RWLOCK_H */
