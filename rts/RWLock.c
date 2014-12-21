/*
 * (c)2014 Tweag I/O
 *
 * A readers-writer lock implementation.
 *
 * See invariants in RWLock.h.
 *
 * */

#include "RWLock.h"
#include "Rts.h"
#include "RtsUtils.h"

#ifdef THREADED_RTS

struct rwlock {
  Mutex mutex;     // This is held by writers while accessing the shared
                   // resource and it is held by readers before and after
                   // accessing the resource.
  Condition noReaders;       // Writers waiting until there are no readers
  Condition noQueuedWriters; // Readers waiting until there are no queued
                             // writers
  int readers;       // Amount of readers accessing the shared resource
  int queuedWriters; // Amount of queued writers
};

RWLock* allocRWLock() {
  RWLock* lock = stgMallocBytes(sizeof(RWLock), "allocRWLock");
  initMutex(&lock->mutex);
  initCondition(&lock->noReaders);
  initCondition(&lock->noQueuedWriters);
  lock->readers = 0;
  lock->queuedWriters = 0;
  return lock;
}

void freeRWLock(RWLock* lock) {
  closeMutex(&lock->mutex);
  closeCondition(&lock->noReaders);
  closeCondition(&lock->noQueuedWriters);
  stgFree(lock);
}

void rlockRWLock(RWLock* lock) {
  ACQUIRE_LOCK(&lock->mutex);
  while (lock->queuedWriters)
    waitCondition(&lock->noQueuedWriters, &lock->mutex);

  lock->readers += 1;
  RELEASE_LOCK(&lock->mutex);
}

void wlockRWLock(RWLock* lock) {
  ACQUIRE_LOCK(&lock->mutex);
  while (lock->readers) {
    lock->queuedWriters += 1;
    waitCondition(&lock->noReaders, &lock->mutex);
    lock->queuedWriters -= 1;
  }
}

void runlockRWLock(RWLock* lock) {
  ACQUIRE_LOCK(&lock->mutex);
  lock->readers -= 1;
  if (!lock->readers)
    signalCondition(&lock->noReaders);
  RELEASE_LOCK(&lock->mutex);
}

void wunlockRWLock(RWLock* lock) {
  if (!lock->queuedWriters)
    broadcastCondition(&lock->noQueuedWriters);
  else
    signalCondition(&lock->noReaders);
  RELEASE_LOCK(&lock->mutex);
}

#endif // #ifdef THREADED_RTS

void exitRWLock ( void ) {}

