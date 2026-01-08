#ifndef POOL_H
#define POOL_H

#include "da.h"
#include "mem.h"
#include <stdint.h>

#define pooltype(ty) __##ty##_pool_t
#define init_pool(ty) __##ty##_init_pool
#define singles(ty) __##ty##_free_singles
#define pool_capacity(ty) __##ty##_pool_capacity
#define da_type(ty) __##ty##_da_t
#define setup_pool(ty)                                                         \
  typedef struct da_type(ty) {                                                 \
    void **items;                                                              \
    size_t count, capacity;                                                    \
  } da_type(ty);                                                               \
  typedef struct pooltype(ty) pooltype(ty);                                    \
  static da_type(ty) __##ty##_free_singles = {0};                              \
  const static size_t pool_capacity(ty) = 1024;                                \
  struct pooltype(ty) {                                                        \
    size_t capacity, filled;                                                   \
    ty *data;                                                                  \
    pooltype(ty) * next;                                                       \
  };                                                                           \
  void init_pool(ty)(pooltype(ty) * pool) {                                    \
    pool->capacity = pool_capacity(ty);                                        \
    pool->filled = 0;                                                          \
    pool->data = malloc(sizeof(ty) * pool->capacity);                          \
    register_object(pool->data);                                               \
  }                                                                            \
  ty *ty##_pool_allocate(pooltype(ty) * pool) {                                \
    if (pool == NULL) {                                                        \
      return NULL;                                                             \
    }                                                                          \
    if (singles(ty).count > 0) {                                               \
      return singles(ty).items[--singles(ty).count];                           \
    }                                                                          \
    if (pool->capacity <= sizeof(int) || pool->data == NULL) {                 \
      init_pool(ty)(pool);                                                     \
    }                                                                          \
    if (pool->filled + 1 >= pool->capacity) {                                  \
      if (pool->next == NULL) {                                                \
        pooltype(ty) *next_pool = malloc(sizeof(*next_pool));                  \
        register_object(next_pool);                                            \
        init_pool(ty)(next_pool);                                              \
        pool->next = next_pool;                                                \
      }                                                                        \
      return ty##_pool_allocate(pool->next);                                   \
    }                                                                          \
    return &pool->data[pool->filled++];                                        \
  }                                                                            \
  void ty##_free(pooltype(ty) * pool, void *ptr) {                             \
    if (pool == NULL || ptr == NULL || pool->data == NULL ||                   \
        pool->capacity <= sizeof(ty)) {                                        \
      return;                                                                  \
    }                                                                          \
    if ((intptr_t)ptr >= (intptr_t)pool->data ||                               \
        (intptr_t)ptr < (intptr_t)&pool->data[pool->capacity]) {               \
      da_append(&singles(ty), ptr);                                            \
    } else {                                                                   \
      ty##_free(pool->next, ptr);                                              \
    }                                                                          \
  }

#endif // POOL_H
