#ifndef POOL_H
#define POOL_H

#include "da.h"
#include <stdint.h>
#include <stdlib.h>

// Pool-local reference count stored alongside data
typedef struct {
  size_t ref_count;
} pool_ref_header;

#define pooltype(ty) __##ty##_pool_t
#define init_pool(ty) __##ty##_init_pool
#define singles(ty) __##ty##_free_singles
#define pool_capacity(ty) __##ty##_pool_capacity

typedef void (*pool_clean_fun)();

typedef struct {
  pool_clean_fun *items;
  size_t count, capacity;
} __pool_clean_da;

extern __pool_clean_da __pools_to_clean;

void __cleanup_pools(void);

#define setup_pool(ty)                                                         \
  typedef struct {                                                             \
    void **items;                                                              \
    size_t count, capacity;                                                    \
  } __##ty##_freelist_t;                                                       \
                                                                               \
  typedef struct pooltype(ty) pooltype(ty);                                    \
  struct pooltype(ty) {                                                        \
    size_t capacity, filled;                                                   \
    ty *data;                                                                  \
    size_t *ref_counts; /* Parallel array of ref counts */                     \
    pooltype(ty) * next;                                                       \
  };                                                                           \
                                                                               \
  __##ty##_freelist_t singles(ty) = {0};                                       \
  pooltype(ty) *ty##_current_pool = NULL;                                      \
  pooltype(ty) *ty##_first_pool = NULL;                                        \
                                                                               \
  void init_pool(ty)(pooltype(ty) * pool) {                                    \
    pool->capacity = 1024;                                                     \
    pool->filled = 0;                                                          \
    pool->data = malloc(sizeof(ty) * pool->capacity);                          \
    pool->ref_counts = calloc(pool->capacity, sizeof(size_t));                 \
    pool->next = NULL;                                                         \
  }                                                                            \
  void ty##_pool_cleanup(void) {                                               \
    pooltype(ty) *pool = ty##_first_pool;                                      \
    while (pool != NULL) {                                                     \
      pooltype(ty) *next = pool->next;                                         \
      free(pool->data);                                                        \
      free(pool->ref_counts);                                                  \
      free(pool);                                                              \
      pool = next;                                                             \
    }                                                                          \
    free(singles(ty).items);                                                   \
    singles(ty) = (__##ty##_freelist_t){0};                                    \
    ty##_first_pool = NULL;                                                    \
    ty##_current_pool = NULL;                                                  \
  }                                                                            \
                                                                               \
  /* Find which pool owns this pointer and get its ref count */                \
  size_t *ty##_get_ref_count_internal(pooltype(ty) * pool, void *ptr) {        \
    while (pool != NULL) {                                                     \
      if (pool->data != NULL) {                                                \
        intptr_t base = (intptr_t)pool->data;                                  \
        intptr_t end = (intptr_t)&pool->data[pool->capacity];                  \
        intptr_t p = (intptr_t)ptr;                                            \
        if (p >= base && p < end) {                                            \
          size_t index = ((ty *)ptr) - pool->data;                             \
          return &pool->ref_counts[index];                                     \
        }                                                                      \
      }                                                                        \
      pool = pool->next;                                                       \
    }                                                                          \
    return NULL;                                                               \
  }                                                                            \
                                                                               \
  ty *ty##_pool_allocate_internal(pooltype(ty) * pool) {                       \
    if (pool == NULL)                                                          \
      return NULL;                                                             \
                                                                               \
    /* Reuse from free list */                                                 \
    if (singles(ty).count > 0) {                                               \
      ty *reused = singles(ty).items[--singles(ty).count];                     \
      size_t *rc = ty##_get_ref_count_internal(ty##_first_pool, reused);       \
      if (rc)                                                                  \
        *rc = 1;                                                               \
      return reused;                                                           \
    }                                                                          \
                                                                               \
    /* Ensure pool is initialized */                                           \
    if (pool->data == NULL) {                                                  \
      init_pool(ty)(pool);                                                     \
    }                                                                          \
                                                                               \
    /* Pool full, go to next */                                                \
    if (pool->filled >= pool->capacity) {                                      \
      if (pool->next == NULL) {                                                \
        pool->next = malloc(sizeof(*pool->next));                              \
        init_pool(ty)(pool->next);                                             \
      }                                                                        \
      ty##_current_pool = pool->next;                                          \
      return ty##_pool_allocate_internal(pool->next);                          \
    }                                                                          \
                                                                               \
    size_t idx = pool->filled++;                                               \
    pool->ref_counts[idx] = 1;                                                 \
    return &pool->data[idx];                                                   \
  }                                                                            \
                                                                               \
  ty *ty##_pool_allocate(void) {                                               \
    if (ty##_first_pool == NULL) {                                             \
      da_append(&__pools_to_clean, &ty##_pool_cleanup);                        \
      ty##_first_pool = malloc(sizeof(*ty##_first_pool));                      \
      init_pool(ty)(ty##_first_pool);                                          \
      ty##_current_pool = ty##_first_pool;                                     \
    }                                                                          \
    return ty##_pool_allocate_internal(ty##_current_pool);                     \
  }                                                                            \
                                                                               \
  void ty##_free_internal(void *ptr) {                                         \
    if (ptr == NULL)                                                           \
      return;                                                                  \
    da_append(&singles(ty), ptr);                                              \
  }                                                                            \
                                                                               \
  void ty##_release(void *ptr) {                                               \
    if (ptr == NULL)                                                           \
      return;                                                                  \
    size_t *rc = ty##_get_ref_count_internal(ty##_first_pool, ptr);            \
    if (rc == NULL)                                                            \
      return;                                                                  \
    if (--(*rc) == 0) {                                                        \
      ty##_free_internal(ptr);                                                 \
    }                                                                          \
  }                                                                            \
                                                                               \
  void ty##_retain(void *ptr) {                                                \
    if (ptr == NULL)                                                           \
      return;                                                                  \
    size_t *rc = ty##_get_ref_count_internal(ty##_first_pool, ptr);            \
    if (rc == NULL)                                                            \
      return;                                                                  \
    (*rc)++;                                                                   \
  }                                                                            \
                                                                               \
  int ty##_is_unique(void *ptr) {                                              \
    if (ptr == NULL)                                                           \
      return 0;                                                                \
    size_t *rc = ty##_get_ref_count_internal(ty##_first_pool, ptr);            \
    return rc && *rc == 1;                                                     \
  }

#endif // POOL_H
