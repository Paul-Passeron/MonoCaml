#include "runtime.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define da_reserve(da, expected_capacity)                                      \
  do {                                                                         \
    if ((expected_capacity) > (da)->capacity) {                                \
      if ((da)->capacity == 0) {                                               \
        (da)->capacity = 32;                                                   \
      }                                                                        \
      while ((expected_capacity) > (da)->capacity) {                           \
        (da)->capacity *= 2;                                                   \
      }                                                                        \
      (da)->items =                                                            \
          realloc((da)->items, (da)->capacity * sizeof(*(da)->items));         \
      assert((da)->items != NULL && "Buy more RAM lol");                       \
    }                                                                          \
  } while (0)

#define da_append(da, item)                                                    \
  do {                                                                         \
    da_reserve((da), (da)->count + 1);                                         \
    (da)->items[(da)->count++] = (item);                                       \
  } while (0)

#define da_free(da) free((da).items)

#define da_resize(da, new_size)                                                \
  do {                                                                         \
    da_reserve((da), new_size);                                                \
    (da)->count = (new_size);                                                  \
  } while (0)

typedef struct {
  size_t count;
  void *ptr;
} ref;

typedef struct {
  ref *items;
  size_t count;
  size_t capacity;
} refs;

typedef struct {
  size_t *items;
  size_t count;
  size_t capacity;
} ints;

#define REF_COUNT 256

static refs references[REF_COUNT] = {0};
static ints free_refs[REF_COUNT] = {0};

size_t hash(intptr_t in) { return (in >> 3) % REF_COUNT; }

__inline__ void register_closure(void *ptr) {
  size_t hashed = hash((intptr_t)ptr);
  refs *refs = &references[hashed];
  for (size_t i = 0; i < refs->count; ++i) {
    if (refs->items[i].ptr == ptr) {
      return;
    }
  }
  ref ref = {.count = 0, .ptr = ptr};
  if (free_refs[hashed].count > 0) {
    int idx = free_refs[hashed].items[--free_refs[hashed].count];
    refs->items[idx] = ref;
    return;
  }
  da_append(refs, ref);
}

ref *get_ref(const void *const ptr, size_t *h, size_t *idx) {
  size_t hashed = hash((intptr_t)ptr);
  if (h)
    *h = hashed;
  refs *refs = &references[hashed];
  for (size_t i = 0; i < refs->count; ++i) {
    if (refs->items[i].ptr == ptr) {
      if (idx)
        *idx = i;
      return &refs->items[i];
    }
  }
  fprintf(stderr, "RUNTIME ERROR: CANNOT FIND REFERENCE TO CLOSURE...\n");
  fprintf(stderr, "ABORTING NOW...\n");
  exit(1);
}

__inline__ void borrow_closure(void *env) {
  if (!env)
    return;
  ref *ref = get_ref(env, NULL, NULL);
  ref->count++;
}

__inline__ void drop_closure(void *env) {
  if (!env)
    return;
  size_t hash, idx;
  ref *r = get_ref(env, &hash, &idx);
  r->count--;
  if (r->count == 0) {
    free(env);
    da_append(&free_refs[hash], idx);
    references[hash].items[idx] = (ref){0};
  }
}

void cleanup(void) {
  for (size_t i = 0; i < REF_COUNT; ++i) {
    refs da = references[i];
    for (size_t j = 0; j < da.count; ++j) {
      if (da.items[j].ptr) {
        free(da.items[j].ptr);
        da.items[j] = (ref){0};
      }
    }
    da_free(da);
    da_free(free_refs[i]);
  }
}

void print_int(int x) { printf("%d", x); }

int main() {
  srand(time(NULL));
  start();
  cleanup();
  return 0;
}

void print_string(const char *const str) { printf("%s", str); }

int random_int(int max) { return rand() % max; }
