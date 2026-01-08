#include "runtime.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

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

__inline__ void register_object(void *ptr) {
#ifdef DEBUG
  printf("registering %p\n", ptr);
#endif // DEBUG
  size_t hashed = hash((intptr_t)ptr);
  refs *refs = &references[hashed];
  for (size_t i = 0; i < refs->count; ++i) {
    if (refs->items[i].ptr == ptr) {
      return;
    }
  }
  ref ref = {.count = 1, .ptr = ptr};
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
  fprintf(stderr, "RUNTIME ERROR: CANNOT FIND REFERENCE TO OBJECT...\n");
  fprintf(stderr, "ABORTING NOW...\n");
  exit(1);
}

__inline__ void borrow_object(void *env) {
#ifdef DEBUG
  printf("borrowing %p\n", env);
#endif // DEBUG

  if (!env)
    return;
  ref *ref = get_ref(env, NULL, NULL);
  ref->count++;
}

__inline__ void drop_object(void *env) {
#ifdef DEBUG
  printf("dropping %p\n", env);
#endif // DEBUG
  if (!env)
    return;
  size_t hash, idx;
  ref *r = get_ref(env, &hash, &idx);
  r->count--;
  if (r->count == 0) {
    free(env);
    #ifdef DEBUG_FREE
        printf("FREE %p\n", env);
    #endif // DEBUG_FREE
    da_append(&free_refs[hash], idx);
    references[hash].items[idx] = (ref){0};
  }
}

__inline__ size_t *get_ref_count(void *ptr) {
    size_t hash, idx;
    ref *r = get_ref(ptr, &hash, &idx);
    return &r->count;
}

__inline__ int is_unique(void *ptr) {
    return *get_ref_count(ptr) == 1;
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

typedef enum {
  MY_LST_NIL,
  MY_LST_CONS,
} my_lst_discr;
typedef struct my_lst my_lst;

struct my_lst {
  int _0;
  union {
    struct {
      int _0;
      my_lst *_1;
    } cons;
  } _1;
};

void print_lst_aux(my_lst *ptr, int first) {
lbl:
  if (ptr->_0 != 0) {
    if (!first) {
      printf(", ");
    }
    printf("%d", ptr->_1.cons._0);
    ptr = ptr->_1.cons._1;
    first = 0;
    goto lbl;
  }
}

my_lst *lst_nil(void) {
  my_lst *res = malloc(sizeof(*res));
  register_object(res);
  res->_0 = 0;
  return res;
}

my_lst *lst_cons(int elt, my_lst *next) {
  my_lst *res = malloc(sizeof(*res));
  register_object(res);
  res->_0 = 1;
  res->_1.cons._0 = elt;
  res->_1.cons._1 = next;
  return res;
}

void print_lst(void *lst) {
  printf("[");
  print_lst_aux(lst, 1);
  printf("]\n");
}

int main() {
  srand(time(NULL));
  start();
  cleanup();
  return 0;
}

void print_string(const char *const str) { printf("%s", str); }

int random_int(int max) { return rand() % max; }
