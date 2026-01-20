#include <stddef.h>

#ifndef MEM_H
#define MEM_H

void register_object(void *ptr);
void borrow_object(void *ptr);
void drop_object(void *ptr);
size_t *get_ref_count(void *ptr);
int is_unique(void *ptr);

#endif // MEM_H
