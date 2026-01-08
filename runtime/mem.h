#include <stddef.h>

void register_object(void *ptr);
void borrow_object(void *ptr);
void drop_object(void *ptr);
size_t *get_ref_count(void *ptr);
int is_unique(void *ptr);
