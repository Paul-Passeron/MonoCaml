#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void register_closure(void *env);
void borrow_closure(void *env);
void drop_closure(void *env);

void print_int(int x);
void print_string(const char *const str);
void start(void);
