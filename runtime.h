#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void register_object(void *env);
void borrow_object(void *env);
void drop_object(void *env);

void print_lst(void *lst);

void print_int(int x);
void print_string(const char *const str);
int random_int(int max);
void start(void);
