#ifndef RUNTIME_H
#define RUNTIME_H

#include "da.h"
#include "mem.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print_lst(void *lst);

void print_int(int x);
void print_string(const char *const str);
int random_int(int max);
void start(void);

#endif // RUNTIME_H
