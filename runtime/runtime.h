#ifndef RUNTIME_H
#define RUNTIME_H

#include "da.h"
#include "mem.h"
#include "pool.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "std/std.h"
void print_lst(void *lst);
int random_int(int max);

void start(void);

#endif // RUNTIME_H
