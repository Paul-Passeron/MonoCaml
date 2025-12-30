#include <stdio.h>
#include <stdlib.h>
#include <time.h>



void print_string(const char *const str) { printf("%s", str); }

int random_int(int max) { return rand() % max; }
void print_int(int x) { printf("%d", x); }

int fact(int n) {
    if (n > 0) {
        return n * fact(n-1);
    } else {
        return 1;
    }
}

void loo(int n) {
    if (n) {
        int res = fact(random_int(10));
        print_int(res);
        print_string("\n");
        loo(n-1);
    } else {
        print_string("Done !\n");
    }
}

int main(void) {
    srand(time(NULL));
    loo(100000);
    return 0;
}
