#ifndef STD_H
#define STD_H

// Printing functions
void print_endline(char *);
void print_string(char *);
void print_int(int);

#ifdef STD_IMPL
void print_endline(char *s) { printf("%s\n", s); }
void print_string(char *s) { printf("%s", s); }
void print_int(int n) { printf("%d", n); }

#endif // STD_IMPL

#endif // STD_H
