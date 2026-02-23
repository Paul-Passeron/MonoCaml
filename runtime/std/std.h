#ifndef STD_H
#define STD_H

// Printing functions
void __std_print_endline(char *);
void __std_print_string(char *);
void __std_print_int(int);

#ifdef STD_IMPL
void __std_print_endline(char *s) { printf("%s\n", s); }

void __std_print_string(char *s) { printf("%s", s); }

void __std_print_int(int n) { printf("%d", n); }

#endif // STD_IMPL

#endif // STD_H
