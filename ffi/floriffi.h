
#include <stdint.h>

typedef void* flori_fexpr;
typedef long flori_int;

flori_fexpr (*flori_new_fident)(char* s);
flori_fexpr (*flori_new_fseq)();
flori_fexpr (*flori_new_farray)();
flori_fexpr (*flori_new_flist)();
flori_fexpr (*flori_new_fblock)();
flori_fexpr (*flori_new_fintlit)(flori_int s);
flori_fexpr (*flori_new_fstrlit)(char* s);
flori_fexpr (*flori_parse_fexpr)(char* filename, flori_int line, flori_int linepos, char* s);
flori_fexpr (*flori_print_fexpr)(char* s);
flori_fexpr (*flori_quoted)(flori_fexpr fexpr);
flori_int (*flori_length)(flori_fexpr fexpr);
void (*flori_push)(flori_fexpr fexpr, flori_fexpr son);
flori_int (*flori_kind)(flori_fexpr fexpr);
void (*flori_expect)(flori_fexpr fexpr, int kind);
void (*flori_error)(flori_fexpr fexpr, char* msg);
flori_fexpr (*flori_access)(flori_fexpr fexpr, flori_int i);
void (*flori_set)(flori_fexpr fexpr, flori_int i, flori_fexpr value);
char* (*flori_to_cs)(flori_fexpr fexpr);
flori_int (*flori_intval)(flori_fexpr fexpr);
char* (*flori_strval)(flori_fexpr fexpr);
flori_fexpr (*flori_gensym)();
flori_fexpr (*flori_get_type)(flori_fexpr fexpr);
char* (*flori_get_srcexpr)(flori_fexpr fexpr);
