
typedef void* flori_fexpr;

flori_fexpr (*flori_new_fident)(char* s);
flori_fexpr (*flori_new_fseq)();
flori_fexpr (*flori_new_farray)();
flori_fexpr (*flori_new_flist)();
flori_fexpr (*flori_new_fblock)();
flori_fexpr (*flori_new_fstrlit)(char* s);
flori_fexpr (*flori_parse_fexpr)(char* s);
flori_fexpr (*flori_print_fexpr)(char* s);
int (*flori_length)(flori_fexpr fexpr);
void (*flori_push)(flori_fexpr fexpr, flori_fexpr son);
int (*flori_kind)(flori_fexpr fexpr);
void (*flori_expect)(flori_fexpr fexpr, int kind);
void (*flori_error)(flori_fexpr fexpr, char* msg);
flori_fexpr (*flori_access)(flori_fexpr fexpr, int i);
void (*flori_set)(flori_fexpr fexpr, int i, flori_fexpr value);
char* (*flori_to_cs)(flori_fexpr fexpr);
flori_fexpr (*flori_gensym)();
