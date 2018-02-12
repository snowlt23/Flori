
typedef void* flori_fexpr;

flori_fexpr (*flori_new_fident)(char* s);
flori_fexpr (*flori_new_fseq)();
flori_fexpr (*flori_new_fblock)();
flori_fexpr (*flori_parse_fexpr)(char* s);
flori_fexpr (*flori_print_fexpr)(char* s);
void (*flori_add_son)(flori_fexpr fexpr, flori_fexpr son);
void (*flori_expect)(flori_fexpr fexpr, int kind);
flori_fexpr (*flori_access)(flori_fexpr fexpr, int i);
