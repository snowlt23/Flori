
typedef void* flori_fexpr;

flori_fexpr (*flori_new_fident)(char* s);
flori_fexpr (*flori_new_fblock)();
flori_fexpr (*flori_parse_fexpr)(char* s);
flori_fexpr (*flori_print_fexpr)(char* s);
