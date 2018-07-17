FILE* tostringfile = NULL;

%%hook enum {
  if (tostringfile == NULL){
    tostringfile = fopen("tostring.c", "a");
  }

  skip_spaces();
  string* enumname = parse_ident();
  skip_spaces();
  if (getc(stdin) != '{') error("expected { token in %%%%enum");
  skip_spaces();

  string* enumsrc = empty_string();
  string* tostrsrc = empty_string();

  for (;;) {
    skip_spaces();
    char c = getc(stdin);
    if (c == '}') break;
    ungetc(c, stdin);

    string* kind = parse_ident();

    string* s = empty_string();
    format(s, "%s, ", cstr(kind));
    pushs(enumsrc, cstr(s));
    format(s, "if (kind == %s) return \"%s\";", cstr(kind), cstr(kind));
    pushs(tostrsrc, cstr(s));

    skip_spaces();
    c = getc(stdin);
    if (c == ',') continue;
    ungetc(c, stdin);
  }
  if (getc(stdin) != ';') error("expected ; token in %%%%enum");

  printf("typedef enum { %s } %s;\n", string_cstr(enumsrc), string_cstr(enumname));
  fprintf(tostringfile, "#include \"flori.h\"\n");
  fprintf(tostringfile, "char* %s_tostring(%s kind) { %s; return \"unknownkind\"; }\n", string_cstr(enumname), string_cstr(enumname), string_cstr(tostrsrc));
}
