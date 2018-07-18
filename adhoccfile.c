
bool included = false;

%%hook enum {
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
  if (getc(stdin) != ';') {
    error("expected ; token in %%%%enum");
    included = true;
  }

  printf("typedef enum { %s } %s;\n", string_cstr(enumsrc), string_cstr(enumname));
  if (!included) adprintf("#include \"flori.h\"\n");
  adprintf("char* %s_tostring(%s kind) { %s; return \"unknownkind\"; }\n", string_cstr(enumname), string_cstr(enumname), string_cstr(tostrsrc));
}

%%hook fstruct {
  skip_spaces();
  string* fsname = parse_ident();
  skip_spaces();
  string* objname = parse_ident();
  skip_spaces();
  string* blk = parse_block();

  printf("typedef struct { int index; } %s;\n", cstr(fsname));
  printf("typedef struct %s %s;\n", cstr(blk), cstr(objname));

  if (!included) {
    adprintf("#include \"flori.h\"\n");
    included = true;
  }
  printf("%s alloc_%s();\n", cstr(fsname), cstr(fsname));
  adprintf("%s alloc_%s() { return (%s){linmem_alloc(sizeof(%s))}; }\n", cstr(fsname), cstr(fsname), cstr(fsname), cstr(objname));
  printf("%s* %s_ptr(%s x);\n", cstr(objname), cstr(fsname), cstr(fsname));
  adprintf("%s* %s_ptr(%s x) { return (%s*)linmem_toptr(x.index); }\n", cstr(objname), cstr(fsname), cstr(fsname), cstr(objname));
}
