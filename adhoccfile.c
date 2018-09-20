
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
  }

  printf("typedef enum { %s } %s;\n", string_cstr(enumsrc), string_cstr(enumname));
  adprintf("char* %s_tostring(%s kind) { %s; return \"unknownkind\"; }\n", string_cstr(enumname), string_cstr(enumname), string_cstr(tostrsrc));
}

typedef struct {
  string* name;
  string* cbody;
  string* hbody;
} Template;

Template tnames[1024] = {};
int tnamepos = 0;

%%hook adinclude {
  skip_spaces();
  string* s = parse_string();
  adprintf("#include \"%s\"\n", s->data);
}

%%hook template {
  skip_spaces();
  string* name = parse_ident();
  skip_spaces();
  string* hbody = parse_block();
  if (hbody == NULL) error("template expect header block.");
  skip_spaces();
  string* cbody = parse_block();
  if (cbody == NULL) error("template expect c block.");
  tnames[tnamepos] = (Template){name, cbody, hbody};
  tnamepos++;
}

%%hook expand {
  skip_spaces();
  string* name = parse_ident();
  vector* args = parse_arguments();
  string* ctmpl = NULL;
  string* htmpl = NULL;
  for (int i=0; i<tnamepos; i++) {
    if (strcmp(tnames[i].name->data, name->data) == 0) {
      ctmpl = tnames[i].cbody;
      htmpl = tnames[i].hbody;
      break;
    }
  }
  if (ctmpl == NULL || htmpl == NULL) error("undefined %s template.", name->data);

  for (int i=1; i<htmpl->len-1; i++) {
    char c = htmpl->data[i];
    if (c == '%') {
      if (i+2 < htmpl->len && htmpl->data[i+1] == '%') {
        int n = htmpl->data[i+2] - '1';
        if (args->len <= n) error("%s expand hasn't %d argument.", name->data, n);
        printf("%s", ((string*)vector_get(args, n))->data);
        i+=2;
        continue;
      }
    }
    printf("%c", c);
  }
  for (int i=1; i<ctmpl->len-1; i++) {
    char c = ctmpl->data[i];
    if (c == '%') {
      if (i+2 < ctmpl->len && ctmpl->data[i+1] == '%') {
        int n = ctmpl->data[i+2] - '1';
        if (args->len <= n) error("%s expand hasn't %d argument.", name->data, n);
        adprintf("%s", ((string*)vector_get(args, n))->data);
        i+=2;
        continue;
      }
    }
    adprintf("%c", c);
  }
}

