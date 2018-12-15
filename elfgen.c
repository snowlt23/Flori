#include "flori.h"
#include <elf.h>

void write_elf_header(FILE* fp, size_t entryoffset) {
  Elf64_Ehdr hdr = {};
  hdr.e_ident[0] = '\177';
  hdr.e_ident[1] = 'E';
  hdr.e_ident[2] = 'L';
  hdr.e_ident[3] = 'F';
  hdr.e_ident[4] = ELFCLASS64;
  hdr.e_ident[5] = ELFDATA2LSB;
  hdr.e_ident[6] = EV_CURRENT;
  hdr.e_type = ET_EXEC;
  hdr.e_machine = EM_X86_64;
  hdr.e_version = EV_CURRENT;
  hdr.e_entry = 0x400000 + sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr) + entryoffset;
  hdr.e_phoff = sizeof(Elf64_Ehdr);
  hdr.e_shoff = 0;
  hdr.e_ehsize = sizeof(Elf64_Ehdr);
  hdr.e_phentsize = sizeof(Elf64_Phdr);
  hdr.e_phnum = 1;
  hdr.e_shentsize = sizeof(Elf64_Shdr);
  hdr.e_shnum = 0;
  hdr.e_shstrndx = SHN_UNDEF;
  fwrite(&hdr, sizeof(hdr), 1, fp);
}

void write_program_header(FILE* fp, size_t codesize) {
  Elf64_Phdr hdr = {};
  hdr.p_type = PT_LOAD;
  hdr.p_offset = 0;
  hdr.p_vaddr = 0x400000;
  hdr.p_paddr = 0x400000;
  hdr.p_filesz = sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr) + codesize;
  hdr.p_memsz = sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr) + codesize;
  hdr.p_flags = PF_X | PF_W | PF_R;
  hdr.p_align = 0x200000;
  fwrite(&hdr, sizeof(hdr), 1, fp);
}

void write_program_code(FILE* fp, uint8_t* codeptr, size_t codesize) {
  fwrite(codeptr, codesize, 1, fp);
}

void write_elf_executable(FILE* fp, uint8_t* codeptr, size_t codesize, size_t entryoffset) {
  write_elf_header(fp, entryoffset);
  write_program_header(fp, codesize);
  write_program_code(fp, codeptr, codesize);
}
