#include "flori.h"
#include <elf.h>

#define BASE_ADDR 0x400000

void write_elf_header(FILE* fp, size_t phnum, size_t entryoffset) {
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
  hdr.e_entry = BASE_ADDR + sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr)*phnum + entryoffset;
  hdr.e_phoff = sizeof(Elf64_Ehdr);
  hdr.e_shoff = 0;
  hdr.e_ehsize = sizeof(Elf64_Ehdr);
  hdr.e_phentsize = sizeof(Elf64_Phdr);
  hdr.e_phnum = phnum;
  hdr.e_shentsize = sizeof(Elf64_Shdr);
  hdr.e_shnum = 0;
  hdr.e_shstrndx = SHN_UNDEF;
  fwrite(&hdr, sizeof(hdr), 1, fp);
}

void write_program_header(FILE* fp, size_t phidx, size_t offset, size_t codesize) {
  Elf64_Phdr hdr = {};
  hdr.p_type = PT_LOAD;
  hdr.p_offset = 0;
  hdr.p_vaddr = BASE_ADDR + offset;
  hdr.p_paddr = BASE_ADDR + offset;
  hdr.p_filesz = sizeof(Elf64_Ehdr) + codesize;
  hdr.p_memsz = sizeof(Elf64_Ehdr) + codesize;
  hdr.p_flags = PF_X | PF_W | PF_R;
  hdr.p_align = BASE_ADDR / 2;
  fwrite(&hdr, sizeof(hdr), 1, fp);
}

void write_segment_body(FILE* fp, uint8_t* p, size_t size) {
  fwrite(p, size, 1, fp);
}

void write_elf_executable(FILE* fp, uint8_t* codeptr, size_t codesize, uint8_t* dataptr, size_t datasize, size_t entryoffset) {
  with_reloc((uint8_t*)(BASE_ADDR + sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr)), {
    write_elf_header(fp, 2, datasize + entryoffset);
    write_program_header(fp, 0, 0, datasize);
    write_segment_body(fp, dataptr, datasize);
    write_program_header(fp, 1, datasize, codesize);
    write_segment_body(fp, codeptr, codesize);
  });
}
