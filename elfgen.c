#include "flori.h"
#include <elf.h>

#define BASE_ADDR 0x400000
#define ELF_ALIGN 0x1000

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
  hdr.e_entry = BASE_ADDR + sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr) + entryoffset;
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

void write_program_header(FILE* fp, size_t offset, size_t size) {
  Elf64_Phdr hdr = {};
  hdr.p_type = PT_LOAD;
  hdr.p_offset = offset;
  hdr.p_vaddr = BASE_ADDR + offset;
  hdr.p_paddr = BASE_ADDR + offset;
  hdr.p_filesz = size;
  hdr.p_memsz = size;
  hdr.p_flags = PF_X | PF_W | PF_R;
  hdr.p_align = ELF_ALIGN;
  fwrite(&hdr, sizeof(hdr), 1, fp);
}

void write_segment_body(FILE* fp, uint8_t* p, size_t size) {
  fwrite(p, size, 1, fp);
}

size_t calc_padding(size_t offset) {
  return ELF_ALIGN - (offset % ELF_ALIGN);
}

void write_padding(FILE* fp, size_t pad) {
  for (int i=0; i<pad; i++) {
    char zb = 0;
    fwrite(&zb, 1, 1, fp);
  }
}

void write_elf_executable(FILE* fp, uint8_t* codeptr, size_t codesize, uint8_t* dataptr, size_t datasize, size_t entryoffset) {
  size_t codesegsize = sizeof(Elf64_Phdr) + codesize;
  size_t pad1 = calc_padding(codesegsize);
  size_t datasegsize = sizeof(Elf64_Phdr) + datasize;
  size_t pad2 = calc_padding(codesegsize + pad1 + datasegsize);
  /* debug("C: %zd, %zd = %zd", codesize, pad1, sizeof(Elf64_Phdr)+codesize+pad1); */
  /* debug("D: %zd, %zd = %zd", datasize, pad2, sizeof(Elf64_Phdr)+datasize+pad2); */
  with_reloc((uint8_t*)(BASE_ADDR + sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr) + codesize + pad1 + sizeof(Elf64_Phdr)), {
    write_elf_header(fp, 2, entryoffset);

    write_program_header(fp, sizeof(Elf64_Ehdr), codesegsize + pad1);
    write_segment_body(fp, codeptr, codesize);
    write_padding(fp, pad1);
    
    write_program_header(fp, codesegsize + pad1, datasegsize + pad2);
    write_segment_body(fp, dataptr, datasize);
    write_padding(fp, pad2);
  });
}
