#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include "wasmc.h"

void _wasmc_allocate_memory(
        struct MemoryInstance *memory_instance,
        uint32_t min_num_pages,
        int64_t max_num_pages) {
    printf("_wasmc_allocate_memory: %p, %d, %ld\n", memory_instance, min_num_pages, max_num_pages);
    assert (max_num_pages < (1l << 32));
    uint64_t len = WASMC_PAGE_SIZE * min_num_pages;
    uint8_t *vec = mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (vec == MAP_FAILED) {
        perror("mmap");
        exit(1);
    }
    memory_instance->vec = vec;
    memory_instance->len = len;
    memory_instance->max = max_num_pages;
}

void _wasmc_initialize_memory(
        struct MemoryInstance *memory_instance,
        int32_t offset,
        uint8_t *initial_vec,
        uint64_t initial_vec_len) {
    printf("_wasmc_initialize_memory: %p, %d, %p, %ld\n", memory_instance, offset, initial_vec, initial_vec_len);
    memcpy(memory_instance->vec + offset, initial_vec, initial_vec_len);
}
