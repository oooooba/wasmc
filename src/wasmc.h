#ifndef _WASMC_H_
#define _WASMC_H_

#include <stdint.h>

struct MemoryInstance {
    uint8_t *vec;
    uint64_t len;
    int64_t max;
};

extern struct MemoryInstance memory;

#define WASMC_PAGE_SIZE 65536

void _wasmc_allocate_memory(
        struct MemoryInstance *memory_instance,
        uint32_t min_num_pages,
        int64_t max_num_pages);

void _wasmc_initialize_memory(
        struct MemoryInstance *memory_instance,
        int32_t offset,
        uint8_t *initial_vec,
        uint64_t initial_vec_len);

#endif // _WASMC_H_
