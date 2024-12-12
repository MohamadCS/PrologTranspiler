#pragma once

#define ENABLE_DEBUG 1

#if defined(ENABLE_DEBUG) && ENABLE_DEBUG == 1
#define DEBUG(line) line
#else
#define DEBUG(line)
#endif


#define CHECK_NULL(ptr)   \
    if (ptr == nullptr) { \
        LOG("Nullptr"); \
        throw std::invalid_argument("Null pointer exception"); \
    }
