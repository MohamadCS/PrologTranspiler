#pragma once

#define ENABLE_DEBUG 1

#if defined(ENABLE_DEBUG) && ENABLE_DEBUG == 1
#define DEBUG(line) line
#define LOG(line) std::cout << "(" << __FUNCTION__ << "," << __LINE__ << "):" << line << std::endl
#else
#define DEBUG(line)
#define LOG(line)
#endif


#define CHECK_NULL(ptr)   \
    if (ptr == nullptr) { \
        LOG("Nullptr"); \
        throw std::invalid_argument("Null pointer exception"); \
    }
