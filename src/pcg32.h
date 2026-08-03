// PCG Random Number Generation for C++
//
// Copyright 2014-2019 Melissa O'Neill <oneill@pcg-random.org>,
//                     and the PCG Project contributors.
//
// SPDX-License-Identifier: (Apache-2.0 OR MIT)
//
// Licensed under the Apache License, Version 2.0 (provided in
// LICENSE-APACHE.txt) and the MIT License (provided in
// LICENSE-MIT.txt).

#ifndef PCG32_H
#define PCG32_H

#include <cstdint>

class pcg32 {
    uint64_t state;
    uint64_t inc;
public:
    using result_type = uint32_t;
    static constexpr uint32_t min() { return 0; }
    static constexpr uint32_t max() { return UINT32_MAX; }

    pcg32(uint64_t initstate = 0x853c49e6748fea9bULL, uint64_t initseq = 0xda3e39cb94b95bdbULL) {
        state = 0U;
        inc = (initseq << 1u) | 1u;
        operator()();
        state += initstate;
        operator()();
    }

    uint32_t operator()() {
        uint64_t oldstate = state;
        state = oldstate * 6364136223846793005ULL + inc;
        uint32_t xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
        uint32_t rot = oldstate >> 59u;
        return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
    }
};

#endif
