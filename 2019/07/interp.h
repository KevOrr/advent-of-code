#ifndef _INTCODE_INTERP_H
#define _INTCODE_INTERP_H

#include <stdint.h>

#define _INTCODE_MAX_LENGTH 4

typedef uint32_t cell_t;

typedef enum param_mode {
    PMODE_DIRECT = 0,
    PMODE_IMM = 1
} param_mode;

typedef struct instruction {
    cell_t op;
    cell_t params[_INTCODE_MAX_LENGTH - 1];
} Instruction;

typedef enum flags {
    FLAG_NONE = 0,
    FLAG_HALTED = 1
} Flags;

typedef struct state {
    Flags flags;
    cell_t ip;
} State;

const unsigned char LENGTH_BY_OP[256] = {
    [1] = 4,
    [2] = 4,
    [3] = 2,
    [4] = 2,
    [5] = 3,
    [6] = 3,
    [7] = 4,
    [8] = 4,
    [99] = 1,
};

const unsigned char SRC_PARAMS_BY_OP[256] = {
    [1] = 2,
    [2] = 2,
    [3] = 0,
    [4] = 1,
    [5] = 2,
    [6] = 2,
    [7] = 2,
    [8] = 2,
    [99] = 0,
};

Instruction decode(const cell_t *mem, const State *state);

void execute(const Instruction instr, cell_t *mem, State *state);

void run(cell_t *mem);

#endif
