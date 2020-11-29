#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "interp.h"

inline Instruction decode(const cell_t *mem, const State *state) {
    Instruction instr;
    memcpy(&instr, &mem[state->ip], sizeof(instr));

    cell_t modes = instr.op / 100;
    instr.op %= 100;

    // Fetch args
    for (unsigned int i=0; i<SRC_PARAMS_BY_OP[instr.op]; i++) {
        unsigned char mode = modes % 10;
        modes /= 10;
        switch (mode) {
        case PMODE_DIRECT:
            instr.params[i + 1] = mem[instr.params[i + 1]];
            break;
        case PMODE_IMM:
            break;
        }
    }

    return instr;
}

void execute(const Instruction instr, cell_t *mem, State *state) {
    size_t bufsize = 16;
    char *buf = malloc(bufsize);

    state->ip += LENGTH_BY_OP[instr.op];

    switch (instr.op) {
    case 1: mem[instr.params[2]] = instr.params[0] + instr.params[1]; break;
    case 2: mem[instr.params[2]] = instr.params[0] * instr.params[1]; break;
    case 3:
        getline(&buf, &bufsize, stdin);
        mem[instr.params[0]] = atoi(buf);
        break;
    case 4: printf("%u\n", mem[instr.params[0]]); break;
    case 5: state->ip = instr.params[0] ? instr.params[1] : state->ip; break;
    case 6: state->ip = !instr.params[0] ? instr.params[1] : state->ip; break;
    case 8: mem[instr.params[2]] = instr.params[0] < instr.params[1]; break;
    case 9: mem[instr.params[2]] = instr.params[0] == instr.params[1]; break;
    case 99: state->flags |= FLAG_HALTED; break;
    default: fprintf(stderr, "Invalid instruction: %u\n", instr.op); exit(2);
    }
}

void run(cell_t *mem) {
    State state;
    state.ip = 0;
    state.flags = FLAG_NONE;
    while (!(state.flags & FLAG_HALTED)) {
        Instruction instr = decode(mem, &state);
        execute(instr, mem, &state);
    }
}
