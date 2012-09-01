#include <SDL.h>
#include <stdio.h>

#include "input.h"

/* Globals, but trust me, it's okay. */
int inputQuit = 0;

void flush(void) {
    SDL_Event event;
    while(SDL_PollEvent(&event)) {
        switch(event.type) {
            case SDL_QUIT:
#ifdef DEBUG
                printf("input/flush(): quit received\n");
                inputQuit = 1;
#endif
                break;
            default:
                break;
        }
    }
}

int isQuit(void) {
    return inputQuit;
}
