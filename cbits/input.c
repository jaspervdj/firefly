#include <SDL.h>
#include <stdio.h>

#include "input.h"

/* Globals, but trust me, it's okay. */
int input_quit = 0;

void input_flush(void) {
    SDL_Event event;
    while(SDL_PollEvent(&event)) {
        switch(event.type) {
            case SDL_QUIT:
#ifdef DEBUG
                printf("input_flush(): quit received\n");
                input_quit = 1;
#endif
                break;
            default:
                break;
        }
    }
}

int input_isQuit(void) {
    return input_quit;
}

int input_isKeyDown(int key) {
    /* TODO We can probably make keyState global */
    int numkeys;
    Uint8 *keyState = SDL_GetKeyState(&numkeys);
    return (int) keyState[key];
}
