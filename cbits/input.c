#include <SDL.h>
#include <stdio.h>

#include "input.h"

/* Globals, but trust me, it's okay. */
int global_quit = 0;
int global_mouseX = 0;
int global_mouseY = 0;

void input_flush(void) {
    SDL_Event event;
    while(SDL_PollEvent(&event)) {
        switch(event.type) {
            case SDL_QUIT:
#ifdef DEBUG
                printf("input_flush(): quit received\n");
                global_quit = 1;
#endif
                break;
            default:
                break;
        }
    }

    SDL_GetMouseState(&global_mouseX, &global_mouseY);
}

int input_quit(void) {
    return global_quit;
}

int input_keyDown(int key) {
    /* TODO We can probably make keyState global */
    int numkeys;
    Uint8 *keyState = SDL_GetKeyState(&numkeys);
    return (int) keyState[key];
}

int input_mouseX(void) {
    return global_mouseX;
}

int input_mouseY(void) {
    return global_mouseY;
}
