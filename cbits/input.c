#include <SDL.h>
#include <stdio.h>

#include "input.h"

/* Globals, but trust me, it's okay. */
int global_quit = 0;
int global_mouseX = 0;
int global_mouseY = 0;
int global_mouseButtons = 0;

void ff_flushInput(void) {
    SDL_Event event;
    while(SDL_PollEvent(&event)) {
        switch(event.type) {
            case SDL_QUIT:
#ifdef DEBUG
                printf("input/ff_flushInput: quit received\n");
                global_quit = 1;
#endif
                break;
            default:
                break;
        }
    }

    global_mouseButtons = SDL_GetMouseState(&global_mouseX, &global_mouseY);
}

int ff_receivedQuit(void) {
    return global_quit;
}

int ff_keyDown(int key) {
    /* TODO We can probably make keyState global */
    int numkeys;
    Uint8 *keyState = SDL_GetKeyState(&numkeys);
    return (int) keyState[key];
}

int ff_mouseX(void) {
    return global_mouseX;
}

int ff_mouseY(void) {
    return global_mouseY;
}

int ff_mouseButtonDown(int mouseButton) {
    return SDL_BUTTON(mouseButton) & global_mouseButtons;
}
