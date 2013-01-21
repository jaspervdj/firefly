#include <SDL.h>
#include <stdio.h>
#include <string.h>

#include "firefly/input.h"

#define NUM_KEYS SDLK_LAST
#define NUM_MOUSEBUTTONS 16

/* Globals, but trust me, it's okay. */
int global_quit = 0;
int global_mouseX = 0;
int global_mouseY = 0;
int global_mouseButtons = 0;
char global_keyPressed[NUM_KEYS];
char global_keyReleased[NUM_KEYS];
char global_mouseButtonPressed[NUM_MOUSEBUTTONS];
char global_mouseButtonReleased[NUM_MOUSEBUTTONS];

void ff_setGrabInput(int grabInput) {
    SDL_WM_GrabInput(grabInput ? SDL_GRAB_ON : SDL_GRAB_OFF);
}

int ff_isGrabInput(void) {
    int grabInput = SDL_WM_GrabInput(SDL_GRAB_QUERY);
    return grabInput == SDL_GRAB_ON ? 1 : 0;
}

void ff_flushInput(void) {
    memset(global_keyPressed, 0, NUM_KEYS * sizeof(char));
    memset(global_keyReleased, 0, NUM_KEYS * sizeof(char));
    memset(global_mouseButtonPressed, 0, NUM_MOUSEBUTTONS * sizeof(char));
    memset(global_mouseButtonReleased, 0, NUM_MOUSEBUTTONS * sizeof(char));

    SDL_Event event;
    while(SDL_PollEvent(&event)) {
        switch(event.type) {
            case SDL_KEYDOWN:
                global_keyPressed[event.key.keysym.sym] = 1;
                break;
            case SDL_KEYUP:
                global_keyReleased[event.key.keysym.sym] = 1;
                break;
            case SDL_MOUSEBUTTONDOWN:
                global_mouseButtonPressed[event.button.button] = 1;
                break;
            case SDL_MOUSEBUTTONUP:
                global_mouseButtonReleased[event.button.button] = 1;
                break;
            case SDL_QUIT:
#ifdef DEBUG
                printf("input/ff_flushInput: quit received\n");
#endif
                global_quit = 1;
                break;
            default:
                break;
        }
    }

    global_mouseButtons = SDL_GetMouseState(&global_mouseX, &global_mouseY);
}

int ff_hasReceivedQuit(void) {
    return global_quit;
}

int ff_isKeyDown(int key) {
    /* TODO We can probably make keyState global */
    int numkeys;
    Uint8 *keyState = SDL_GetKeyState(&numkeys);
    return (int) keyState[key];
}

int ff_isKeyPressed(int key) {
    return (int) global_keyPressed[key];
}

int ff_isKeyReleased(int key) {
    return (int) global_keyReleased[key];
}

int ff_isMouseButtonDown(int mouseButton) {
    return SDL_BUTTON(mouseButton) & global_mouseButtons;
}

int ff_isMouseButtonPressed(int mouseButton) {
    return (int) global_mouseButtonPressed[mouseButton];
}

int ff_isMouseButtonReleased(int mouseButton) {
    return (int) global_mouseButtonReleased[mouseButton];
}

int ff_getMouseX(void) {
    return global_mouseX;
}

int ff_getMouseY(void) {
    return global_mouseY;
}
