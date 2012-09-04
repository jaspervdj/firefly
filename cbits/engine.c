#include <SDL.h>
#include <stdio.h>

#include "engine.h"

void ff_init(void) {
#ifdef DEBUG
    printf("engine/ff_init()\n");
#endif
    SDL_Init(SDL_INIT_EVERYTHING);
}

void ff_quit(void) {
#ifdef DEBUG
    printf("engine/ff_quit()\n");
#endif
    SDL_Quit();
}

int ff_getTicks(void) {
    return SDL_GetTicks();
}

void ff_delay(int ms) {
    SDL_Delay(ms);
}
