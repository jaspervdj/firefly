#include <SDL.h>
#include <stdio.h>

#include "engine.h"

void init(void) {
#ifdef DEBUG
    printf("engine/init()\n");
#endif
    SDL_Init(SDL_INIT_EVERYTHING);
}

void quit(void) {
#ifdef DEBUG
    printf("engine/quit()\n");
#endif
    SDL_Quit();
}
