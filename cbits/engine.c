#include <SDL.h>
#include <stdio.h>

#include "engine.h"

void engine_init(void) {
#ifdef DEBUG
    printf("engine_init()\n");
#endif
    SDL_Init(SDL_INIT_EVERYTHING);
}

void engine_quit(void) {
#ifdef DEBUG
    printf("engine_quit()\n");
#endif
    SDL_Quit();
}
