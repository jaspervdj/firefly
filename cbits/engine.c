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
