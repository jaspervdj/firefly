#include <SDL.h>
#include <stdio.h>

#include "firefly/audio.h"
#include "firefly/engine.h"

void ff_init(void) {
#ifdef DEBUG
    printf("engine/ff_init()\n");
#endif

    SDL_Init(SDL_INIT_EVERYTHING);
    ff_initAudio();
}

void ff_quit(void) {
#ifdef DEBUG
    printf("engine/ff_quit()\n");
#endif

    ff_quitAudio();
    SDL_Quit();
}

int ff_getTicks(void) {
    return SDL_GetTicks();
}

void ff_delay(int ms) {
    SDL_Delay(ms);
}
