#include <stdio.h>

#include "firefly/audio/music.h"

ff_music *ff_musicFromFile(const char *filePath) {
    ff_music *music;
    Mix_Music *mus;

#ifdef DEBUG
    printf("audio/music/ff_musicFromFile(\"%s\")\n", filePath);
#endif

    mus = Mix_LoadMUS(filePath);
    if(!mus) {
        return 0;
    }

    music = malloc(sizeof(ff_music));

    music->filePath = malloc((strlen(filePath) + 1) * sizeof(char));
    strcpy(music->filePath, filePath);

    music->music = mus;

    return music;
}

void ff_musicFree(ff_music *music) {
#ifdef DEBUG
    printf("audio/music/ff_musicFree(_)\n");
#endif

    free(music->filePath);
    Mix_FreeMusic(music->music);
    free(music);
}
