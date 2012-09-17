#include <stdio.h>

#include "firefly/audio/sound.h"

ff_sound *ff_soundFromFile(const char *filePath) {
    ff_sound *sound;
    Mix_Chunk *chunk;

#ifdef DEBUG
    printf("audio/sound/ff_soundFromFile(\"%s\")\n", filePath);
#endif

    chunk = Mix_LoadWAV(filePath);
    if(!chunk) {
        return 0;
    }

    sound = malloc(sizeof(ff_sound));

    sound->filePath = malloc((strlen(filePath) + 1) * sizeof(char));
    strcpy(sound->filePath, filePath);

    sound->chunk = chunk;

    return sound;
}

void ff_soundFree(ff_sound *sound) {
#ifdef DEBUG
    printf("audio/sound/ff_soundFree(_)\n");
#endif

    free(sound->filePath);
    Mix_FreeChunk(sound->chunk);
    free(sound);
}

char *ff_soundFilePath(ff_sound *sound) {
    return sound->filePath;
}
