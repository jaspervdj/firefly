#ifndef FIREFLY_AUDIO_SOUND_H
#define FIREFLY_AUDIO_SOUND_H

#include <SDL_mixer.h>

typedef struct {
    char *filePath;
    Mix_Chunk *chunk;
} ff_sound;

ff_sound *ff_soundFromFile(const char *filePath);
void ff_soundFree(ff_sound *sound);

char *songFilePath(ff_sound *sound);

#endif
