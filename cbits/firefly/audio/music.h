#ifndef FIREFLY_AUDIO_MUSIC_H
#define FIREFLY_AUDIO_MUSIC_H

#include <SDL_mixer.h>

typedef struct {
    char *filePath;
    Mix_Music *music;
} ff_music;

ff_music *ff_musicFromFile(const char *filePath);
void ff_musicFree(ff_music *music);

#endif
