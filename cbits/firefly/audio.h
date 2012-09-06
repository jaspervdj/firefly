#ifndef FIREFLY_AUDIO_H
#define FIREFLY_AUDIO_H

#include "firefly/audio/sound.h"

void ff_initAudio(void);
void ff_quitAudio(void);

void ff_playMusic(const char *filePath, int loop);
void ff_stopMusic();

void ff_playSound(ff_sound *sound);

#endif
