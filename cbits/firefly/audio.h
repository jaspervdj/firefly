#ifndef FIREFLY_AUDIO_H
#define FIREFLY_AUDIO_H

#include "firefly/audio/sound.h"

void ff_initAudio(void);
void ff_quitAudio(void);

void ff_playMusic(const char *filePath, int loop);
void ff_stopMusic(void);
void ff_setMusicVolume(double volume);
double ff_getMusicVolume(void);

void ff_playSound(ff_sound *sound);
void ff_playSoundPanning(ff_sound *sound, double panning, double distance);
void ff_playSoundPosition(ff_sound *sound, double angle, double distance);

int ff_getFreeChannel(void);

#endif
