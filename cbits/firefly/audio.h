#ifndef FIREFLY_AUDIO_H
#define FIREFLY_AUDIO_H

void ff_initAudio(void);
void ff_quitAudio(void);

void ff_playMusic(const char *filePath, int loop);
void ff_stopMusic();

#endif
