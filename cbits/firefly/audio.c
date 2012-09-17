#include <math.h>
#include <SDL_mixer.h>
#include <stdio.h>

#include "firefly/audio.h"

#define NUM_CHANNELS 16

Mix_Music *global_music = 0;
char *global_musicFilePath = 0;
double global_musicVolume = 1.0d;

void ff_initAudio(void) {
#ifdef DEBUG
    printf("audio/ff_initAudio()\n");
#endif

    Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT, 2, 1024);
    Mix_AllocateChannels(NUM_CHANNELS);
}

void ff_quitAudio(void) {
#ifdef DEBUG
    printf("audio/ff_quitAudio()\n");
#endif

    ff_stopMusic();
    Mix_CloseAudio();
}

void ff_playMusic(const char *filePath, int loop) {
#ifdef DEBUG
    printf("audio/ff_playMusic(\"%s\", %d)\n", filePath, loop);
#endif

    ff_stopMusic();

    global_music = Mix_LoadMUS(filePath);

    if(!global_music) {
        fprintf(stderr, "audio/ff_playMusic: Couldn't load music\n");
        return;
    }

    global_musicFilePath = malloc((strlen(filePath) + 1) * sizeof(char));
    strcpy(global_musicFilePath, filePath);

    Mix_PlayMusic(global_music, loop);
}

void ff_stopMusic() {
#ifdef DEBUG
    printf("audio/ff_stopMusic()\n");
#endif

    Mix_HaltMusic();

    if(global_music) {
        Mix_FreeMusic(global_music);
        free(global_musicFilePath);
        global_music = 0;
        global_musicFilePath = 0;
    }
}

void ff_setMusicVolume(double volume) {
    int v = (int) (volume * (double) MIX_MAX_VOLUME + 0.5d);
    global_musicVolume = volume;
    Mix_VolumeMusic(v);
}

double ff_getMusicVolume(void) {
    return global_musicVolume;
}

void ff_playSound(ff_sound *sound) {
    int channel = ff_getFreeChannel();
    Mix_PlayChannel(channel, sound->chunk, 0);
}

void ff_playSoundPanning(ff_sound *sound, double panning, double distance) {
    int channel = ff_getFreeChannel();
    Uint8 right = (Uint8) (panning * 254.0d);
    Mix_SetDistance(channel, (Uint8) (distance * 255.0d));
    Mix_SetPanning(channel, 254 - right, right);
    Mix_PlayChannel(channel, sound->chunk, 0);
}

void ff_playSoundPosition(ff_sound *sound, double angle, double distance) {
    int channel = ff_getFreeChannel();
    int degrees = (int) (angle * 180.0d / M_PI);

    degrees += 90;  /* Adjust for inconsistency between polars and SDL_Mixer */
    degrees %= 360;
    if(degrees < 0) degrees += 360;

    Mix_SetPosition(channel, (Sint16) degrees, (Uint8) (distance * 255.0d));
    Mix_PlayChannel(channel, sound->chunk, 0);
}

int ff_getFreeChannel(void) {
    int c = 0;

    while(c < NUM_CHANNELS && Mix_Playing(c)) c++;

    if(c >= NUM_CHANNELS) {
        /* Oops. */
        return -1;
    } else {
        Mix_UnregisterAllEffects(c);
        return c;
    }
}
