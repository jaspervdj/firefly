#include <SDL_mixer.h>
#include <stdio.h>

#include "firefly/audio.h"

Mix_Music *global_music = 0;
char *global_musicFilePath = 0;

void ff_initAudio(void) {
#ifdef DEBUG
    printf("audio/ff_initAudio()\n");
#endif

    Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT, 2, 1024);
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
