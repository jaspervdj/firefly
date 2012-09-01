#include <GL/gl.h>
#include <SDL.h>
#include <stdio.h>

#include "video.h"

void video_setMode(int width, int height) {
#ifdef DEBUG
    printf("video_setMode(%d, %d)\n", width, height);
#endif
    SDL_SetVideoMode(width, height, 24, SDL_OPENGL | SDL_DOUBLEBUF);


    glEnable(GL_TEXTURE_2D);
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glViewport(0, 0, width, height);
     
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0.0f, width, height, 0.0f, -1.0f, 1.0f);
     
    glMatrixMode(GL_MODELVIEW);
}

int video_screenWidth(void) {
    SDL_Surface *screen = SDL_GetVideoSurface();
    return screen->w;
}


int video_screenHeight(void) {
    SDL_Surface *screen = SDL_GetVideoSurface();
    return screen->h;
}


void video_startFrame(void) {
    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();
}

void video_endFrame(void) {
    SDL_GL_SwapBuffers();
}

void video_startLine(void) {
    glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
    glBegin(GL_LINE_STRIP);
}

void video_endLine(void) {
    glEnd();
}

void video_vertex(double x, double y) {
    glVertex3d(x, y, 0.0f);
}
