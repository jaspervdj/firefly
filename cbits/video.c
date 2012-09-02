#include <GL/gl.h>
#include <SDL.h>
#include <stdio.h>

#include "video.h"

void ff_setVideoMode(int width, int height) {
#ifdef DEBUG
    printf("video/ff_setVideoMode(%d, %d)\n", width, height);
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

int ff_screenWidth(void) {
    SDL_Surface *screen = SDL_GetVideoSurface();
    return screen->w;
}


int ff_screenHeight(void) {
    SDL_Surface *screen = SDL_GetVideoSurface();
    return screen->h;
}


void ff_startFrame(void) {
    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();
}

void ff_endFrame(void) {
    SDL_GL_SwapBuffers();
}

void ff_startLine(void) {
    glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
    glBegin(GL_LINE_STRIP);
}

void ff_endLine(void) {
    glEnd();
}

void ff_vertex(double x, double y) {
    glVertex3d(x, y, 0.0f);
}

void ff_drawImage(ff_image *image) {
    glColor3f(1.0f, 1.0f, 1.0f);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, image->texture);

    glBegin(GL_QUADS);

    /*
    glTexCoord2f(image->ltc, image->ttc);
    glVertex2f(0.0f, 0.0f);

    glTexCoord2f(image->ltc, image->btc);
    glVertex2f(0.0f, (GLfloat) image->height);

    glTexCoord2f(image->rtc, image->btc);
    glVertex2f((GLfloat) image->tw, (GLfloat) image->height);

    glTexCoord2f(image->rtc, image->ttc);
    glVertex2f((GLfloat) image->width, 0.0f);
    */
    glTexCoord2f(0.0f, 0.0f);
    glVertex2f(0.0f, 0.0f);

    glTexCoord2f(0.0f, 1.0f);
    glVertex2f(0.0f, (GLfloat) image->th);

    glTexCoord2f(1.0f, 1.0f);
    glVertex2f((GLfloat) image->tw, (GLfloat) image->th);

    glTexCoord2f(1.0f, 0.0f);
    glVertex2f((GLfloat) image->tw, 0.0f);

    glEnd();

    glDisable(GL_TEXTURE_2D);
    glColor3f(1.0f, 0.0f, 0.0f);
    glBegin(GL_LINE_LOOP);
    glVertex2f(0.0f, 0.0f);
    glVertex2f(0.0f, (GLfloat) image->height);
    glVertex2f((GLfloat) image->width, (GLfloat) image->height);
    glVertex2f((GLfloat) image->width, 0.0f);
    glEnd();
}
