#include <GL/gl.h>
#include <math.h>
#include <SDL.h>
#include <stdio.h>

#include "video.h"

void ff_setVideoMode(int width, int height) {
#ifdef DEBUG
    printf("video/ff_setVideoMode(%d, %d)\n", width, height);
#endif
    SDL_SetVideoMode(width, height, 24, SDL_OPENGL | SDL_DOUBLEBUF);

    /* Enable textures, and transparent texures */
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glViewport(0, 0, width, height);
     
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0.0f, width, height, 0.0f, -1.0f, 1.0f);
     
    glMatrixMode(GL_MODELVIEW);
}

int ff_getScreenWidth(void) {
    SDL_Surface *screen = SDL_GetVideoSurface();
    return screen->w;
}

int ff_getScreenHeight(void) {
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

    glTexCoord2f(image->ltc, image->ttc);
    glVertex2f(0.0f, 0.0f);

    glTexCoord2f(image->ltc, image->btc);
    glVertex2f(0.0f, (GLfloat) image->height);

    glTexCoord2f(image->rtc, image->btc);
    glVertex2f((GLfloat) image->width, (GLfloat) image->height);

    glTexCoord2f(image->rtc, image->ttc);
    glVertex2f((GLfloat) image->width, 0.0f);

    glEnd();
}

void ff_drawImageCentered(ff_image *image) {
    glPushMatrix();
    glTranslatef((float) -image->width * 0.5f,
            (float) -image->height * 0.5f, 0.0f);
    ff_drawImage(image);
    glPopMatrix();
}

void ff_drawImageDebug(ff_image *image) {
    glColor3f(1.0f, 1.0f, 1.0f);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, image->texture);

    glBegin(GL_QUADS);
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

void ff_drawString(ff_font *font,
        const unsigned long *string, int stringLength) {
    int i;
    ff_glyph *glyph;

    glPushMatrix();
    for(i = 0; i < stringLength; i++) {
        glyph = ff_fontLookupGlyph(font, string[i]);

        glTranslatef((GLfloat) glyph->left, (GLfloat) -glyph->top, 0.0f);
        ff_drawImage(glyph->image);
        glTranslatef((GLfloat) (glyph->advance - glyph->left),
                (GLfloat) glyph->top, 0.0f);
    }
    glPopMatrix();
}

void ff_pushMatrix() {
    glPushMatrix();
}

void ff_popMatrix() {
    glPopMatrix();
}

void ff_translate(double x, double y) {
    glTranslated(x, y, 0.0f);
}

void ff_rotate(double r) {
    glRotated(r * 180 / M_PI, 0.0f, 0.0f, 1.0f);
}

void ff_scale(double x, double y) {
    glScaled(x, y, 1.0f);
}

void ff_setColor(double r, double g, double b, double a) {
    glColor4d(r, g, b, a);
}

void ff_getColor(double *rgba) {
    glGetDoublev(GL_CURRENT_COLOR, rgba);
}
