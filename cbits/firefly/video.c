#include <GL/gl.h>
#include <math.h>
#include <SDL.h>
#include <stdio.h>

#include "firefly/video.h"

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
    glVertex3d(x, y, 0.0d);
}

void ff_drawTexture(ff_texture *texture) {
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texture->texture);

    glBegin(GL_QUADS);

    glTexCoord2f(texture->ltc, texture->ttc);
    glVertex2f(0.0f, 0.0f);

    glTexCoord2f(texture->ltc, texture->btc);
    glVertex2f(0.0f, (GLfloat) texture->height);

    glTexCoord2f(texture->rtc, texture->btc);
    glVertex2f((GLfloat) texture->width, (GLfloat) texture->height);

    glTexCoord2f(texture->rtc, texture->ttc);
    glVertex2f((GLfloat) texture->width, 0.0f);

    glEnd();
}

void ff_drawTextureCentered(ff_texture *texture) {
    glPushMatrix();
    glTranslatef((float) -texture->width * 0.5f,
            (float) -texture->height * 0.5f, 0.0f);
    ff_drawTexture(texture);
    glPopMatrix();
}

void ff_drawTextureDebug(ff_texture *texture) {
    glColor3f(1.0f, 1.0f, 1.0f);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texture->texture);

    glBegin(GL_QUADS);
    glTexCoord2f(0.0f, 0.0f);
    glVertex2f(0.0f, 0.0f);
    glTexCoord2f(0.0f, 1.0f);
    glVertex2f(0.0f, (GLfloat) texture->th);
    glTexCoord2f(1.0f, 1.0f);
    glVertex2f((GLfloat) texture->tw, (GLfloat) texture->th);
    glTexCoord2f(1.0f, 0.0f);
    glVertex2f((GLfloat) texture->tw, 0.0f);
    glEnd();

    glDisable(GL_TEXTURE_2D);
    glColor3f(1.0f, 0.0f, 0.0f);
    glBegin(GL_LINE_LOOP);
    glVertex2f(0.0f, 0.0f);
    glVertex2f(0.0f, (GLfloat) texture->height);
    glVertex2f((GLfloat) texture->width, (GLfloat) texture->height);
    glVertex2f((GLfloat) texture->width, 0.0f);
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
        ff_drawTexture(glyph->texture);
        glTranslatef((GLfloat) (glyph->advance - glyph->left),
                (GLfloat) glyph->top, 0.0f);
    }
    glPopMatrix();
}

void ff_drawStringCentered(ff_font *font,
        const unsigned long *string, int stringLength) {
    double width = ff_fontStringWidth(font, string, stringLength);

    glPushMatrix();
    glTranslated(-width * 0.5d, 0.0d, 0.0d);
    ff_drawString(font, string, stringLength);
    glPopMatrix();
}

void ff_pushMatrix() {
    glPushMatrix();
}

void ff_popMatrix() {
    glPopMatrix();
}

void ff_translate(double x, double y) {
    glTranslated(x, y, 0.0d);
}

void ff_rotate(double r) {
    glRotated(r * 180.0d / M_PI, 0.0d, 0.0d, 1.0d);
}

void ff_scale(double x, double y) {
    glScaled(x, y, 1.0d);
}

void ff_setColor(double r, double g, double b, double a) {
    glColor4d(r, g, b, a);
}

void ff_getColor(double *rgba) {
    glGetDoublev(GL_CURRENT_COLOR, rgba);
}
