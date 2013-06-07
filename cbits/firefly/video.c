#include <GL/gl.h>
#include <math.h>
#include <SDL.h>
#include <stdio.h>

#include "firefly/video.h"

#define VIDEO_FLAGS (SDL_OPENGL | SDL_DOUBLEBUF)

int global_fullScreen = 0;
int global_screenWidth = 0;
int global_screenHeight = 0;

void ff_setVideoMode(int width, int height, int fullScreen) {
    int flags = VIDEO_FLAGS;
    Uint32 bpp;

#ifdef DEBUG
    printf("video/ff_setVideoMode(%d, %d, %d)\n",
            width, height, fullScreen);
#endif
    global_fullScreen = fullScreen;

    /* Figure out bits per pixel */
    bpp = SDL_GetVideoInfo()->vfmt->BitsPerPixel;

    if(fullScreen) flags |= SDL_FULLSCREEN;
    SDL_SetVideoMode(width, height, bpp, flags);

    /* Enable textures, and transparent texures */
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glViewport(0, 0, width, height);
    ff_setScreenSize(width, height);

#ifdef DEBUG
    printf("video/ff_setVideoMode: Using Open %s by %s on renderer %s\n",
            glGetString(GL_VERSION), glGetString(GL_VENDOR),
            glGetString(GL_RENDERER));
#endif
}

void ff_setScreenSize(int width, int height) {
    global_screenWidth = width;
    global_screenHeight = height;

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0.0f, width, height, 0.0f, -1.0f, 1.0f);
    glMatrixMode(GL_MODELVIEW);
}

int ff_getScreenWidth(void) {
    return global_screenWidth;
}

int ff_getScreenHeight(void) {
    return global_screenHeight;
}

int ff_isFullScreen(void) {
    return global_fullScreen;
}

int ff_getFullScreenModes(int maxModes, int *modes) {
    int i;
    SDL_Rect **rects;

    rects = SDL_ListModes(0, SDL_FULLSCREEN | VIDEO_FLAGS);

    for(i = 0; i < maxModes && rects[i]; i++) {
        modes[i * 2] = rects[i]->w;
        modes[i * 2 + 1] = rects[i]->h;
    }

    return i;
}

void ff_setShowCursor(int showCursor) {
    SDL_ShowCursor(showCursor ? SDL_ENABLE : SDL_DISABLE);
}

int ff_isShowCursor(void) {
    int showCursor = SDL_ShowCursor(SDL_QUERY);
    return showCursor == SDL_ENABLE ? 1 : 0;
}

void ff_startFrame(void) {
    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();
    glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
}

void ff_endFrame(void) {
    SDL_GL_SwapBuffers();
}

void ff_startLine(void) {
    glBegin(GL_LINE_STRIP);
}

void ff_endLine(void) {
    glEnd();
}

void ff_startTriangles(void) {
    glBegin(GL_TRIANGLES);
}

void ff_endTriangles(void) {
    glEnd();
}

void ff_startQuads(void) {
    glBegin(GL_QUADS);
}

void ff_endQuads(void) {
    glEnd();
}

void ff_vertex(double x, double y) {
    glVertex3d(x, y, 0.0);
}

void ff_drawCircle(double x, double y, double r, int steps) {
    int i;
    double th = 0.0;
    double d = 2.0 * M_PI / (double) steps;

    glBegin(GL_TRIANGLE_FAN);
    glVertex3d(x, y, 0.0);

    for(i = 0; i < steps; i++) {
        glVertex3d(x + r * cos(th), y + r * sin(th), 0.0);
        th += d;
    }

    glVertex3d(x + r, y, 0.0);  /* Closing point */

    glEnd();
}

void ff_drawTexture(double x, double y, ff_texture *texture) {
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texture->texture);

    glBegin(GL_QUADS);

    glTexCoord2f(texture->ltc, texture->ttc);
    glVertex2f(x, y);

    glTexCoord2f(texture->ltc, texture->btc);
    glVertex2f(x, y + (GLfloat) texture->height);

    glTexCoord2f(texture->rtc, texture->btc);
    glVertex2f(x + (GLfloat) texture->width, y + (GLfloat) texture->height);

    glTexCoord2f(texture->rtc, texture->ttc);
    glVertex2f(x + (GLfloat) texture->width, y);

    glEnd();

    glDisable(GL_TEXTURE_2D);
}

void ff_drawTextureCentered(double x, double y, ff_texture *texture) {
    glPushMatrix();
    glTranslatef((GLfloat) -texture->width * 0.5f,
            (GLfloat) -texture->height * 0.5f, 0.0f);
    ff_drawTexture(x, y, texture);
    glPopMatrix();
}

void ff_drawTextureDebug(double x, double y, ff_texture *texture) {
    glColor3f(1.0f, 1.0f, 1.0f);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texture->texture);

    glBegin(GL_QUADS);
    glTexCoord2f(0.0f, 0.0f);
    glVertex2f(x, y);
    glTexCoord2f(0.0f, 1.0f);
    glVertex2f(x, y + (GLfloat) texture->th);
    glTexCoord2f(1.0f, 1.0f);
    glVertex2f(x + (GLfloat) texture->tw, y + (GLfloat) texture->th);
    glTexCoord2f(1.0f, 0.0f);
    glVertex2f(x + (GLfloat) texture->tw, y);
    glEnd();

    glDisable(GL_TEXTURE_2D);
    glColor3f(1.0f, 0.0f, 0.0f);
    glBegin(GL_LINE_LOOP);
    glVertex2f(x, y);
    glVertex2f(x, y + (GLfloat) texture->height);
    glVertex2f(x + (GLfloat) texture->width, y + (GLfloat) texture->height);
    glVertex2f(x + (GLfloat) texture->width, x);
    glEnd();
    glDisable(GL_TEXTURE_2D);
}

void ff_drawTextures(double *xys, ff_texture** textures, int num) {
    int i;
    double x, y;
    ff_texture *texture;

    if(num <= 0) return;

    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, textures[0]->texture);
    glBegin(GL_QUADS);

    for(i = 0; i < num; i++) {
        x = xys[i * 2];
        y = xys[i * 2 + 1];
        texture = textures[i];

        glTexCoord2f(texture->ltc, texture->ttc);
        glVertex2f(x, y);

        glTexCoord2f(texture->ltc, texture->btc);
        glVertex2f(x, y + (GLfloat) texture->height);

        glTexCoord2f(texture->rtc, texture->btc);
        glVertex2f(x + (GLfloat) texture->width, y + (GLfloat) texture->height);

        glTexCoord2f(texture->rtc, texture->ttc);
        glVertex2f(x + (GLfloat) texture->width, y);
    }

    glEnd();
    glDisable(GL_TEXTURE_2D);
}

void ff_drawString(double x, double y, ff_font *font,
        const unsigned long *string, int stringLength) {
    int i;
    ff_glyph *glyph;

    glPushMatrix();
    glTranslatef(x, y + font->ascent, 0.0f);
    for(i = 0; i < stringLength; i++) {
        glyph = ff_fontLookupGlyph(font, string[i]);

        glTranslatef((GLfloat) glyph->left, (GLfloat) -glyph->top, 0.0f);
        ff_drawTexture(0.0, 0.0, glyph->texture);
        glTranslatef((GLfloat) (glyph->advance - glyph->left),
                (GLfloat) glyph->top, 0.0f);
    }
    glPopMatrix();
}

void ff_drawStringCentered(double x, double y, ff_font *font,
        const unsigned long *string, int stringLength) {
    double width = ff_fontStringWidth(font, string, stringLength);

    glPushMatrix();
    glTranslated(-width * 0.5, 0.0, 0.0);
    ff_drawString(x, y, font, string, stringLength);
    glPopMatrix();
}

void ff_pushMatrix(void) {
    glPushMatrix();
}

void ff_popMatrix(void) {
    glPopMatrix();
}

void ff_translate(double x, double y) {
    glTranslated(x, y, 0.0);
}

void ff_rotate(double r) {
    glRotated(r * 180.0 / M_PI, 0.0, 0.0, 1.0);
}

void ff_scale(double x, double y) {
    glScaled(x, y, 1.0);
}

void ff_setColor(double r, double g, double b, double a) {
    glColor4d(r, g, b, a);
}

void ff_getColor(double *rgba) {
    glGetDoublev(GL_CURRENT_COLOR, rgba);
}

void ff_setBackgroundColor(double r, double g, double b, double a) {
    glClearColor(r, g, b, a);
}
