#ifndef FIREFLY_VIDEO_TEXTURE_H
#define FIREFLY_VIDEO_TEXTURE_H

#include <GL/gl.h>

#include "firefly/video/image.h"

typedef struct {
    int id;
    int width, height;
    int bpp;
    int tw, th; /* Powers of two */
    float ttc, rtc, btc, ltc; /* Texture coordinates */

    int *refcount;
    GLuint texture;
} ff_texture;

ff_texture *ff_textureFromImage(ff_image *image);
ff_texture *ff_textureFromPng(const char *filePath);
void ff_textureFree(ff_texture *texture);

int ff_textureId(ff_texture *texture);
int ff_textureWidth(ff_texture *texture);
int ff_textureHeight(ff_texture *texture);

ff_texture *ff_textureSlice(ff_texture *texture,
        int x, int y, int width, int height);

GLenum ff_formatForPixelSize(int bpp);
int ff_nearestPowerOfTwo(int x);
void ff_copyPixels(ff_image *image, GLubyte *dst, int dw, int dh);

#endif
