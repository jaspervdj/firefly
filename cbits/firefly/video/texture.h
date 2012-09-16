#ifndef FIREFLY_VIDEO_TEXTURE_H
#define FIREFLY_VIDEO_TEXTURE_H

#include <GL/gl.h>

typedef struct {
    int width, height;
    int pixelSize;
    int tw, th; /* Powers of two */
    float ttc, rtc, btc, ltc; /* Texture coordinates */

    int *refcount;
    GLuint texture;
} ff_texture;

ff_texture *ff_textureCreate(int width, int height, int pixelSize,
        GLubyte *pixels);
ff_texture *ff_textureFromGradient(int width, int height);
ff_texture *ff_textureFromPng(const char *filePath);
void ff_textureFree(ff_texture *texture);

ff_texture *ff_textureSlice(ff_texture *texture,
        int x, int y, int width, int height);

GLenum ff_formatForPixelSize(int pixelSize);
int ff_nearestPowerOfTwo(int x);
void ff_copyPixels(GLubyte *src, int sw, int sh,
        GLubyte *dst, int dw, int dh, int pixelSize);

#endif
