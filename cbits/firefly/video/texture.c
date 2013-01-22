#include <stdio.h>
#include <stdlib.h>

#include "firefly/video/texture.h"

ff_texture *ff_textureFromImage(ff_image *image) {
    ff_texture *texture;
    GLenum format;
    GLubyte *tp;

#ifdef DEBUG
    printf("video/texture/ff_textureFromImage(_)\n");
#endif

    texture = malloc(sizeof(ff_texture));

    texture->width = image->width;
    texture->height = image->height;
    texture->bpp = image->bpp;
    texture->tw = ff_nearestPowerOfTwo(image->width);
    texture->th = ff_nearestPowerOfTwo(image->height);
    texture->ttc = 0.0f;
    texture->rtc = (float) (image->width) / (float) (texture->tw);
    texture->btc = (float) (image->height) / (float) (texture->th);
    texture->ltc = 0.0f;
    texture->refcount = malloc(sizeof(int));

    *(texture->refcount) = 1;

    glGenTextures(1, &texture->texture);
    glBindTexture(GL_TEXTURE_2D, texture->texture);

    /* TODO: GL_NEAREST is more pixel-perfect, GL_LINEAR is nicer, possibly let
     * the user choose? */
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

    format = ff_formatForPixelSize(texture->bpp);

    /* Write pixel data to video memory */

    tp = malloc(texture->tw * texture->th * texture->bpp * sizeof(GLubyte));
    ff_copyPixels(image, tp, texture->tw, texture->th);

    glTexImage2D(GL_TEXTURE_2D, 0, format, texture->tw, texture->th, 0, format,
            GL_UNSIGNED_BYTE, (GLvoid *) tp);

    free(tp);

    return texture;
}

ff_texture *ff_textureFromPng(const char *filePath) {
    ff_image *image;
    ff_texture *texture;
#ifdef DEBUG
    printf("video/texture/ff_textureFromPng(\"%s\")\n", filePath);
#endif

    image = ff_imageFromPng(filePath);
    if(image) {
        texture = ff_textureFromImage(image);
        free(image);
        return texture;
    } else {
        return 0;
    }
}

void ff_textureFree(ff_texture *texture) {
#ifdef DEBUG
    printf("video/texture/ff_textureFree(_)\n");
#endif

    (*(texture->refcount))--;

#ifdef DEBUG
    printf("video/texture/ff_textureFree: Refcount is now: %d\n",
            *(texture->refcount));
#endif

    if(*(texture->refcount) <= 0 && texture->texture) {
        free(texture->refcount);
        glDeleteTextures(1, &texture->texture);
    }

    free(texture);
}

int ff_textureWidth(ff_texture *texture) {
    return texture->width;
}

int ff_textureHeight(ff_texture *texture) {
    return texture->height;
}

ff_texture *ff_textureSlice(ff_texture *texture,
        int x, int y, int width, int height) {
    ff_texture *sub;

#ifdef DEBUG
    printf("video/texture/ff_textureSlice(_, %d, %d, %d, %d)\n",
            x, y, width, height);
#endif

    sub = malloc(sizeof(ff_texture));
    sub->width = width;
    sub->height = height;
    sub->bpp = texture->bpp;
    sub->tw = texture->tw;
    sub->th = texture->th;
    sub->ttc = (float) y / (float) texture->th;
    sub->rtc = (float) (x + width) / (float) texture->tw;
    sub->btc = (float) (y + height) / (float) texture->th;
    sub->ltc = (float) x / (float) texture->tw;
    sub->refcount = texture->refcount;
    sub->texture = texture->texture;

    (*(sub->refcount))++;

#ifdef DEBUG
    printf("video/texture/ff_textureSlice: Refcount is now: %d\n",
            *(sub->refcount));
#endif

    return sub;
}

GLenum ff_formatForPixelSize(int bpp) {
    switch(bpp) {
        case 1:
            return GL_ALPHA;
        case 2:
            return GL_LUMINANCE_ALPHA;
        case 3:
            return GL_RGB;
        case 4: default:
            return GL_RGBA;
    }
}

int ff_nearestPowerOfTwo(int x) {
    int y = 1;
    while(y < x) y <<= 1;
    return y;
}

void ff_copyPixels(ff_image *image, GLubyte *dst, int dw, int dh) {
    int x, y, b;
    int sw, sh, bpp;

#ifdef DEBUG
    printf("video/texture/ff_copyPixels(_, _, %d, %d)\n", dw, dh);
#endif

    sw = image->width;
    sh = image->height;
    bpp = image->bpp;
    for(y = 0; y < dh; y++) {
        for(x = 0; x < dw; x++) {
            for(b = 0; b < bpp; b++) {
                dst[dw * bpp * y + bpp * x + b] =
                    (x < sw && y < sh) ?
                        (GLubyte) image->pixels[sw * bpp * y + bpp * x + b] :
                        0x00;
            }
        }
    }
}
