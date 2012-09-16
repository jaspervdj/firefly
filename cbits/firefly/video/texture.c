#include <stdio.h>
#include <stdlib.h>
#include <png.h>

#include "firefly/video/texture.h"

ff_texture *ff_textureCreate(int width, int height, int pixelSize,
        GLubyte *pixels) {
    ff_texture *texture;
    GLenum format;
    GLubyte *tp;

#ifdef DEBUG
    printf("video/texture/ff_textureCreate(%d, %d, %d, _)\n", width, height,
            pixelSize);
#endif

    texture = malloc(sizeof(ff_texture));

    texture->width = width;
    texture->height = height;
    texture->pixelSize = pixelSize;
    texture->tw = ff_nearestPowerOfTwo(width);
    texture->th = ff_nearestPowerOfTwo(height);
    texture->ttc = 0.0f;
    texture->rtc = (float) (width) / (float) (texture->tw);
    texture->btc = (float) (height) / (float) (texture->th);
    texture->ltc = 0.0f;
    texture->refcount = malloc(sizeof(int));

    *(texture->refcount) = 1;

    glGenTextures(1, &texture->texture);
    glBindTexture(GL_TEXTURE_2D, texture->texture);

    /* TODO: GL_NEAREST is more pixel-perfect, GL_LINEAR is nicer, possibly let
     * the user choose? */
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    format = ff_formatForPixelSize(pixelSize);

    /* Write pixel data to video memory */

    tp = malloc(texture->tw * texture->th * pixelSize * sizeof(GLubyte));
    ff_copyPixels(pixels, width, height, tp, texture->tw, texture->th, pixelSize);

    glTexImage2D(GL_TEXTURE_2D, 0, format, texture->tw, texture->th, 0, format,
            GL_UNSIGNED_BYTE, (GLvoid *) tp);

    free(tp);

    return texture;
}

ff_texture *ff_textureFromGradient(int width, int height) {
    GLubyte *pixels;
    int x, y;
    int r, g, b;
    ff_texture *texture;

    pixels = malloc(width * height * 3 * sizeof(GLubyte));
    for(y = 0; y < height; y++) {
        for(x = 0; x < width; x++) {

            r = (GLubyte) (0xff * (float) x / (float) width);
            g = (GLubyte) (0xff * (float) y / (float) height);
            b = 0xff;

            pixels[y * width * 3 + x * 3] = r;
            pixels[y * width * 3 + x * 3 + 1] = g;
            pixels[y * width * 3 + x * 3 + 2] = b;
        }
    }

    texture = ff_textureCreate(width, height, 3, pixels);
    free(pixels);
    return texture;
}

#define PNG_BYTES_TO_CHECK 4

ff_texture *ff_textureFromPng(const char *filePath) {
    FILE *file;
    png_byte buffer[PNG_BYTES_TO_CHECK];
    png_structp pngp;
    png_infop infop;
    png_uint_32 width, height;
    int bitDepth, colorType, interlaceType;
    png_byte **rowp;
    int x, y, b;
    int pixelSize;
    GLubyte *pixels;
    ff_texture *texture;

#ifdef DEBUG
    printf("video/texture/ff_textureFromPng(\"%s\")\n", filePath);
#endif

    /* Open PNG file */
    file = fopen(filePath, "rb");
    if(!file) {
        fprintf(stderr, "video/texture/ff_textureFromPng: Could not open \"%s\"\n",
                filePath);
        return 0;
    }

    /* Check signature */
    b = fread(buffer, 1, PNG_BYTES_TO_CHECK, file);
    if(b != PNG_BYTES_TO_CHECK ||
            png_sig_cmp(buffer, (png_size_t) 0, PNG_BYTES_TO_CHECK)) {
        fprintf(stderr,
                "video/texture/ff_textureFromPng: PNG signature in \"%s\"\n",
                filePath);
        fclose(file);
        return 0;
    }

    /* Initialize handles */
    pngp = png_create_read_struct(PNG_LIBPNG_VER_STRING, 0, 0, 0);
    infop = png_create_info_struct(pngp);
    setjmp(png_jmpbuf(pngp));

    /* Prepare for reading */
    png_init_io(pngp, file);
    png_set_sig_bytes(pngp, PNG_BYTES_TO_CHECK);

    /* Read */
    png_read_png(pngp, infop, PNG_TRANSFORM_IDENTITY, 0);
    png_get_IHDR(pngp, infop, &width, &height, &bitDepth, &colorType,
            &interlaceType, 0, 0);

    pixelSize = png_get_rowbytes(pngp, infop) / width;
    pixels = malloc(width * height * pixelSize * sizeof(GLubyte));

    /* Copy data to pixels array */
    rowp = png_get_rows(pngp, infop);
    for(y = 0; y < height; y++) {
        for(x = 0; x < width; x++) {
            for(b = 0; b < pixelSize; b++) {
                pixels[width * pixelSize * y + x * pixelSize + b] =
                        rowp[y][x * pixelSize + b];
            }
        }
    }

    png_destroy_read_struct(&pngp, &infop, 0);
    fclose(file);

    texture = ff_textureCreate(width, height, pixelSize, pixels);

    free(pixels);

    return texture;
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
    sub->pixelSize = texture->pixelSize;
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

GLenum ff_formatForPixelSize(int pixelSize) {
    switch(pixelSize) {
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

void ff_copyPixels(GLubyte *src, int sw, int sh,
        GLubyte *dst, int dw, int dh, int pixelSize) {
    int x, y, b;

#ifdef DEBUG
    printf("video/texture/ff_copyPixels(_, %d, %d, _, %d, %d, %d)\n",
            sw, sh, dw, dh, pixelSize);
#endif

    for(y = 0; y < dh; y++) {
        for(x = 0; x < dw; x++) {
            for(b = 0; b < pixelSize; b++) {
                dst[dw * pixelSize * y + pixelSize * x + b] =
                    (x < sw && y < sh) ?
                        src[sw * pixelSize * y + pixelSize * x + b] : 0xff;
            }
        }
    }
}
