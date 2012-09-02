#include <stdio.h>
#include <stdlib.h>

#include "video/image.h"

ff_image *ff_imageCreate(int width, int height, int pixelSize,
        GLubyte *pixels) {
    ff_image *image;
    GLenum format;
    GLubyte *tp;

#ifdef DEBUG
    printf("video/image/ff_imageCreate(%d, %d, %d, _)\n", width, height,
            pixelSize);
#endif

    image = malloc(sizeof(ff_image));

    image->width = width;
    image->height = height;
    image->pixelSize = pixelSize;
    image->tw = ff_nearestPowerOfTwo(width);
    image->th = ff_nearestPowerOfTwo(height);
    image->ttc = 0.0f;
    image->rtc = (float) (width) / (float) (image->tw);
    image->btc = (float) (height) / (float) (image->th);
    image->ltc = 0.0f;

    glGenTextures(1, &image->texture);
    glBindTexture(GL_TEXTURE_2D, image->texture);

    /* TODO: GL_NEAREST is more pixel-perfect, GL_LINEAR is nicer, possibly let
     * the user choose? */
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    format = ff_formatForPixelSize(pixelSize);

    /* Write pixel data to video memory */

    tp = malloc(image->tw * image->th * pixelSize * sizeof(GLubyte));
    ff_copyPixels(pixels, width, height, tp, image->tw, image->th, pixelSize);

    glTexImage2D(GL_TEXTURE_2D, 0, pixelSize, image->tw, image->th, 0, format,
            GL_UNSIGNED_BYTE, (GLvoid *) tp);

    free(tp);

    return image;
}

ff_image *ff_imageFromGradient(int width, int height) {
    GLubyte *pixels;
    int x, y;
    int r, g, b;
    ff_image *image;

    pixels = malloc(width * height * 3 * sizeof(GLubyte));
    for(x = 0; x < width; x++) {
        for(y = 0; y < height; y++) {

            r = (GLubyte) (0xff * (float) x / (float) width);
            g = (GLubyte) (0xff * (float) y / (float) height);
            b = 0xff;

            pixels[y * width * 3 + x * 3] = r;
            pixels[y * width * 3 + x * 3 + 1] = g;
            pixels[y * width * 3 + x * 3 + 2] = b;
        }
    }

    image = ff_imageCreate(width, height, 3, pixels);
    free(pixels);
    return image;
}

void ff_imageFree(ff_image *image) {
#ifdef DEBUG
    printf("video/image/ff_imageFree(_)\n");
#endif
    if(image->texture) glDeleteTextures(1, &image->texture);
    free(image);
}

GLenum ff_formatForPixelSize(int pixelSize) {
    switch(pixelSize) {
        case 1:
            return GL_LUMINANCE;
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
    printf("video/image/ff_copyPixels(_, %d, %d, _, %d, %d, %d)\n",
            sw, sh, dw, dh, pixelSize);
#endif

    for(x = 0; x < dw; x++) {
        for(y = 0; y < dh; y++) {
            for(b = 0; b < pixelSize; b++) {
                dst[dw * pixelSize * y + pixelSize * x + b] =
                    (x < sw && y < sh) ?
                        src[sw * pixelSize * y + pixelSize * x + b] : 0xff;
            }
        }
    }
}
