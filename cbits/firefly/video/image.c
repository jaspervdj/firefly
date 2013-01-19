#include <stdio.h>
#include <stdlib.h>
#include <png.h>

#include "firefly/video/image.h"

ff_image *ff_imageCreate(int width, int height, int bpp) {
    ff_image *image;

#ifdef DEBUG
    printf("video/image/ff_imageCreate(%d, %d, %d)\n", width, height, bpp);
#endif

    image = malloc(sizeof(ff_image));
    image->width = width;
    image->height = height;
    image->bpp = bpp;
    image->pixels = malloc(width * height * bpp * sizeof(unsigned char));

    return image;
}

#define PNG_BYTES_TO_CHECK 4

ff_image *ff_imageFromPng(const char *filePath) {
    FILE *file;
    png_byte buffer[PNG_BYTES_TO_CHECK];
    png_structp pngp;
    png_infop infop;
    png_uint_32 width, height;
    int bitDepth, colorType, interlaceType;
    png_byte **rowp;
    int x, y, b;
    int bpp;
    ff_image *image;

#ifdef DEBUG
    printf("video/image/ff_imageFromPng(\"%s\")\n", filePath);
#endif

    /* Open PNG file */
    file = fopen(filePath, "rb");
    if(!file) {
        fprintf(stderr, "video/image/ff_imageFromPng: Could not open \"%s\"\n",
                filePath);
        return 0;
    }

    /* Check signature */
    b = fread(buffer, 1, PNG_BYTES_TO_CHECK, file);
    if(b != PNG_BYTES_TO_CHECK ||
            png_sig_cmp(buffer, (png_size_t) 0, PNG_BYTES_TO_CHECK)) {
        fprintf(stderr,
                "video/image/ff_imageFromPng: PNG signature in \"%s\"\n",
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

    bpp = png_get_rowbytes(pngp, infop) / width;
    image = ff_imageCreate(width, height, bpp);

    /* Copy data to pixels array */
    rowp = png_get_rows(pngp, infop);
    for(y = 0; y < height; y++) {
        for(x = 0; x < width; x++) {
            for(b = 0; b < bpp; b++) {
                image->pixels[width * bpp * y + x * bpp + b] =
                        rowp[y][x * bpp + b];
            }
        }
    }

    png_destroy_read_struct(&pngp, &infop, 0);
    fclose(file);

    return image;
}

void ff_imageFree(ff_image *image) {
#ifdef DEBUG
    printf("video/image/ff_imageFree(_)\n");
#endif

    free(image->pixels);
    free(image);
}

int ff_imageWidth(ff_image *image) {
    return image->width;
}

int ff_imageHeight(ff_image *image) {
    return image->height;
}

int ff_imageBpp(ff_image *image) {
    return image->bpp;
}

unsigned char *ff_imagePixels(ff_image *image) {
    return image->pixels;
}

ff_image *ff_imageSlice(ff_image *image, int x, int y, int w, int h) {
    ff_image *slice;
    int len;
    int offset;
    int j;

#ifdef DEBUG
    printf("video/image/ff_imageSlice(_, %d, %d, %d, %d)\n", x, y, w, h);
#endif

    len = w * image->bpp;
    slice = ff_imageCreate(w, h, image->bpp);

    for(j = 0; j < h; j++) {
        offset = image->width * image->bpp * (y + j) + image->bpp * x;
        memcpy(slice->pixels + len * j, image->pixels + offset, len);
    }

    return slice;
}
