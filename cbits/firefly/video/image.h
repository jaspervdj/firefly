#ifndef FIREFLY_VIDEO_IMAGE_H
#define FIREFLY_VIDEO_IMAGE_H

typedef struct {
    int width, height;
    int bpp;
    unsigned char *pixels;
} ff_image;

ff_image *ff_imageCreate(int width, int height, int bpp);
ff_image *ff_imageFromPng(const char *filePath);
void ff_imageFree(ff_image *image);

int ff_imageWidth(ff_image *image);
int ff_imageHeight(ff_image *image);
int ff_imageBpp(ff_image *image);
unsigned char *ff_imagePixels(ff_image *image);

ff_image *ff_imageSlice(ff_image *image, int x, int y, int w, int h);

#endif
