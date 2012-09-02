#ifndef VIDEO_IMAGE_H
#define VIDEO_IMAGE_H

#include <GL/gl.h>

typedef struct {
    int width;
    int height;
    int pixelSize;

    GLuint texture;
} ff_image;

ff_image *ff_imageCreate();
void ff_imageFree(ff_image *image);

#endif
