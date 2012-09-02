#include <stdio.h>
#include <stdlib.h>

#include "video/image.h"

ff_image *ff_imageCreate() {
#ifdef DEBUG
    printf("video/image/imageCreate()\n");
#endif
    ff_image *image = malloc(sizeof(ff_image));
    image->texture = 0;
    return image;
}

void ff_imageFree(ff_image *image) {
#ifdef DEBUG
    printf("video/image/imageFree(_)\n");
#endif
    if(image->texture) glDeleteTextures(1, &image->texture);
    free(image);
}
