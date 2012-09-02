#ifndef FF_VIDEO_H
#define FF_VIDEO_H

#include "video/image.h"

void ff_setVideoMode(int width, int height);

int ff_screenWidth(void);
int ff_screenHeight(void);

void ff_startFrame(void);
void ff_endFrame(void);

void ff_startLine(void);
void ff_endLine(void);

void ff_vertex(double x, double y);

void ff_drawImage(ff_image *image);
void ff_drawImageCentered(ff_image *image);
void ff_drawImageDebug(ff_image *image);

void ff_translate(double x, double y);
void ff_rotate(double r);
void ff_scale(double x, double y);

#endif
