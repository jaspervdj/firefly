#ifndef VIDEO_H
#define VIDEO_H

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

#endif
