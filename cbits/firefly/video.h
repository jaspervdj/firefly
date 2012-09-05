#ifndef FIREFLY_VIDEO_H
#define FIREFLY_VIDEO_H

#include "firefly/video/font.h"
#include "firefly/video/image.h"

void ff_setVideoMode(int width, int height);

int ff_getScreenWidth(void);
int ff_getScreenHeight(void);

void ff_startFrame(void);
void ff_endFrame(void);

void ff_startLine(void);
void ff_endLine(void);

void ff_vertex(double x, double y);

void ff_drawImage(ff_image *image);
void ff_drawImageCentered(ff_image *image);
void ff_drawImageDebug(ff_image *image);

void ff_drawString(ff_font *font,
        const unsigned long *string, int stringLength);
void ff_drawStringCentered(ff_font *font,
        const unsigned long *string, int stringLength);

void ff_pushMatrix();
void ff_popMatrix();
void ff_translate(double x, double y);
void ff_rotate(double r);
void ff_scale(double x, double y);

void ff_setColor(double r, double g, double b, double a);
void ff_getColor(double *rgba);

#endif
