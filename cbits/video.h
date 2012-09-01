#ifndef VIDEO_H
#define VIDEO_H

void setMode(int width, int height);

int screenWidth(void);
int screenHeight(void);

void startFrame(void);
void endFrame(void);

void startLine(void);
void endLine(void);

void vertex(double x, double y);

#endif
