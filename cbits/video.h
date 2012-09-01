#ifndef VIDEO_H
#define VIDEO_H

void video_setMode(int width, int height);

int video_screenWidth(void);
int video_screenHeight(void);

void video_startFrame(void);
void video_endFrame(void);

void video_startLine(void);
void video_endLine(void);

void video_vertex(double x, double y);

#endif
