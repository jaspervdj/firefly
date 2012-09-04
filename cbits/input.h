#ifndef FF_INPUT_H
#define FF_INPUT_H

void ff_flushInput(void);

int ff_receivedQuit(void);
int ff_keyDown(int key);
int ff_mouseX(void);
int ff_mouseY(void);
int ff_mouseButtonDown(int mouseButton);

#endif
