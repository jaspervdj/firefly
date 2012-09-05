#ifndef FIREFLY_INPUT_H
#define FIREFLY_INPUT_H

void ff_flushInput(void);

int ff_hasReceivedQuit(void);
int ff_isKeyDown(int key);
int ff_getMouseX(void);
int ff_getMouseY(void);
int ff_isMouseButtonDown(int mouseButton);

#endif
