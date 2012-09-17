#ifndef FIREFLY_INPUT_H
#define FIREFLY_INPUT_H

void ff_setGrabInput(int grabInput);
int ff_isGrabInput(void);

void ff_flushInput(void);

int ff_hasReceivedQuit(void);

int ff_isKeyDown(int key);
int ff_isKeyPressed(int key);
int ff_isKeyReleased(int key);

int ff_isMouseButtonDown(int mouseButton);
int ff_isMouseButtonPressed(int mouseButton);
int ff_isMouseButtonReleased(int mouseButton);
int ff_getMouseX(void);
int ff_getMouseY(void);

#endif
