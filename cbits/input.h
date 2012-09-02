#ifndef INPUT_H
#define INPUT_H

void ff_flushInput(void);

int ff_receivedQuit(void);
int ff_keyDown(int key);
int ff_mouseX(void);
int ff_mouseY(void);

#endif
