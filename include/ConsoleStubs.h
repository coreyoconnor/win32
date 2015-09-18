#ifndef __CONSOLE_STUBS_H
#define __CONSOLE_STUBS_H

#include <windows.h>

extern BOOL ConsoleStubs_SetConsoleCursorPosition(HANDLE hConsoleOutput,
                                                  SHORT wCursorPositionX,
                                                  SHORT wCursorPositionY);

#endif /* __CONSOLE_STUBS_H */
