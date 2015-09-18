/* Wrapper functions for Console functions that require structs to be passed on stack.
 *
 * coreyoconnor@gmail.com
 */
#define UNICODE
#include <windows.h>
#include "ConsoleStubs.h"

BOOL ConsoleStubs_SetConsoleCursorPosition(HANDLE hConsoleOutput,
                                           SHORT wCursorPositionX,
                                           SHORT wCursorPositionY) {
  COORD cursorPosition;
  cursorPosition.X = wCursorPositionX;
  cursorPosition.Y = wCursorPositionY;
  return SetConsoleCursorPosition(hConsoleOutput, cursorPosition);
}
