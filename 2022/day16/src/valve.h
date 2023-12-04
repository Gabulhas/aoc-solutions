#ifndef VALVE_H
#define VALVE_H

#include "stdbool.h"

typedef struct Valve {
  char *name;
  int flowRate;
  int numConnected;
  char **connectedValves;
  struct Valve **connectedPointers;
  bool open;
} Valve;

void freeValve(void *valveData);
void *copyValveData(void *valveData);
Valve lineToValve(char *line);
char *valveToString(Valve *v);

#endif
