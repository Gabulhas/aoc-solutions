#include "assert.h"
#include "hashtable.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "valve.h"
#include <ctype.h>
#include <limits.h>
#define MAX_PATH_LEN 200 // This is an example; adjust based on your needs

typedef struct {
  char *name;
  bool isOpen;
} ValveState;

char *readLine(FILE *fp) {
  char *line = NULL;
  size_t len = 0;
  ssize_t read;

  read = getline(&line, &len, fp);

  if (read == -1) {
    free(line);
    return NULL; // End of file or error
  }

  return line;
}

void *pointerCopyFunction(void *data) {
  return data; // Simply return the same pointer
}

void freePointer(void *data) { free(data); }

int readValvesFromFile(char *filePath, Valve **result) {
  FILE *file = fopen(filePath, "r");
  if (!file) {
    perror("Error opening file");
    return -1;
  }

  int valveCount = 0;
  char *line = NULL;
  *result = NULL; // Initialize the result to NULL

  while ((line = readLine(file)) != NULL) {
    Valve temp = lineToValve(line);
    valveCount++;
    *result = realloc(*result, sizeof(Valve) * valveCount);
    if (!*result) {
      perror("Error reallocating memory");
      free(line);
      fclose(file);
      return -1;
    }
    char *valveString = valveToString(&temp);
    printf("Got valve <> %s\n", valveString);
    free(valveString);
    (*result)[valveCount - 1] = temp;
    free(line); // Free the line after processing
  }

  HashTable *myTable =
      Hashtable_new(valveCount, pointerCopyFunction,
                    freePointer); // No copy or free functions needed

  for (int i = 0; i < valveCount; i++) {
    printf("<>Valve name %s\n", result[i]->name);
  }

  for (int i = 0; i < valveCount; i++) {
    Valve *temp = result[i];

    for (int n = 0; n < temp->numConnected; n++) {
      temp->connectedPointers[i] =
          Hashtable_search(myTable, temp->connectedValves[i]);
    }
  }

  Hashtable_free(myTable);
  fclose(file);
  return valveCount;
}

int findValveIndex(char **valveNames, char *targetValve, int numValves) {
  for (int i = 0; i < numValves; ++i) {
    if (strcmp(valveNames[i], targetValve) == 0) {
      return i;
    }
  }
  return -1; // Return -1 if the valve is not found
}

void searchGraph(Valve *root, int curFlow, int leftMinutes, int *bestFlow,
                 char *bestPath, char *curPath, int curPathLen) {
  if (leftMinutes <= 1) {
    if (curFlow > *bestFlow) {
      printf("Best %d: %s\n", curFlow, curPath);

      if (curFlow > 900) {
        exit(0);
      }
      *bestFlow = curFlow;
      strcpy(bestPath, curPath);
    }
    return;
  }

  for (int i = 0; i < root->numConnected; i++) {

    Valve *nextValve = root->connectedPointers[i];

    // PATH A: "Open it" path
    if (!root->open && root->flowRate > 0) {
      root->open = true;
      int prevLen = curPathLen;
      snprintf(curPath + curPathLen, MAX_PATH_LEN - curPathLen, "%s ",
               root->name);
      curPathLen = strlen(curPath);

      int newFlow = root->flowRate * (leftMinutes - 1);
      searchGraph(nextValve, curFlow + newFlow, leftMinutes - 2, bestFlow,
                  bestPath, curPath, curPathLen);

      curPath[prevLen] = '\0'; // Undo path
      curPathLen = prevLen;
      root->open = false;
    }

    // PATH B: "Don't open it" path
    int prevLen = curPathLen;
    snprintf(curPath + curPathLen, MAX_PATH_LEN - curPathLen, "%c%c ",
             tolower(root->name[0]), tolower(root->name[1]));
    curPathLen = strlen(curPath);

    searchGraph(nextValve, curFlow, leftMinutes - 1, bestFlow, bestPath,
                curPath, curPathLen);

    curPath[prevLen] = '\0'; // Undo path
    curPathLen = prevLen;
  }
}

int main(int argc, char **argv) {

  Valve *valves = NULL;

  readValvesFromFile("./input/1.txt", &valves);

  int bestFlow = 0;
  char bestPath[MAX_PATH_LEN];
  bestPath[0] = '\0'; // Initialize best path as an empty string

  // Variables to track the current path
  char curPath[MAX_PATH_LEN];
  curPath[0] = '\0';  // Initialize current path as an empty string
  int curPathLen = 0; // Start with a current path length of 0

  // Call the searchGraph function
  searchGraph(&valves[0], 0, 30, &bestFlow, bestPath, curPath, curPathLen);

  // Output the result
  printf("Maximum Flow: %d\nBest Path: %s\n", bestFlow, bestPath);

  return 0;
}
