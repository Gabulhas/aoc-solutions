#include "valve.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

void freeValve(void *valveData) {
  Valve *valve = (Valve *)valveData;
  free(valve->name);
  for (int i = 0; i < valve->numConnected; i++) {
    free(valve->connectedValves[i]);
  }
  free(valve->connectedValves);
}

void *copyValveData(void *valveData) {
  if (valveData == NULL) {
    exit(-1);
    return NULL;
  }

  Valve *original = (Valve *)valveData;
  Valve *copy = malloc(sizeof(Valve));
  if (copy == NULL || original == NULL) {
    // Handle allocation error
    return NULL;
  }

  // Copying the name
  copy->name = original->name ? strdup(original->name) : NULL;
  if (original->name && copy->name == NULL) {
    // Handle allocation error
    free(copy);
    return NULL;
  }

  // Copying flowRate
  copy->flowRate = original->flowRate;
  copy->numConnected = original->numConnected;

  // Allocating and copying connectedValves
  if (original->numConnected > 0) {
    copy->connectedValves = malloc(sizeof(char *) * original->numConnected);
    copy->connectedPointers = malloc(sizeof(Valve *) * original->numConnected);

    if (copy->connectedValves == NULL || copy->connectedPointers == NULL) {
      // Handle allocation error
      free(copy->connectedValves);
      free(copy->connectedPointers);
      free(copy->name);
      free(copy);
      return NULL;
    }

    for (int i = 0; i < original->numConnected; ++i) {
      if (original->connectedValves[i] != NULL) {

        copy->connectedValves[i] = strdup(original->connectedValves[i]);
        if (copy->connectedValves[i] == NULL) {
          // Handle allocation error
          for (int j = 0; j < i; ++j) {
            free(copy->connectedValves[j]);
          }
          free(copy->connectedValves);
          free(copy->connectedPointers);
          free(copy->name);
          free(copy);
          return NULL;
        }
      } else {
        copy->connectedValves[i] = NULL;
      }
    }
  } else {
    copy->connectedValves = NULL;
    copy->connectedPointers = NULL;
  }

  return copy;
}

Valve lineToValve(char *line) {
  Valve v;
  char *token;

  // Extract Valve name
  token = strtok(line, " ;\n");
  token = strtok(NULL, " ;\n"); // Skip "Valve"
  v.name = malloc(strlen(token) + 1);
  strcpy(v.name, token);

  // Extract and set flow rate
  token = strtok(NULL, ";");
  token = strstr(token, "="); // Find the '=' in "flow rate=x"
  if (token) {
    v.flowRate = atoi(token + 1); // Skip '=' and convert to integer
  } else {
    v.flowRate = 0; // Default flow rate if not found
  }

  // Initialize connected valves
  v.numConnected = 0;
  v.connectedValves = NULL;

  // Extract connected valves
  token = strtok(NULL, "\n"); // Get the rest of the line
  if (token) {
    char *valveStart = strstr(token, "valves");
    if (!valveStart) {
      valveStart = strstr(token, "valve"); // For "tunnel leads to valve"
    }
    if (valveStart) {
      token = strtok(valveStart, " ,\n");
      while ((token = strtok(NULL, " ,\n")) != NULL) {
        v.connectedValves =
            realloc(v.connectedValves, sizeof(char *) * (v.numConnected + 1));
        v.connectedValves[v.numConnected] = malloc(strlen(token) + 1);
        strcpy(v.connectedValves[v.numConnected], token);
        v.numConnected++;
      }
    }
  }

  v.connectedPointers = malloc(sizeof(Valve *) * v.numConnected);

  return v;
}

char *valveToString(Valve *v) {
  if (v == NULL || v->name == NULL) {
    return strdup("NULL Valve");
  }

  // Estimate the size needed for the string
  int size =
      snprintf(NULL, 0, "Valve: %s, Flow Rate: %d, Connected Valves: ", v->name,
               v->flowRate);
  for (int i = 0; i < v->numConnected; ++i) {
    size += snprintf(NULL, 0, "%s, ", v->connectedValves[i]);
  }
  size++; // For null terminator

  // Allocate the string
  char *str = malloc(size);
  if (str == NULL) {
    return NULL; // Allocation failure
  }

  // Construct the string
  int offset =
      sprintf(str, "Valve: %s, Flow Rate: %d, Connected Valves: ", v->name,
              v->flowRate);
  for (int i = 0; i < v->numConnected; ++i) {
    offset += sprintf(str + offset, "%s%s", v->connectedValves[i],
                      (i < v->numConnected - 1) ? ", " : "");
  }

  return str;
}
