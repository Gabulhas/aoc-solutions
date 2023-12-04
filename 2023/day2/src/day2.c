#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include <limits.h>
#include <stdio.h>

char *concatStrings(char *dest, const char *src) {
  size_t newSize =
      (dest ? strlen(dest) : 0) + strlen(src) + 1; // +1 for the null terminator
  char *newStr = realloc(dest, newSize);

  if (newStr == NULL) {
    perror("Unable to allocate memory");
    free(dest); // Free the original string if reallocation failed
    return NULL;
  }

  // Concatenate the new string
  strcat(newStr, src);

  return newStr;
}

typedef enum { RED, BLUE, GREEN } DiceColor;

char *colorToString(DiceColor color) {

  switch (color) {
  case RED:
    return "red";
    break;
  case BLUE:
    return "blue";
    break;
  case GREEN:
    return "green";
    break;
  }
}

typedef struct {
  int amount;
  DiceColor color;
} Hand;

char *handToString(Hand *hand) {
  char *result;
  asprintf(&result, "%d %s", hand->amount, colorToString(hand->color));
  return result;
}

typedef struct {
  int id;
  int numberOfSets;
  int *setSizes;
  Hand ***sets;
} Game;

void freeGame(Game *game) {
  for (int set = 0; set < game->numberOfSets; set++) {
    for (int hand = 0; hand < game->setSizes[set]; hand++) {
      free(game->sets[set][hand]);
    }
    free(game->sets[set]);
  }
  free(game->sets);
  free(game->setSizes);
  free(game);
}

char *gameToString(Game *game) {
  if (game == NULL || game->numberOfSets == 0) {
    return strdup("Empty Game");
  }

  // Calculate total length first
  size_t totalLength = 10; // For "Game X: " and null terminator
  for (int i = 0; i < game->numberOfSets; i++) {
    for (int j = 0; j < game->setSizes[i]; j++) {
      char *handStr = handToString(game->sets[i][j]);
      totalLength += strlen(handStr) + 2; // For hand string and ", "
      free(handStr);
    }
    totalLength += 3; // For " | "
  }

  // Allocate memory for the whole string
  char *gameStr = malloc(totalLength);
  if (gameStr == NULL) {
    return NULL;
  }
  sprintf(gameStr, "Game %d: ", game->id);

  // Build the string
  for (int i = 0; i < game->numberOfSets; i++) {
    for (int j = 0; j < game->setSizes[i]; j++) {
      char *handStr = handToString(game->sets[i][j]);
      strcat(gameStr, handStr);
      free(handStr);
      if (j < game->setSizes[i] - 1) {
        strcat(gameStr, ", ");
      }
    }
    if (i < game->numberOfSets - 1) {
      strcat(gameStr, " ; ");
    }
  }

  return gameStr;
}

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

DiceColor colorFromString(const char *colorStr) {
  if (strcmp(colorStr, "red") == 0)
    return RED;
  if (strcmp(colorStr, "blue") == 0)
    return BLUE;
  if (strcmp(colorStr, "green") == 0)
    return GREEN;
  return -1; // Invalid color
}

Game *parseLine(char *line) {
  char *token;
  token = strtok(line, " "); // Skip the "Game" word
  token = strtok(NULL, " "); // Get the game ID
  int id = atoi(token);

  Game *game = malloc(sizeof(Game));
  game->id = id;
  game->numberOfSets = 0;
  game->setSizes = NULL;
  game->sets = NULL;

  char *restSets; // for strtok_r
  char *setsToken = strtok(NULL, ":");
  char *eachSet = strtok_r(setsToken, ";", &restSets);

  while (eachSet != NULL) {
    game->numberOfSets++;
    game->setSizes = realloc(game->setSizes, sizeof(int) * game->numberOfSets);
    game->sets = realloc(game->sets, sizeof(Hand **) * game->numberOfSets);

    int handCount = 0;
    Hand **hands = NULL;

    char *restHands; // for the inner strtok_r
    char *handToken = strtok_r(eachSet, ",", &restHands);
    while (handToken != NULL) {
      // Trim leading spaces from handToken if necessary

      handCount++;
      hands = realloc(hands, sizeof(Hand *) * handCount);

      int amount;
      char colorStr[10];
      sscanf(handToken, "%d %s", &amount,
             colorStr); // Make sure input format matches
      DiceColor color = colorFromString(colorStr);

      Hand *hand = malloc(sizeof(Hand));
      hand->amount = amount;
      hand->color = color;
      hands[handCount - 1] = hand;

      handToken = strtok_r(NULL, ",", &restHands);
    }

    game->setSizes[game->numberOfSets - 1] = handCount;
    game->sets[game->numberOfSets - 1] = hands;

    eachSet = strtok_r(NULL, ";", &restSets);
  }

  return game;
}

int readGames(char *fileName, Game ***result) {
  FILE *file = fopen(fileName, "r");
  if (file == NULL) {
    perror("Error opening file");
    return -1;
  }

  int linesRead = 0;
  *result = NULL; // Initialize the result array

  char *line = NULL;
  while ((line = readLine(file)) != NULL) {
    linesRead++;
    *result = realloc(*result, sizeof(Game *) * linesRead);
    if (*result == NULL) {
      perror("Error reallocating memory");
      return -1; // Handle memory allocation failure
    }

    Game *parsed = parseLine(line);
    (*result)[linesRead - 1] = parsed;

    free(line); // Free the line after parsing
  }

  fclose(file); // Close the file after reading
  return linesRead;
}

bool possibleNumber(DiceColor color, int amount) {
  if ((color == RED && amount > 12) || (color == GREEN && amount > 13) ||
      (color == BLUE && amount > 14)) {
    return false;
  }
  return true;
}

int part1(Game *game) {
  for (int set = 0; set < game->numberOfSets; set++) {
    for (int hand = 0; hand < game->setSizes[set]; hand++) {
      Hand *thisHand = game->sets[set][hand];
      if (!possibleNumber(thisHand->color, thisHand->amount)) {
        return 0;
      }
    }
  }
  return game->id;
}

int part2(Game *game) {

  int red = 0;
  int blue = 0;
  int green = 0;
  for (int set = 0; set < game->numberOfSets; set++) {
    for (int hand = 0; hand < game->setSizes[set]; hand++) {
      Hand *thisHand = game->sets[set][hand];
      int amount = thisHand->amount;
      switch (thisHand->color) {
      case RED:
        if (amount > red) {
          red = amount;
        }
        break;
      case BLUE:
        if (amount > blue) {
          blue = amount;
        }
        break;
      case GREEN:
        if (amount > green) {
          green = amount;
        }
        break;
      }
    }
  }

  return red * blue * green;
}

int main(int argc, char **argv) {
  char *fileName = "./input/input2.txt";

  Game **games;
  int totalGames = readGames(fileName, &games);
  for (int i = 0; i < totalGames; i++) {
    char *gameAsString = gameToString(games[i]);
    printf("> %s\n", gameAsString);
    free(gameAsString);
  }

  int totalIds = 0;
  for (int i = 0; i < totalGames; i++) {
    totalIds += part1(games[i]);
  }

  int totalPowers = 0;
  for (int i = 0; i < totalGames; i++) {
    totalPowers += part2(games[i]);
  }

  for (int i = 0; i < totalGames; i++) {
    freeGame(games[i]);
  }

  free(games);
  printf("Final Part1 %d\n", totalIds);
  printf("Final Part2 %d\n", totalPowers);
}
