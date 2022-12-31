#include "stdio.h"
#include "string.h"
#include <stdlib.h>
//BUCKETSIZE is the number of elements that need to be different
#define BUCKETSIZE 14

struct bucket {
   char elements[BUCKETSIZE];
};
typedef struct bucket Bucket ;


void printBucket(Bucket *b) {
    printf("[");
    for(int i = 0; i < BUCKETSIZE; i++){
        printf("%c", b->elements[i]);
    }
    printf("]\n");

}

void pushToBucket(char val, Bucket *b) {
    for (int i = 0; i < BUCKETSIZE; i++) {
        b->elements[i] = b->elements[i + 1];
    }
    b->elements[BUCKETSIZE - 1] = val;
}

int areAllDifferent(Bucket *b){
    for (int i = 0; i < BUCKETSIZE; i++) {
        for (int j = 0; j < i; j++) {
            if (b->elements[i] == b->elements[j]) {
                return 0;
            }
        }
    }
    return 1;

}

ssize_t readLine(char **line) {
  size_t len = 0;
  ssize_t lineSize = 0;
  lineSize = getline(line, &len, stdin);
  return lineSize;
}


int howManyCharacters(char **line, ssize_t lineSize){
    Bucket b;

    for (int i= 0; i< BUCKETSIZE; i++) {
        pushToBucket((*line)[i], &b);
    }

    if (areAllDifferent(&b)) {
        printBucket(&b);
        return 0;
    }

    for (int i = BUCKETSIZE; i < lineSize; i++) {
        char temp = (*line)[i];
        pushToBucket(temp, &b);
        if (areAllDifferent(&b)) {
            printBucket(&b);
            return i + 1;
        }
    }
    return -1;
}


int main(void) { 
    char *line = NULL;
    ssize_t lineSize = readLine(&line);
    int i = howManyCharacters(&line, lineSize);
    printf("Answer: %d", i);

    return 0; 
}
