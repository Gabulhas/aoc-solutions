#include "hashtable.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned int hash(HashTable *table, char *key) {
  unsigned int hash = 0;
  while (*key) {
    hash = hash * 31 + *key++;
  }
  return hash % table->size;
}

void Hashtable_insert(HashTable *table, char *key, void *data) {
  unsigned int index = hash(table, key);
  Node *newNode = malloc(sizeof(Node));
  newNode->key = strdup(key);
  newNode->data = table->copyFunc(data);
  newNode->next = table->buckets[index];
  table->buckets[index] = newNode;
}

void *Hashtable_search(HashTable *table, char *key) {
  unsigned int index = hash(table, key);
  Node *node = table->buckets[index];
  while (node) {
    if (node->key == NULL) {
      return NULL;
    }
    if (strcmp(node->key, key) == 0) {
      return node->data;
    }
    node = node->next;
  }
  return NULL;
}

int containsKey(HashTable *table, char *key) {
  if (Hashtable_search(table, key) != NULL) {
    return 1;
  } else {
    return 0;
  }
}

void Hashtable_free(HashTable *table) {
  for (int i = 0; i < table->size; ++i) {
    Node *node = table->buckets[i];
    while (node) {
      Node *temp = node;
      node = node->next;
      table->freeFunc(temp->data);
      free(temp->key);
      free(temp);
    }
  }
  free(table->buckets);
}

HashTable *Hashtable_new(int size, CopyDataFunc copyData,
                         FreeDataFunc freeData) {

  HashTable *temp = malloc(sizeof(HashTable));
  temp->buckets = malloc(sizeof(Node *) * size);
  temp->size = size;
  temp->copyFunc = copyData;
  temp->freeFunc = freeData;
  return temp;
}
