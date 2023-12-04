#ifndef HASHTABLE_H
#define HASHTABLE_H

typedef void *(*CopyDataFunc)(void *);
typedef void (*FreeDataFunc)(void *);

typedef struct Node {
  char *key;
  void *data;
  struct Node *next;
} Node;

typedef struct HashTable {
  int size;
  Node **buckets;
  CopyDataFunc copyFunc;
  FreeDataFunc freeFunc;
} HashTable;

void Hashtable_insert(HashTable *table, char *key, void *data);
void *Hashtable_search(HashTable *table, char *key);
int Hashtable_containsKey(HashTable *table, char *key);
void Hashtable_free(HashTable *table);
HashTable *Hashtable_new(int size, CopyDataFunc copyData,
                         FreeDataFunc freeData);

#endif
