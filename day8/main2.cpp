#include <iostream>
#include <string>
#include <vector>
using namespace std;

typedef vector<vector<int>> grid;

// y is row
// x is col

int valueUp(grid g, int x, int y) {
  int gridHeight = g.size();
  int gridLength = g.front().size();
  int treeValue = g[y][x];
  int count = 0;
  for (int i = y - 1; i >= 0; i--) {
    count++;
    if (g[i][x] >= treeValue) {
      break;
    }
  }
  return count;
}
int valueDown(grid g, int x, int y) {
  int gridHeight = g.size();
  int gridLength = g.front().size();
  int treeValue = g[y][x];
  int count = 0;
  for (int i = y + 1; i < gridHeight; i++) {
    count++;
    if (g[i][x] >= treeValue) {
        break;
    }
  }
  return count;
}
int valueLeft(grid g, int x, int y) {
  int gridHeight = g.size();
  int gridLength = g.front().size();
  int treeValue = g[y][x];
  int count = 0;
  for (int i = x - 1; i >= 0; i--) {
    count++;
    if (g[y][i] >= treeValue) {
        break;
    }
  }
  return count;
}
int valueRight(grid g, int x, int y) {
  int gridHeight = g.size();
  int gridLength = g.front().size();
  int treeValue = g[y][x];
  int count = 0;
  for (int i = x + 1; i < gridLength; i++) {
    count++;
    if (g[y][i] >= treeValue) {
        break;
    }
  }
  return count;
}

int scenicScore(grid g, int x, int y) {
    return valueUp(g, x, y) * valueDown(g, x, y) * valueLeft(g, x, y) * valueRight(g, x, y);
}

int highestScenicTree(grid g) {
  int gridHeight = g.size();
  int gridLength = g.front().size();
  int currentMaximum = 0 ;

  // iterate by row
  for (int row = 0; row < gridHeight; row++) {
    for (int col = 0; col < gridLength; col++) {
      int valueOfTree = scenicScore(g, row, col);
      if (valueOfTree > currentMaximum) {
          currentMaximum = valueOfTree;
      }
    }
  }

  return currentMaximum;
}


void showGrid(grid g) {
  for (vector<int> row : g) {
    for (int i : row) {
      cout << i;
    }
    cout << endl;
  }
}

grid readGrid() {
  grid g;
  for (string line; getline(cin, line);) {
    vector<int> temp;
    for (char c : line) {
      int x = (int)c - 48;
      temp.push_back(x);
    }
    g.push_back(temp);
  }
  showGrid(g);
  return g;
}

int main() { cout << (highestScenicTree(readGrid())) << endl; }
