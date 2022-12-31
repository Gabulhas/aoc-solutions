#include <iostream>
#include <string>
#include <vector>
using namespace std;

typedef vector<vector<int>> grid;

// y is row
// x is col

bool isVisibleUp(grid g, int x, int y) {
  int gridHeight = g.size();
  int gridLength = g.front().size();
  int treeValue = g[y][x];
  for (int i = y - 1; i >= 0; i--) {
    if (g[i][x] >= treeValue) {
      return false;
    }
  }
  return true;

}
bool isVisibleDown(grid g, int x, int y) {
  int gridHeight = g.size();
  int gridLength = g.front().size();
  int treeValue = g[y][x];
  for (int i = y + 1 ; i < gridHeight; i++) {
    if (g[i][x] >= treeValue) {
      return false;
    }
  }
  return true;

}
bool isVisibleLeft(grid g, int x, int y) {
  int gridHeight = g.size();
  int gridLength = g.front().size();
  int treeValue = g[y][x];
  for (int i = x - 1; i >= 0; i--) {
    if (g[y][i] >= treeValue) {
      return false;
    }
  }
  return true;

}
bool isVisibleRight(grid g, int x, int y) {
  int gridHeight = g.size();
  int gridLength = g.front().size();
  int treeValue = g[y][x];
  for (int i = x + 1; i < gridLength; i++) {
    if (g[y][i] >= treeValue) {
      return false;
    }
  }
  return true;
}

bool isVisible(grid g, int x, int y) {
  return isVisibleUp(g, x, y) || isVisibleDown(g, x, y) ||
         isVisibleLeft(g, x, y) || isVisibleRight(g, x, y);
}

int checkInnerTrees(grid g) {
  int gridHeight = g.size();
  int gridLength = g.front().size();
  int total = 0;

  // iterate by row
  for (int row = 1; row < gridHeight - 1; row++) {
    for (int col = 1; col < gridLength - 1; col++) {
      if (isVisible(g, col, row)) {
        total += 1;
      }
    }
  }

  return total;
}

int getTotalVisibleTrees(grid g) {
  int gridHeight = g.size();
  int gridLength = g.front().size();
  int topAndBot = g.size() * 2;
  int leftAndRight = g.front().size() * 2 - 4;
  int innner = checkInnerTrees(g);
  return topAndBot + leftAndRight + innner;
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

int main() { cout << getTotalVisibleTrees(readGrid()) << endl; }
