#include <algorithm>
#include <array>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <vector>
#include <string>
#include <numeric>
#include <utility>
#include <ranges>
#include <cassert>

class Die {
public:
  virtual int roll() = 0;
};

class Deterministic : public Die {
  int Value = 0;
public:
  int roll() override {
    if (Value == 100)
      Value = 0;
    return ++Value;
  }
};

struct Player {
  int Position;
  int Score = 0;

  void makeTurn(Die &D) {
    Position = (Position - 1 + D.roll() + D.roll() + D.roll()) % 10 + 1;
    Score += Position;
  }
};

class Dirac : public Die {
  std::array<int, 3> Arr;
  int P = 0;
public:
  Dirac(int A) : Arr{A, 0, 0} {}
  int roll() override {
    return Arr[P++];
  }
};

class DiracGame {
public:
  long long P1Wins = 0, P2Wins = 0;
  void playGame(Player P1, Player P2, bool P1Turn, Dirac D, long long Spawn) {
    (P1Turn ? P1 : P2).makeTurn(D);
    if (P1.Score >= 21) {
      P1Wins += Spawn;
      return;
    }
    if (P2.Score >= 21) {
      P2Wins += Spawn;
      return;
    }
    playGame(P1, P2, !P1Turn, Dirac(3), 1 * Spawn);
    playGame(P1, P2, !P1Turn, Dirac(4), 3 * Spawn);
    playGame(P1, P2, !P1Turn, Dirac(5), 6 * Spawn);
    playGame(P1, P2, !P1Turn, Dirac(6), 7 * Spawn);
    playGame(P1, P2, !P1Turn, Dirac(7), 6 * Spawn);
    playGame(P1, P2, !P1Turn, Dirac(8), 3 * Spawn);
    playGame(P1, P2, !P1Turn, Dirac(9), 1 * Spawn);
  }
};

int main(int argc, const char *argv[]) {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  std::fstream File;
  if (argc > 1)
    File.open(argv[1]);
  std::istream &Input = argc == 1 ? std::cin : File;

  Player P1, P2;
  P1.Position = std::invoke([&Input] {
    int V;
    std::string S;
    std::getline(Input, S);
    std::sscanf(S.c_str(), "Player 1 starting position: %d", &V);
    return V;
  });
  P2.Position = std::invoke([&Input] {
    int V;
    std::string S;
    std::getline(Input, S);
    std::sscanf(S.c_str(), "Player 2 starting position: %d", &V);
    return V;
  });

  Player P1C = P1, P2C = P2;

  Deterministic Die;
  Player *CP = &P1C;
  int RC = 0;
  while (P1C.Score < 1000 && P2C.Score < 1000) {
    CP->makeTurn(Die);
    CP = CP == &P1C ? &P2C : &P1C;
    RC += 3;
  }
  std::cout << "Part 1 = " << (std::min(P1C.Score, P2C.Score) * RC) << std::endl;

  DiracGame DG;
  DG.playGame(P1, P2, true, Dirac(3), 1);
  DG.playGame(P1, P2, true, Dirac(4), 3);
  DG.playGame(P1, P2, true, Dirac(5), 6);
  DG.playGame(P1, P2, true, Dirac(6), 7);
  DG.playGame(P1, P2, true, Dirac(7), 6);
  DG.playGame(P1, P2, true, Dirac(8), 3);
  DG.playGame(P1, P2, true, Dirac(9), 1);

  std::cout << "Part 2 = " << std::max(DG.P1Wins, DG.P2Wins) << std::endl;

  return 0;
}