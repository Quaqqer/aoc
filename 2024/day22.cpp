/*
 * A fast solution for day 22.
 *
 * After solving it, me and some friends tried to speed up our solutions. This
 * is my friend Axel's solution, but optimized a bit further by me.
 */
#include <iostream>

inline int next_secret(int secret) {
  secret ^= secret << 6;
  secret &= 0xffffff;

  secret ^= secret >> 5;
  secret &= 0xffffff;

  secret ^= secret << 11;
  secret &= 0xffffff;

  return secret;
}

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(0);
  std::cout.tie(0);

  long answer_a{};
  int answer_b{};

  int scores[19][19][19][19]{};

  int secret;
  int seen[19][19][19][19]{};
  for (int i = 0; std::cin >> secret; i++) {
    int suffix[4]{0};

    int price;
    int prev_price = secret % 10;
    int delta;

    for (int k = 0; k < 2000; k++) {
      secret = next_secret(secret);
      price = secret % 10;
      delta = price - prev_price;
      prev_price = secret % 10;

      if (k < 4) {
        suffix[k] = delta + 9;
      } else {
        for (int i = 0; i < 3; i++) {
          suffix[i] = suffix[i + 1];
        }
        suffix[3] = delta + 9;
      }

      auto [a, b, c, d] = suffix;

      if (seen[a][b][c][d] == i + 1) {
        continue;
      }
      seen[a][b][c][d] = i + 1;

      scores[a][b][c][d] += price;
      answer_b = std::max(answer_b, scores[a][b][c][d]);
    }

    answer_a += secret;
  }

  std::cout << answer_a << '\n' << answer_b << '\n';
}
