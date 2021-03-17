#include <cstdio>
#include <unordered_map>
#include <valarray>
#include <vector>

using Vec = std::valarray<int>;
struct Input { const char *name; Vec vec; };

// ---- start of parameters ---

// http://golf.shinh.org/p.rb?reversed+even+or+odd+first
static const Input inputs[] = {
                  {"n", {100, 100, 100, 100,     53,  53,  53,  53}},
                  {"x", {100,  98,   2,  99,     53,   1,  20,   4}}, };
static const Vec goal = { 98,  96,  99,  97,     51,  52,  18,   2};

const int max_length = 11;

static const int literals[] = {1, 2}; // 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,};

// ---- end of parameters ---

static const int p_literal = 999;
static const int p_parens = 998;

struct Expr {
  const Expr *left;
  const Expr *right;
  const char *op_glyph;
  int literal;
};

void print_expression(const Expr *expr) {
  if (expr->left != nullptr) {
    print_expression(expr->left);
  }
  if (expr->op_glyph != nullptr) {
    printf("%s", expr->op_glyph);
  }
  if (expr->right != nullptr) {
    print_expression(expr->right);
    if (expr->op_glyph != nullptr && expr->op_glyph[0] == '(') {
      putchar(')');
    }
  } else if (expr->literal != 0) {
    if (expr->literal < 0) {
      printf("%s", inputs[~expr->literal].name);
    } else {
      printf("%d", expr->literal);
    }
  }
}

struct VecHasher {
  std::size_t operator()(const Vec &v) const {
    int h = 0;
    for (const auto x : v)
      h ^= x + 0x9e3779b9 + (h << 6) + (x >> 2);
    return h;
  }
};

struct VecEqual {
  bool operator()(const Vec &v1, const Vec &v2) const {
    return (v1 == v2).min();
  }
};

// cache[length][prec][hash(output)] = expression of that length/prec yielding that output
using Cache = std::unordered_map<int, std::unordered_map<int, std::unordered_map<Vec, Expr, VecHasher, VecEqual>>>;
static Cache cache = Cache();

// "3or" and ")or" are valid, but "nor" isn't.
bool ok_before_keyword(const Expr *e) {
  if (e->right == nullptr) {
    return e->literal >= 0;
  } else {
    return e->op_glyph[0] == '(' || ok_before_keyword(e->right);
  }
}

// "or3", "orn" are invalid. Need a unary op or parens.
bool ok_after_keyword(const Expr *e) {
  if (e->left == nullptr) {
    return e->op_glyph != nullptr;
  } else {
    return ok_after_keyword(e->left);
  }
}

int positive_integer_length(int k) {
  int l = 1;
  while (k >= 10) k /= 10, l++;
  return l;
}

void find_expressions(int n) {
  auto &cn = cache[n];
  if (n == 1) {
    for (int i = 0; i < sizeof(inputs) / sizeof(inputs[0]); i++) {
      cn[p_literal][inputs[i].vec] = Expr{nullptr, nullptr, nullptr, ~i};
    }
  }
  for (const auto l : literals) {
    if (positive_integer_length(l) == n)
      cn[p_literal][0 * goal + l] = Expr{nullptr, nullptr, nullptr, l};
  }

  for (int nR = 1; nR < n; nR++) {
    for (const auto &[precR, mR] : cache[nR]) {
      for (const auto &[oR, eR] : mR) {
        // 1-byte operators
        for (const auto &[precL, mL] : cache[n - nR - 1]) {
          for (const auto &[oL, eL] : mL) {
            if (precL >= 5 && precR > 5) {
              cn[5][+(oL < oR)] = Expr{&eL, &eR, "<", 0};
            }
            if (precL >= 6 && precR > 6) {
              cn[6][oL | oR] = Expr{&eL, &eR, "|", 0};
            }
            if (precL >= 7 && precR > 7) {
              cn[7][oL & oR] = Expr{&eL, &eR, "^", 0};
            }
            if (precL >= 8 && precR > 8) {
              cn[8][oL & oR] = Expr{&eL, &eR, "&", 0};
            }
            if (precL >= 10 && precR > 10) {
              cn[10][oL + oR] = Expr{&eL, &eR, "+", 0};
              cn[10][oL - oR] = Expr{&eL, &eR, "-", 0};
            }
            if (precL >= 11 && precR > 11) {
              cn[11][oL * oR] = Expr{&eL, &eR, "*", 0};
              if ((oR != 0).min()) {
                auto mod = ((oL % oR) + oR) % oR;
                cn[11][mod] = Expr{&eL, &eR, "%", 0};
                cn[11][(oL - mod) / oR] = Expr{&eL, &eR, "/", 0};
              }
            }
          }
        }
        // 2-byte operators
        for (const auto &[precL, mL] : cache[n - nR - 2]) {
          for (const auto &[oL, eL] : mL) {
            if (precL >= 3 && precR > 3) {
              if (ok_before_keyword(&eL) && ok_after_keyword(&eR))
                cn[3][oL + oR * +(oL == 0)] = Expr{&eL, &eR, "or", 0};
            }
            if (precL >= 5 && precR > 5) {
              cn[5][+(oL <= oR)] = Expr{&eL, &eR, "<=", 0};
            }
            if (precL >= 9 && precR > 9 && (oR >= 0).min() &&
                (oR <= 31).min()) {
              cn[9][oL << oR] = Expr{&eL, &eR, "<<", 0};
              cn[9][oL >> oR] = Expr{&eL, &eR, ">>", 0};
            }
          }
        }

        // 3-byte operators
        for (const auto &[precL, mL] : cache[n - nR - 3]) {
          for (const auto &[oL, eL] : mL) {
            if (precL >= 3 && precR > 3) {
              if (!ok_before_keyword(&eL) && ok_after_keyword(&eR))
                cn[3][oL + oR * +(oL == 0)] = Expr{&eL, &eR, " or", 0};
              if (ok_before_keyword(&eL) && !ok_after_keyword(&eR))
                cn[3][oL + oR * +(oL == 0)] = Expr{&eL, &eR, "or ", 0};
            }
          }
        }
      }
    }
  }
  for (const auto &[precR, mR] : cache[n - 2]) {
    if (precR >= p_parens)
      continue;
    for (const auto &[oR, eR] : mR) {
      cn[p_parens][oR] = Expr{nullptr, &eR, "(", 0};
    }
  }
  for (const auto &[precR, mR] : cache[n - 1]) {
    for (const auto &[oR, eR] : mR) {
      if (precR >= 12) {
        cn[12][~oR] = Expr{nullptr, &eR, "~", 0};
        cn[12][-oR] = Expr{nullptr, &eR, "-", 0};
      }
    }
  }
}

int main() {
  for (int n = 1; n <= max_length; n++) {
    printf("Finding length %d...\n", n);
    find_expressions(n);
  }

  bool no_results = true;
  for (int n = 1; n <= max_length; n++) {
    bool first = true;
    for (const auto &[prec, m] : cache[n]) {
      for (const auto &[oR, eR] : m) {
        if ((oR != goal).max())
          continue;
        if (first) {
          printf("\n--- Length %d ---\n", n);
          first = false;
          no_results = false;
        }
        print_expression(&eR);
        puts("");
      }
    }
  }
  if (no_results) puts("\nNo results found.");
  puts("");
  return 0;
}