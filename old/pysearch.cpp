#include <cstdio>
#include <numeric>
#include <unordered_map>
#include <valarray>
#include <vector>

using Vec = std::valarray<int>;
const int int_min = std::numeric_limits<int>::min();
struct Input { const char *name; Vec vec; };

// ---- start of parameters ---

static const Input inputs[] = {
                  {"n", {100, 100, 100, 100,     53,  53,  53,  53}},
                  {"x", {100,  98,   2,  99,     53,   1,  20,   4}}, };
static const Vec goal = { 98,  96,  99,  97,     51,  52,  18,   2};

const int max_length = 14;

// static const int literals[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99 };
// static const int literals[] = { 1, 2, 3, 4, 510, 511, 512, 513, 514 };
static const int literals[] = {1,2,3,4,5,6,7,8,9,10,11,12};

const bool Use_Or = true;
const bool Use_Lt = true;
const bool Use_Leq = true;
const bool Use_BitOr = true;
const bool Use_BitXor = true;
const bool Use_BitAnd = true;
const bool Use_BitShl = true;
const bool Use_BitShr = true;
const bool Use_BitNeg = true;
const bool Use_Add = true;
const bool Use_Sub = true;
const bool Use_Mul = true;
const bool Use_Mod = true;
const bool Use_Div1 = false;  /* / */
const bool Use_Div2 = true;  /* // */
const bool Use_Gcd = false;
const bool Use_Neg = true;
const bool Use_Exp = true;

const bool CStyleMod = false;
const bool ReuseVars = true;

// ---- end of parameters ---

int ipow(int b, int e) {
  int r = 1;
  while (e) { if (e&1) r*=b; e>>=1; b*=b; }
  return r;
}

Vec ipow(Vec b, Vec e) {
  Vec r = b;
  for (auto i=0; i<b.size(); i++) r[i] = ipow(b[i], e[i]);
  return r;
}

Vec gcd(Vec a, Vec b) {
  Vec r = b;
  for (auto i=0; i<b.size(); i++) r[i] = std::gcd(a[i], b[i]);
  return r;
}

enum class Operator : int32_t {
  Or = 0x300,
  SpaceOr = 0x301,
  OrSpace = 0x302,
  SpaceOrSpace = 0x303,
  Lt = 0x500,
  Leq = 0x501,
  Gt = 0x502,
  Geq = 0x503,
  Eq = 0x504,
  Neq = 0x505,
  BitOr = 0x600,
  BitXor = 0x700,
  BitAnd = 0x800,
  BitShl = 0x900,
  BitShr = 0x901,
  Add = 0xA00,
  Sub = 0xA01,
  Mul = 0xB00,
  Mod = 0xB01,
  Div1 = 0xB02,
  Div2 = 0xB03,
  Gcd = 0xB04, // doesn't exist in python
  Neg = 0xC00,
  BitNeg = 0xC01,
  Exp = 0xD00,
  Parens = 0xFE00,
  Literal = 0xFF00,
};

struct Expr {
  const Expr *left;
  const Expr *right;
  Operator op;
  int32_t literal;
  int32_t var_mask;
  int32_t prec() const { return static_cast<int32_t>(op) >> 8; }
};

void print_operator(Operator op) {
  switch (op) {
    case Operator::Or: printf("or"); break;
    case Operator::SpaceOr: printf(" or"); break;
    case Operator::OrSpace: printf("or "); break;
    case Operator::SpaceOrSpace: printf(" or "); break;
    case Operator::Lt: printf("<"); break;
    case Operator::Leq: printf("<="); break;
    case Operator::Gt: printf(">"); break;
    case Operator::Geq: printf(">="); break;
    case Operator::Eq: printf("=="); break;
    case Operator::Neq: printf("!="); break;
    case Operator::BitOr: printf("|"); break;
    case Operator::BitXor: printf("^"); break;
    case Operator::BitAnd: printf("&"); break;
    case Operator::BitShl: printf("<<"); break;
    case Operator::BitShr: printf(">>"); break;
    case Operator::Add: printf("+"); break;
    case Operator::Sub: printf("-"); break;
    case Operator::Mul: printf("*"); break;
    case Operator::Mod: putchar('%'); break;
    case Operator::Div1: printf("/"); break;
    case Operator::Div2: printf("//"); break;
    case Operator::Gcd: printf("∨"); break;
    case Operator::Neg: printf("-"); break;
    case Operator::BitNeg: printf("~"); break;
    case Operator::Exp: printf("**"); break;
    case Operator::Parens: printf("("); break;
    default: break;
  }
}

void print_expression(const Expr *expr) {
  if (expr->left != nullptr) {
    print_expression(expr->left);
  }
  print_operator(expr->op);
  if (expr->right != nullptr) {
    print_expression(expr->right);
    if (expr->op == Operator::Parens) {
      putchar(')');
    }
  } else if (expr->literal < 0) {
    printf("%s", inputs[~expr->literal].name);
  } else {
    printf("%d", expr->literal);
  }
}

struct VecHasher {
  std::size_t operator()(const Vec &v) const {
    std::size_t h = 0;
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

// cache[length][output] = highest-prec expression of that length yielding that output
using CacheLevel = std::unordered_map<Vec, Expr, VecHasher, VecEqual>;
using Cache = std::unordered_map<int, CacheLevel>;
static Cache cache = Cache();

// "3or" and ")or" are valid, but "nor" isn't.
bool ok_before_keyword(const Expr *e) {
  if (e->right == nullptr) {
    return e->literal >= 0;
  } else {
    return e->op == Operator::Parens || ok_before_keyword(e->right);
  }
}

// "or3", "orn" are invalid. Need a unary op or parens.
bool ok_after_keyword(const Expr *e) {
  if (e->left == nullptr) {
    return e->op != Operator::Literal;
  } else {
    return ok_after_keyword(e->left);
  }
}

int positive_integer_length(int k) {
  int l = 1;
  while (k >= 10) k /= 10, l++;
  return l;
}

void cache_if_better(CacheLevel& level, const Vec& output, const Expr& expr) {
  const int32_t all_mask = (1 << (sizeof(inputs) / sizeof(inputs[0]))) - 1;
  if (!ReuseVars && expr.var_mask == all_mask) {
    std::unordered_map<int, int> mp;
    for (int i = 0; i < output.size(); i++) {
      const auto [it, emplaced] = mp.try_emplace(output[i], goal[i]);
      if (!emplaced && it->second != goal[i]) {
        return;
      }
    }
  }

  // if it doesn't exist, "old" is a default-constructed Expr with prec == 0 so it all works out.
  auto old = level[output];
  if (expr.prec() > old.prec()) {
    level[output] = expr;
  }
}

void find_expressions(int n) {
  auto &cn = cache[n]; Vec z;
  if (n == 1) {
    for (int i = 0; i < sizeof(inputs) / sizeof(inputs[0]); i++) {
      cn[inputs[i].vec] = Expr{nullptr, nullptr, Operator::Literal, ~i, 1 << i};
    }
  }
  for (const auto l : literals) {
    if (positive_integer_length(l) == n)
      cn[0 * goal + l] = Expr{nullptr, nullptr, Operator::Literal, l};
  }

  for (int nR = 1; nR < n; nR++) {
    for (const auto &[oR, eR] : cache[nR]) {
      // 1-byte operators
      for (const auto &[oL, eL] : cache[n - nR - 1]) {
        if (!ReuseVars && (eL.var_mask & eR.var_mask)) continue;
        const auto mask = eL.var_mask | eR.var_mask;
        if (Use_Lt && eL.prec() >= 5 && eR.prec() > 5) {
          z = 0*oL; z[oL < oR] = 1;
          cache_if_better(cn, z, Expr{&eL, &eR, Operator::Lt, 0, mask});
        }
        if (Use_BitOr && eL.prec() >= 6 && eR.prec() > 6) {
          cache_if_better(cn, oL | oR, Expr{&eL, &eR, Operator::BitOr, 0, mask});
        }
        if (Use_BitXor && eL.prec() >= 7 && eR.prec() > 7) {
          cache_if_better(cn, oL ^ oR, Expr{&eL, &eR, Operator::BitXor, 0, mask});
        }
        if (Use_BitAnd && eL.prec() >= 8 && eR.prec() > 8) {
          cache_if_better(cn, oL & oR, Expr{&eL, &eR, Operator::BitAnd, 0, mask});
        }
        if (eL.prec() >= 10 && eR.prec() > 10) {
          if (Use_Add) cache_if_better(cn, oL + oR, Expr{&eL, &eR, Operator::Add, 0, mask});
          if (Use_Sub) cache_if_better(cn, oL - oR, Expr{&eL, &eR, Operator::Sub, 0, mask});
        }
        if (eL.prec() >= 11 && eR.prec() > 11) {
          if (Use_Mul) cache_if_better(cn, oL * oR, Expr{&eL, &eR, Operator::Mul, 0, mask});
          if ((oR != 0 && (oL != int_min || oR != -1)).min()) {
            if (CStyleMod) {
              if (Use_Mod) cache_if_better(cn, oL % oR, Expr{&eL, &eR, Operator::Mod, 0, mask});
              if (Use_Div1) cache_if_better(cn, oL / oR, Expr{&eL, &eR, Operator::Div1, 0, mask});
            } else {
              auto mod = ((oL % oR) + oR) % oR;
              if (Use_Mod) cache_if_better(cn, mod, Expr{&eL, &eR, Operator::Mod, 0, mask});
              if (Use_Div1) cache_if_better(cn, (oL - mod) / oR, Expr{&eL, &eR, Operator::Div1, 0, mask});
            }
          }
          if (Use_Gcd) cache_if_better(cn, gcd(oL, oR), Expr{&eL, &eR, Operator::Gcd, 0, mask});
        }
      }
      // 2-byte operators
      for (const auto &[oL, eL] : cache[n - nR - 2]) {
        if (!ReuseVars && (eL.var_mask & eR.var_mask)) continue;
        const auto mask = eL.var_mask | eR.var_mask;
        if (eL.prec() >= 3 && eR.prec() > 3) {
          z = 0*oL; z[oL == 0] = 1;
          if (Use_Or && ok_before_keyword(&eL) && ok_after_keyword(&eR))
            cache_if_better(cn, oL + oR * z, Expr{&eL, &eR, Operator::Or, 0, mask});
        }
        if (Use_Leq && eL.prec() >= 5 && eR.prec() > 5) {
          z = 0*oL; z[oL <= oR] = 1;
          cache_if_better(cn, z, Expr{&eL, &eR, Operator::Leq, 0, mask});
        }
        if (eL.prec() >= 9 && eR.prec() > 9 && (oR >= 0).min() && (oR <= 31).min()) {
          if (Use_BitShl) cache_if_better(cn, oL << oR, Expr{&eL, &eR, Operator::BitShl, 0, mask});
          if (Use_BitShr) cache_if_better(cn, oL >> oR, Expr{&eL, &eR, Operator::BitShr, 0, mask});
        }
        if (eL.prec() >= 11 && eR.prec() > 11) {
          if ((oR != 0 && (oL != int_min || oR != -1)).min()) {
            if (CStyleMod) {
              if (Use_Div2) cache_if_better(cn, oL / oR, Expr{&eL, &eR, Operator::Div2, 0, mask});
            } else {
              auto mod = ((oL % oR) + oR) % oR;
              if (Use_Div2) cache_if_better(cn, (oL - mod) / oR, Expr{&eL, &eR, Operator::Div2, 0, mask});
            }
          }
        }
        if (eL.prec() > 13 && eR.prec() >= 13 && (oR >= 0).min() && (oR <= 6).min()) {
          if (Use_Exp) cache_if_better(cn, ipow(oL, oR), Expr{&eL, &eR, Operator::Exp, 0, mask});
        }
      }

      // 3-byte operators
      for (const auto &[oL, eL] : cache[n - nR - 3]) {
        if (!ReuseVars && (eL.var_mask & eR.var_mask)) continue;
        const auto mask = eL.var_mask | eR.var_mask;
        if (eL.prec() >= 3 && eR.prec() > 3) {
          z = 0*oL, z[oL == 0] = 1;
          if (Use_Or && !ok_before_keyword(&eL) && ok_after_keyword(&eR))
            cache_if_better(cn, oL + oR * z, Expr{&eL, &eR, Operator::SpaceOr, 0, mask});
          if (Use_Or && ok_before_keyword(&eL) && !ok_after_keyword(&eR))
            cache_if_better(cn, oL + oR * z, Expr{&eL, &eR, Operator::OrSpace, 0, mask});
        }
      }
    }
  }
  for (const auto &[oR, eR] : cache[n - 2]) {
    if (eR.op >= Operator::Parens)
      continue;
    cn[oR] = Expr{nullptr, &eR, Operator::Parens, 0, eR.var_mask};
  }
  for (const auto &[oR, eR] : cache[n - 1]) {
    if (eR.prec() >= 12) {
      if (Use_BitNeg) cache_if_better(cn, ~oR, Expr{nullptr, &eR, Operator::BitNeg, 0, eR.var_mask});
      if (Use_Neg) cache_if_better(cn, -oR, Expr{nullptr, &eR, Operator::Neg, 0, eR.var_mask});
    }
  }
}

int main() {
  printf("sizeof(Expr) = %zu\n", sizeof(Expr));
  bool no_results = true;
  for (int n = 1; n <= max_length; n++) {
    printf("Finding length %d...\n", n);
    find_expressions(n);
    printf("Found %zu expressions.\n", cache[n].size());
    bool first = true;
    for (const auto &[oR, eR] : cache[n]) {
      if (( (oR) != goal ).max())
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
  if (no_results) puts("\nNo results found.");
  puts("");
  return 0;
}
