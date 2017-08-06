// X-list of multi-char punctuators.

// Note: Order maters. If one punctuator is a prefix of another, the longer punctuator must appear first.

X(!=, NOT_EQUALS)
X(||, LOGICAL_OR)
X(&&, LOGICAL_AND)
X(%=, ASSIGN_MODULUS)
X(&=, ASSIGN_BIT_ADD)
X(*=, ASSIGN_MULTIPLY)
X(++, INCREMENT)
X(+=, ASSIGN_INCREMENT)
X(--, DECREMENT)
X(-=, ASSIGN_DECREMENT)
X(->, ARROW)
X(..., ELLIPSIS)
X(/=, ASSIGN_DIVIDE)
X(<<=, ASSIGN_SHIFT_LEFT)
X(<<, SHIFT_LEFT)
X(<=, LEQ)
X(==, EQUALS)
X(>=, GEQ)
X(>>=, ASSIGN_SHIFT_RIGHT)
X(>>, SHIFT_RIGHT)
X(^=, ASSIGN_BIT_XOR)
X(|=, ASSIGN_BIT_OR)

// TODO(medium): Add the special punctuators.
// <: :> <% %> %: %:%:
