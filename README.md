## spire-sym

This library is toy class for symbolic computation.

For an overview of what this is see the
[Wikipedia page](http://en.wikipedia.org/wiki/Symbolic_computation).

### overview

The library provides `Sym`, a type that can be used for symbolic
computation. The following capabilities are available:

 * addition (`Sym(3) + Sym(4)`) and multiplication (`Sym(3) * Sym(4)`)
 * negation (`-Sym(3)`) and reciprocals (`Sym(3).reciprocal`)
 * subtraction (`Sym(3) - Sym(4)`) and division (`Sym(3) / Sym(4)`)
 * exponentiation (`Sym(3) ** Sym(2)`) and roots (`Sym(3) nroot Sym(2)`)
 * constants (`Sym(Rational(3, 4))`) and symbolic names (e.g. `Sym("x")`)
 * variable substitution (`(...).subst("x" -> Sym(1))`)
 * partial ordering (`Sym(3) partialCompare Sym(4)`)
 * partial sign tests (`Sym(3).signum`)
 * partial evaluation (`(...).evaluate`)
 
### examples

Here's an example demonstrating how roots are lazily calculated:

```scala
import spire.sym._

val a = Sym(2) * Sym(3).sqrt
val b = a * Sym(5).nroot(3) * Sym(7).sqrt
val c = b * Sym(11).nroot(6)
val d = c ** Sym(6)

// d is 162993600
```

Here's an example that shows how partial comparisons work:

```scala
import spire.sym._

val x = Sym("a") + Sym(9)
val y = Sym("a") + Sym(8)
val z = Sym("a") * Sym(2)

x partialCompare x // 0.0, equal
x partialCompare y // 1.0, x is greater
x partialCompare z // NaN, unknown

val m1 = Map("a" -> Sym(4))
val m2 = Map("a" -> Sym(9))

x.subst(m1) partialCompare z.subst(m1) // 1.0, x is greater
x.subst(m2) partialCompare z.subst(m2) // 0.0, equal
```

### substitution & evaluation

Expressions which build `Sym` values will attempt to "canonicalize"
the expression. If no symbolic names are present, the expression can
be evaluated to an `Algebraic` instance (which allows sign tests and
comparisons).

Values can be provided for symbolic names using the `subst` method.

### limitations

Partial comparisons and partial evaluation will fail (e.g. return
`None`) if there are unbound symbolic names needed to compute the
result. They will also fail for some complex expressions, such as
`Sym(2) ** Sym(3).sqrt` (two to the "√3-rd" power), where no
evaluation strategy currently exists.

Since the underlying type (`spire.math.Rational`) can only use `Int`
arguments for `pow` and `nroot`, it's possible to cause errors by
using very large integer values with these methods in `Sym`.

The current strategy is very inefficient -- a "real" solution probably
needs hashing or ordering constraints to make cancellation faster. It
probably also needs explicit rules about canonical/normal forms. It
should definitely have an `expand` method, and probably some
heuristics to support factoring where possible.

The tests pass, but there are probably also bugs.
