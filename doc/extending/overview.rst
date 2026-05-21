.. _extending-overview

Overview of Verilisp's internals
================================

Verilisp is a domain-specific language (DSL) embedded into Common
Lisp. This means that a Common Lisp program can contain within is a
Verilisp program.

It's tempting to think of Verilisp as an *extension* of Common Lisp,
but that's not quite right. In many ways Verlisp is a *restriction* of
Common Lisp to those features that can be synthesised as digital
circuits in hardware: it's the **synthesisable fragment**of Common
Lisp. Verilisp has its own types, functions, forms, and rules -- most
of which are conveniently so close to Common Lisp's equivalents that
it's often not entirely clear that there are restriction at all.

More importantly, Verilisp can make use of *all* of Common Lisp's
features *as long as* what results is in the synthesisable fragment.
Notably this means we can define macros that *use* Common Lisp but
*generate* Verilisp. But because the Verilisp compiler is itself
written in Common Lisp we can also quite simply extend the compiler,
using Common Lisp to add more features. In this sense Verilisp offers
*more* (or at least different) extension possibilities than Common
Lisp does.


Compiler structure
------------------

Traditional compilers are typically structured as a number of "passes"
that operate over the representation of the program being compiled.
The classic passes are lexical analysis, syntax analysis (parsing),
static analysis (typing), optimisation, and code generation.

Verilisp is structured as a **nanopass compiler**
:cite:`NanopassCompilerEducation`. As their name suggests, these also
use passes over the program representation; unlike the traditional
approach there can be far more passes, each targeted at a very
specific operation on the program. A nanopass might, for example,
perform a transformation on one specific program construct while
leaving all others alone. This makes the code more modular and more
composable. It can also make compilation slower as there are more
passes over the program representation, but for modern systems that's
seldom an issue, and a small cost for the benefits.

As an additional benefit, the program representation used by the
nanopass compiler is Lisp itself -- unlike for more traditional
languages that must parse a textual form into an abstract syntax tree.
In Lisp the program is already represented as a tree (of
s-expressions) that we can operate on directly.


Terminology
-----------

In what follows we'll use the term **guest language** (or just
**guest**) to mean the language the compiler processes. (This is
Verilisp for our purposes most of the time.) The **host language** (or
**host**) is the language the compiler and the guest are embedded
into: Common Lisp in our case.
