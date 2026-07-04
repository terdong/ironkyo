# IronKyo

[![JitPack](https://jitpack.io/v/terdong/ironkyo.svg)](https://jitpack.io/#terdong/ironkyo)

A lightweight, type-safe bridge between **[Iron](https://github.com/Iltotore/iron)** (Refined Types) and **[Kyo](https://github.com/getkyo/kyo)** (Algebraic Effects) for Scala 3, with out-of-the-box support for both **JVM** and **Scala.js**.

## Features

- **Refined Aborts**: Lift constraint validation checks directly into Kyo's `Abort` effect (`refineAbort`, `refineAbortWith`).
- **Validation DSL**: Accumulate multiple validations (`validateAll`) and raise aggregated constraint errors (`AggregatedConstraintError`).
- **Zero-Boilerplate Mapping**: Transform validated tuples directly into case classes using type-safe Mirrors (`.into[T]`).
- **Cross-Platform**: Compile and run seamlessly in JVM and Scala.js.

## Installation

Add the JitPack resolver to your `build.sbt`:

```scala
resolvers += "jitpack" at "https://jitpack.io"
```

Then, add the library dependency to your `build.sbt`:

```scala
// For JVM-only projects
libraryDependencies += "com.github.terdong.ironkyo" %% "ironkyo" % "0.1.2"

// For Scala.js or cross-platform/shared projects
libraryDependencies += "com.github.terdong.ironkyo" %%% "ironkyo" % "0.1.2"
```

*Note: This library is experimental and has strict version compatibility requirements. It is compatible with **Scala 3.8.4 and newer** (compiled with Scala 3.8.4). This constraint exists to align with **Kyo 1.0.0-RC4** (which is compiled against Scala 3.8.3). Due to tracking these bleeding-edge releases, older Scala 3 versions are not supported.*

## Usage

### 1. Refinement to Abort Effect

You can lift standard Iron type refinements into the Kyo effect system. If the constraint fails, it short-circuits via Kyo's `Abort` effect:

```scala
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.*
import kyo.*
import com.teamgehem.ironkyo.*

val validateAge: Int :| Positive < Abort[ConstraintError] =
  42.refineAbort[Positive]

val customError: Int :| Positive < Abort[String] =
  -5.refineAbortWith[Positive, String](msg => s"Validation Failed: $msg")
```

To handle and extract details from a `ConstraintError` failure, run the effect and inspect the resulting Kyo `Result`:

```scala
val result: Result[ConstraintError, Int :| Positive] =
  Abort.run(validateAge).eval

result match {
  case Result.Success(refinedValue) =>
    println(s"Valid value: $refinedValue")
  case Result.Failure(error) =>
    // error is of type ConstraintError
    println(s"Validation failed: ${error.message}")
    println(s"Failed input value: ${error.inputValue}")
    println(s"Target type: ${error.typeName}")
  case Result.Panic(ex) =>
    println(s"Unexpected system panic: ${ex.getMessage}")
}
```

### 2. Multi-field Form Validation

You can validate multiple fields together. There are two ways to do this:

#### A. Case Class Validation (`validateInto`)
If you are validating raw fields directly into a Case Class, use `validateInto[T]`. This leverages the constraints already defined on the fields of `T` at compile-time, eliminating the need to repeat constraints at the call-site. It is also fully type-safe at compile-time:

```scala
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import kyo.*
import com.teamgehem.ironkyo.*

// 1. Define your domain model with Iron refined types
case class User(
  id: Int :| Positive,
  username: String :| MinLength[3],
  age: Int :| Positive
)

// 2. Perform validation and map into the case class
val validatedUser: User < Abort[AggregatedConstraintError] =
  validateInto[User](1, "John", 25)
```

* **Compile-Time Safety**: If you pass mismatched types (e.g. passing a `String` for `id`), or the wrong number of arguments, the compiler will raise a compilation error.
* **Redundant Check Skip**: If you pass pre-validated/refined values (e.g. `Int :| Positive`), `validateInto` will automatically skip the runtime validation for those fields.

#### B. Ad-hoc Tuple Validation (`validateAll`)
If you do not have a case class and want to validate several raw fields into a tuple of refined types:

```scala
val validated: (Int :| Positive, String :| MinLength[3]) < Abort[AggregatedConstraintError] =
  validateAll(
    field(1).as[Positive],
    field("John").as[MinLength[3]]
  )
```

If multiple fields are invalid under either approach, all validation errors are accumulated in `AggregatedConstraintError`:

```scala
val failingValidation =
  validateInto[User](-1, "Jo", -25)

// Running Abort.run(...) will yield:
// Left(AggregatedConstraintError(List(
//   ConstraintError[Int]: -1 — Should be strictly positive,
//   ConstraintError[String]: Jo — Should have min length of 3,
//   ConstraintError[Int]: -25 — Should be strictly positive
// )))
```

## Contributing

Contributions are welcome! Whether it's reporting bugs, suggesting new features, or submitting pull requests, we appreciate any help to improve this library. 

Feel free to check out the [issues](https://github.com/terdong/ironkyo/issues) or open a new pull request. Let's build a better bridge between Iron and Kyo together!

## License

This project is licensed under the Apache 2.0 License.
