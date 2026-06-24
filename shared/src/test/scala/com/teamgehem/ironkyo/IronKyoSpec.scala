package com.teamgehem.ironkyo

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import kyo.*
import munit.FunSuite

case class TestUser(
    id: Int :| Positive,
    name: String :| MinLength[3],
    age: Int :| Positive
)

class IronKyoSpec extends FunSuite {

  extension [E, A](r: Result[E, A])
    def toEither: Either[E, A] = r match
      case Result.Success(v) => Right(v)
      case Result.Failure(e) => Left(e)
      case Result.Panic(ex)  => throw ex

  test("refineAbort should succeed for valid constraints") {
    val result: Result[ConstraintError, Int :| Positive] =
      Abort.run(42.refineAbort[Positive]).eval
    assertEquals(result.toEither, Right(42: Int :| Positive))
  }

  test(
    "refineAbort should abort with ConstraintError for invalid constraints"
  ) {
    val result = Abort.run((-42).refineAbort[Positive]).eval
    assert(result.toEither.isLeft)
    result.toEither match {
      case Left(ConstraintError(msg, value, typeName)) =>
        assertEquals(value, "-42")
        assertEquals(typeName, "int")
      case _ => fail("Expected Left(ConstraintError)")
    }
  }

  test("refineAbortWith should abort with custom mapped error") {
    val result = Abort
      .run((-42).refineAbortWith[Positive, String](msg => s"Custom: $msg"))
      .eval
    assertEquals(result.toEither, Left("Custom: Should be strictly positive"))
  }

  test("validateAll and into should validate fields and map to case class") {
    val result = Abort.run {
      validateAll(
        field(1).as[Positive],
        field("John").as[MinLength[3]],
        field(25).as[Positive]
      ).into[TestUser]
    }.eval
    assertEquals(result.toEither, Right(TestUser(1, "John", 25)))
  }

  test("validateAll should aggregate errors") {
    val result = Abort.run {
      validateAll(
        field(-1).as[Positive],
        field("Jo").as[MinLength[3]],
        field(-25).as[Positive]
      ).into[TestUser]
    }.eval
    assert(result.toEither.isLeft)
    result.toEither match {
      case Left(AggregatedConstraintError(errors)) =>
        assertEquals(errors.size, 3)
        assertEquals(errors(0).inputValue, "-1")
        assertEquals(errors(1).inputValue, "Jo")
        assertEquals(errors(2).inputValue, "-25")
      case _ => fail("Expected Left(AggregatedConstraintError)")
    }
  }

  test(
    "validateInto should automatically validate fields and map to case class (direct parameters)"
  ) {
    val result = Abort.run {
      validateInto[TestUser](1, "John", 25)
    }.eval
    assertEquals(result.toEither, Right(TestUser(1, "John", 25)))
  }

  test(
    "validateInto should aggregate errors automatically (direct parameters)"
  ) {
    val result = Abort.run {
      validateInto[TestUser](-1, "Jo", -25)
    }.eval
    assert(result.toEither.isLeft)
    result.toEither match {
      case Left(AggregatedConstraintError(errors)) =>
        assertEquals(errors.size, 3)
        assertEquals(errors(0).inputValue, "-1")
        assertEquals(errors(1).inputValue, "Jo")
        assertEquals(errors(2).inputValue, "-25")
      case _ => fail("Expected Left(AggregatedConstraintError)")
    }
  }

  test("validateInto should accept already-refined types directly") {
    val validId: Int :| Positive = 1.refine[Positive]
    val result = Abort.run {
      validateInto[TestUser](validId, "John", 25)
    }.eval
    assertEquals(result.toEither, Right(TestUser(1, "John", 25)))
  }

  test("validateInto should fail compilation if types are mismatched") {
    val errors = compileErrors("""
      import com.teamgehem.ironkyo.*
      validateInto[TestUser]("wrong-type-for-id", "John", 25)
    """)
    assert(errors.contains("Cannot prove that"))
  }

  test(
    "validateInto should fail compilation if arity is mismatched (too few)"
  ) {
    val errors = compileErrors("""
      import com.teamgehem.ironkyo.*
      validateInto[TestUser](1, "John")
    """)
    assert(
      errors.contains("Fewer arguments") || errors.contains(
        "cannot find parameter"
      )
    )
  }

  test(
    "validateInto should fail compilation if arity is mismatched (too many)"
  ) {
    val errors = compileErrors("""
      import com.teamgehem.ironkyo.*
      validateInto[TestUser](1, "John", 25, "extra")
    """)
    assert(
      errors.contains("More arguments") || errors.contains(
        "cannot find parameter"
      )
    )
  }
}
