package com.teamgehem.ironkyo

import scala.deriving.Mirror

import io.github.iltotore.iron.*
import kyo.*

// ─────────────────────────────────────────────────────────────────────────────
// Error Types
// ─────────────────────────────────────────────────────────────────────────────

case class ConstraintError(
    message: String,
    inputValue: String,
    typeName: String
):
  override def toString: String =
    "ConstraintError[" + typeName + "]: " + inputValue + " — " + message

case class AggregatedConstraintError(errors: List[ConstraintError]):
  override def toString: String =
    errors.map(_.toString).mkString("\n")

// ─────────────────────────────────────────────────────────────────────────────
// RefinedField DSL Building Blocks
// ─────────────────────────────────────────────────────────────────────────────

/** Represents the validation result of a single field.
  * Created via `field(value).as[Constraint]` and passed to `validateAll(...)`.
  *
  * {{{
  * field(rawAge).as[Positive & Less[150]]
  * }}}
  */
opaque type RefinedField[A] = Either[ConstraintError, A]

object RefinedField:
  def apply[A](e: Either[ConstraintError, A]): RefinedField[A] = e

  extension [A](rf: RefinedField[A])
    def toEither: Either[ConstraintError, A] = rf

/** A builder class that wraps a raw value before refinement. */
final class FieldBuilder[A](value: A, typeName: String):
  inline def as[C](using inline c: Constraint[A, C]): RefinedField[A :| C] =
    RefinedField(
      value.refineEither[C].left.map(msg => ConstraintError(msg, value.toString, typeName))
    )

/** Entry point for the RefinedField builder.
  *
  * {{{
  * field(rawAge).as[Positive & Less[150]]
  * field(rawName).as[MinLength[3] & MaxLength[20] & Alphanumeric]
  * }}}
  */
inline def field[A](value: A)(using ct: scala.reflect.ClassTag[A]): FieldBuilder[A] =
  FieldBuilder(value, ct.runtimeClass.getSimpleName)

// ─────────────────────────────────────────────────────────────────────────────
// validateAll (Arity 1 ~ 9)
// ─────────────────────────────────────────────────────────────────────────────

/** Accumulates multiple `RefinedField` validations.
  * Collects all errors if any fail, otherwise returns a Tuple of refined values.
  *
  * {{{
  * val user: User < Abort[AggregatedConstraintError] =
  * validateAll(
  * field(rawId).as[Positive],
  * field(rawName).as[MinLength[3]],
  * field(rawAge).as[Positive],
  * field(rawEmail).as[Match["^[^@]+@[^@]+$"]]
  * ).into[User]
  * }}}
  */

def validateAll[T1](
    f1: RefinedField[T1]
): Tuple1[T1] < Abort[AggregatedConstraintError] =
  val result: Either[AggregatedConstraintError, Tuple1[T1]] =
    f1 match
      case Right(v1) => Right(Tuple1(v1))
      case _ =>
        val errors = List(f1).collect { case Left(e) => e }
        Left(AggregatedConstraintError(errors))
  Abort.get(result)

def validateAll[T1, T2](
    f1: RefinedField[T1], f2: RefinedField[T2]
): (T1, T2) < Abort[AggregatedConstraintError] =
  val result: Either[AggregatedConstraintError, (T1, T2)] =
    (f1, f2) match
      case (Right(v1), Right(v2)) => Right((v1, v2))
      case _ =>
        val errors = List(f1, f2).collect { case Left(e) => e }
        Left(AggregatedConstraintError(errors))
  Abort.get(result)

def validateAll[T1, T2, T3](
    f1: RefinedField[T1], f2: RefinedField[T2], f3: RefinedField[T3]
): (T1, T2, T3) < Abort[AggregatedConstraintError] =
  val result: Either[AggregatedConstraintError, (T1, T2, T3)] =
    (f1, f2, f3) match
      case (Right(v1), Right(v2), Right(v3)) => Right((v1, v2, v3))
      case _ =>
        val errors = List(f1, f2, f3).collect { case Left(e) => e }
        Left(AggregatedConstraintError(errors))
  Abort.get(result)

def validateAll[T1, T2, T3, T4](
    f1: RefinedField[T1], f2: RefinedField[T2], f3: RefinedField[T3], f4: RefinedField[T4]
): (T1, T2, T3, T4) < Abort[AggregatedConstraintError] =
  val result: Either[AggregatedConstraintError, (T1, T2, T3, T4)] =
    (f1, f2, f3, f4) match
      case (Right(v1), Right(v2), Right(v3), Right(v4)) => Right((v1, v2, v3, v4))
      case _ =>
        val errors = List(f1, f2, f3, f4).collect { case Left(e) => e }
        Left(AggregatedConstraintError(errors))
  Abort.get(result)

def validateAll[T1, T2, T3, T4, T5](
    f1: RefinedField[T1], f2: RefinedField[T2], f3: RefinedField[T3], f4: RefinedField[T4], f5: RefinedField[T5]
): (T1, T2, T3, T4, T5) < Abort[AggregatedConstraintError] =
  val result: Either[AggregatedConstraintError, (T1, T2, T3, T4, T5)] =
    (f1, f2, f3, f4, f5) match
      case (Right(v1), Right(v2), Right(v3), Right(v4), Right(v5)) => Right((v1, v2, v3, v4, v5))
      case _ =>
        val errors = List(f1, f2, f3, f4, f5).collect { case Left(e) => e }
        Left(AggregatedConstraintError(errors))
  Abort.get(result)

def validateAll[T1, T2, T3, T4, T5, T6](
    f1: RefinedField[T1], f2: RefinedField[T2], f3: RefinedField[T3], f4: RefinedField[T4], f5: RefinedField[T5], f6: RefinedField[T6]
): (T1, T2, T3, T4, T5, T6) < Abort[AggregatedConstraintError] =
  val result: Either[AggregatedConstraintError, (T1, T2, T3, T4, T5, T6)] =
    (f1, f2, f3, f4, f5, f6) match
      case (Right(v1), Right(v2), Right(v3), Right(v4), Right(v5), Right(v6)) => Right((v1, v2, v3, v4, v5, v6))
      case _ =>
        val errors = List(f1, f2, f3, f4, f5, f6).collect { case Left(e) => e }
        Left(AggregatedConstraintError(errors))
  Abort.get(result)

def validateAll[T1, T2, T3, T4, T5, T6, T7](
    f1: RefinedField[T1], f2: RefinedField[T2], f3: RefinedField[T3], f4: RefinedField[T4], f5: RefinedField[T5], f6: RefinedField[T6], f7: RefinedField[T7]
): (T1, T2, T3, T4, T5, T6, T7) < Abort[AggregatedConstraintError] =
  val result: Either[AggregatedConstraintError, (T1, T2, T3, T4, T5, T6, T7)] =
    (f1, f2, f3, f4, f5, f6, f7) match
      case (Right(v1), Right(v2), Right(v3), Right(v4), Right(v5), Right(v6), Right(v7)) => Right((v1, v2, v3, v4, v5, v6, v7))
      case _ =>
        val errors = List(f1, f2, f3, f4, f5, f6, f7).collect { case Left(e) => e }
        Left(AggregatedConstraintError(errors))
  Abort.get(result)

def validateAll[T1, T2, T3, T4, T5, T6, T7, T8](
    f1: RefinedField[T1], f2: RefinedField[T2], f3: RefinedField[T3], f4: RefinedField[T4], f5: RefinedField[T5], f6: RefinedField[T6], f7: RefinedField[T7], f8: RefinedField[T8]
): (T1, T2, T3, T4, T5, T6, T7, T8) < Abort[AggregatedConstraintError] =
  val result: Either[AggregatedConstraintError, (T1, T2, T3, T4, T5, T6, T7, T8)] =
    (f1, f2, f3, f4, f5, f6, f7, f8) match
      case (Right(v1), Right(v2), Right(v3), Right(v4), Right(v5), Right(v6), Right(v7), Right(v8)) => Right((v1, v2, v3, v4, v5, v6, v7, v8))
      case _ =>
        val errors = List(f1, f2, f3, f4, f5, f6, f7, f8).collect { case Left(e) => e }
        Left(AggregatedConstraintError(errors))
  Abort.get(result)

def validateAll[T1, T2, T3, T4, T5, T6, T7, T8, T9](
    f1: RefinedField[T1], f2: RefinedField[T2], f3: RefinedField[T3], f4: RefinedField[T4], f5: RefinedField[T5], f6: RefinedField[T6], f7: RefinedField[T7], f8: RefinedField[T8], f9: RefinedField[T9]
): (T1, T2, T3, T4, T5, T6, T7, T8, T9) < Abort[AggregatedConstraintError] =
  val result: Either[AggregatedConstraintError, (T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    (f1, f2, f3, f4, f5, f6, f7, f8, f9) match
      case (Right(v1), Right(v2), Right(v3), Right(v4), Right(v5), Right(v6), Right(v7), Right(v8), Right(v9)) => Right((v1, v2, v3, v4, v5, v6, v7, v8, v9))
      case _ =>
        val errors = List(f1, f2, f3, f4, f5, f6, f7, f8, f9).collect { case Left(e) => e }
        Left(AggregatedConstraintError(errors))
  Abort.get(result)

// ─────────────────────────────────────────────────────────────────────────────
// Core Extension Methods (Lifting to Kyo Abort)
// ─────────────────────────────────────────────────────────────────────────────

extension [A](value: A)

  inline def refineAbort[C](using
      inline constraint: Constraint[A, C],
      inline ct: scala.reflect.ClassTag[A]
  ): (A :| C) < Abort[ConstraintError] =
    // Isolate context inside pure Either to prevent Wartremover Any inference
    val result: Either[ConstraintError, A :| C] =
      value.refineEither[C] match
        case Right(refined) => Right(refined)
        case Left(msg)      => Left(ConstraintError(msg, value.toString, ct.runtimeClass.getSimpleName))

    Abort.get(result)

  inline def refineAbortWith[C, E](
      mapError: String => E
  )(using inline constraint: Constraint[A, C]): (A :| C) < Abort[E] =
    // Isolate context inside pure Either to prevent Wartremover Any inference
    val result: Either[E, A :| C] =
      value.refineEither[C] match
        case Right(refined) => Right(refined)
        case Left(msg)      => Left(mapError(msg))

    Abort.get(result)

extension [T <: Tuple](effect: T < Abort[AggregatedConstraintError])

  /** Transforms a Tuple into a matching Case Class using Scala 3 Mirrors.
    * Type-safety is verified at compile-time based on element order and types.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  inline def into[R](using m: Mirror.ProductOf[R] { type MirroredElemTypes = T }): R < Abort[AggregatedConstraintError] =
    effect.map(t => m.fromProduct(t))