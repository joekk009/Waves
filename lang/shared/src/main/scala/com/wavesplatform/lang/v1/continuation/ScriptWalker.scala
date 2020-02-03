package com.wavesplatform.lang.v1.continuation

import cats.Monad
import com.wavesplatform.lang.EvalF
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.task.TaskMT
import com.wavesplatform.lang.v1.task.imports.{pure, raiseError}

trait ScriptWalker[F[_], S <: AnyRef, E, R] { self =>
  def eval(expr: EXPR, ctx: S): F[Either[E, R]] =
    apply(expr).run(ctx).value._2

  def apply(s: EXPR): TaskMT[F, S, E, R] =
    s match {
      case GETTER(expr, field)       => getter(expr, field)
      case LET_BLOCK(let, body)      => letBlock(let, body)
      case BLOCK(let: LET, body)     => letBlock(let, body)
      case BLOCK(func: FUNC, body)   => funcBlock(func, body)
      case IF(cond, ifTrue, ifFalse) => ifBlock(cond, ifTrue, ifFalse)
      case REF(key)                  => ref(key)
      case FUNCTION_CALL(func, args) => functionCall(func, args)
      case e: EVALUATED              => evaluated(e)
    }

  protected def getter(expr: EXPR, field: String): TaskMT[F, S, E, R]

  protected def letBlock(let: LET, body: EXPR): TaskMT[F, S, E, R]

  protected def funcBlock(func: FUNC, body: EXPR): TaskMT[F, S, E, R]

  protected def ifBlock(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): TaskMT[F, S, E, R]

  protected def ref(key: String): TaskMT[F, S, E, R]

  protected def functionCall(func: FunctionHeader, args: List[EXPR]): TaskMT[F, S, E, R]

  protected def evaluated(e: EVALUATED): TaskMT[F, S, E, R]

  private[continuation] def limited(cond: R => Boolean, errorMsg: R => E)(implicit ev: Monad[EvalF[F, ?]], ev2: Monad[F]): ScriptWalker[F, S, E, R] =
    new ScriptWalker[F, S, E, R] {
      private val none = pure[F, S, E, Unit](())

      override def apply(s: EXPR): TaskMT[F, S, E, R] =
        for {
          r <- self.apply(s)
          _ <- if (cond(r)) none else raiseError[F, S, E, R](errorMsg(r))
        } yield r

      override protected def getter(expr: EXPR, field: String): TaskMT[F, S, E, R] =
        self.getter(expr, field)

      override protected def letBlock(let: LET, body: EXPR): TaskMT[F, S, E, R] =
        self.letBlock(let, body)

      override protected def funcBlock(func: FUNC, body: EXPR): TaskMT[F, S, E, R] =
        self.funcBlock(func, body)

      override protected def ifBlock(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): TaskMT[F, S, E, R] =
        self.ifBlock(cond, ifTrue, ifFalse)

      override protected def ref(key: String): TaskMT[F, S, E, R] =
        self.ref(key)

      override protected def functionCall(func: FunctionHeader, args: List[EXPR]): TaskMT[F, S, E, R] =
        self.functionCall(func, args)

      override protected def evaluated(e: EVALUATED): TaskMT[F, S, E, R] =
        self.evaluated(e)
    }
}

object ScriptWalker {
  def composite[F[_], E, S1 <: AnyRef, S2 <: AnyRef, R1, R2](
     a: ScriptWalker[F, S1, E, R1],
     b: ScriptWalker[F, S2, E, R2]
  )(implicit ev: Monad[F], ev2: Monad[EvalF[F, ?]]): ScriptWalker[F, (S1, S2), E, (R1, R2)] =
    new ScriptWalker[F, (S1, S2), E, (R1, R2)] {
      override def getter(expr: EXPR, field: String): TaskMT[F, (S1, S2), E, (R1, R2)] =
        a.getter(expr, field) product b.getter(expr, field)

      override def letBlock(let: LET, body: EXPR): TaskMT[F, (S1, S2), E, (R1, R2)] =
        a.letBlock(let, body) product b.letBlock(let, body)

      override def funcBlock(func: FUNC, body: EXPR): TaskMT[F, (S1, S2), E, (R1, R2)] =
        a.funcBlock(func, body) product b.funcBlock(func, body)

      override def ifBlock(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): TaskMT[F, (S1, S2), E, (R1, R2)] =
        a.ifBlock(cond, ifTrue, ifFalse) product b.ifBlock(cond, ifTrue, ifFalse)

      override def ref(key: String): TaskMT[F, (S1, S2), E, (R1, R2)] =
        a.ref(key) product b.ref(key)

      override def functionCall(func: FunctionHeader, args: List[EXPR]): TaskMT[F, (S1, S2), E, (R1, R2)] =
        a.functionCall(func, args) product b.functionCall(func, args)

      override def evaluated(e: EVALUATED): TaskMT[F, (S1, S2), E, (R1, R2)] =
        a.evaluated(e) product b.evaluated(e)
    }
}