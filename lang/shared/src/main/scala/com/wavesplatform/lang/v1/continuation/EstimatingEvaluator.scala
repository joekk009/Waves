package com.wavesplatform.lang.v1.continuation

import cats.Id
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR, FUNC, LET}
import com.wavesplatform.lang.v1.estimator.v3.{EstimatorContext, ScriptEstimatorV3}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1._
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext}
import com.wavesplatform.lang.v1.task.TaskM
import com.wavesplatform.lang.v1.traits.Environment

object EstimatingEvaluator {
  def apply(limit: Long): ScriptWalker[Id, (EstimatorContext, LoggedContext), ExecutionError, (Long, (EvalContext, EVALUATED))] =
    ScriptWalker.composite(estimator, evaluator)
      .limited(_._1 <= limit, _ => "ERROR")

  type LoggedContext = LoggedEvaluationContext[Environment, Id]
  type EvalContext = EvaluationContext[Environment, Id]
  type EvalM = TaskM[LoggedContext, ExecutionError, (EvalContext, EVALUATED)]

  private val evaluator: ScriptWalker[Id, LoggedContext, ExecutionError, (EvalContext, EVALUATED)] =
    new ScriptWalker[Id, LoggedContext, ExecutionError, (EvalContext, EVALUATED)] {
      private val inner: EvaluatorV1[Id, Environment] = EvaluatorV1()

      override protected def getter(expr: EXPR, field: String): EvalM =
        inner.evalGetter(expr, field)

      override protected def letBlock(let : LET, body: EXPR): EvalM =
        inner.evalLetBlock(let, body)

      override protected def funcBlock(func: FUNC, body: EXPR): EvalM =
        inner.evalFuncBlock(func, body)

      override protected def ifBlock(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM =
        inner.evalIF(cond, ifTrue, ifFalse)

      override protected def ref(key: String): EvalM =
        inner.evalRef(key)

      override protected def functionCall(func: FunctionHeader, args: List[EXPR]): EvalM =
        inner.evalFunctionCall(func, args)

      override protected def evaluated(e: EVALUATED): EvalM =
        inner.evaluated(e)
    }

  type EstimatorM = EstimatorContext.EvalM[Long]

  private val estimator: ScriptWalker[Id, EstimatorContext, ExecutionError, Long] =
    new ScriptWalker[Id, EstimatorContext, ExecutionError, Long] {
      private val inner: ScriptEstimatorV3.type = ScriptEstimatorV3

      override protected def getter(expr: EXPR, field: String): EstimatorM =
        inner.evalGetter(expr)

      override protected def letBlock(let: LET, body: EXPR): EstimatorM =
        inner.evalLetBlock(let, body)

      override protected def funcBlock(func: FUNC, body: EXPR): EstimatorM =
        inner.evalFuncBlock(func, body)

      override protected def ifBlock(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EstimatorM =
        inner.evalIF(cond, ifTrue, ifFalse)

      override protected def ref(key: String): EstimatorM =
        inner.markRef(key)

      override protected def functionCall(func: FunctionHeader, args: List[EXPR]): EstimatorM =
        inner.evalFuncCall(func, args)

      override protected def evaluated(e: EVALUATED): EstimatorM =
        inner.evaluated
    }
}
