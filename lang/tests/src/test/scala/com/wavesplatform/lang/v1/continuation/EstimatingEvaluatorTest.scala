package com.wavesplatform.lang.v1.continuation

import cats.Id
import cats.kernel.Monoid
import com.wavesplatform.lang.Common.{NoShrink, addCtx, sampleTypes}
import com.wavesplatform.lang.{Common, Global, utils}
import com.wavesplatform.lang.directives.values.{V3, V4}
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.continuation.EstimatingEvaluator.EvalContext
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class EstimatingEvaluatorTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink with Inside {
  private val estimatingEvaluator = EstimatingEvaluator(limit = 100)

  private def eval(script: String): Either[String, (Long, (EvalContext, Terms.EVALUATED))] = {
    val version = V4
    val ctx     = PureContext.build(Global, version).withEnvironment[Environment]
    val evaluationContext = {
      val environment = Common.emptyBlockchainEnvironment()
      LoggedEvaluationContext[Environment, Id](_ => _ => (), ctx.evaluationContext(environment))
    }
    val estimatorContext = {
      val functionCosts = utils.functionCosts(version).mapValues(_.value())
      EstimatorContext(functionCosts)
    }
    val parsed = Parser.parseExpr(script).get.value

    for {
      compiled  <- ExpressionCompiler(ctx.compilerContext, parsed)
      evaluated <- estimatingEvaluator.eval(compiled._1, (estimatorContext, evaluationContext))
    } yield evaluated
  }

  property("keep both results and contexts") {
    inside(eval(
      """
        | let a = 1
        | let b = 2
        | func f(x: Int, y: Int) = x + y
        | f(a, b)
      """.stripMargin
    )) {
      case Right((cost, (ctx, result))) =>
        cost shouldBe 7
        result shouldBe CONST_LONG(3)
        ctx.letDefs("a").value.value shouldBe Right(CONST_LONG(1))
        ctx.letDefs("b").value.value shouldBe Right(CONST_LONG(2))
        ctx.functions.get(User("f")) shouldBe 'defined
    }
  }

  property("stop if complexity exceeds limit") {
    val n = 50
    val script =
      s"""
         | func f0() = 0
         | ${(0 until n).map(i => s"func f${i + 1}() = if (true) then f$i() else f$i()").mkString("\n") }
         | f$n()
      """.stripMargin
    // produce 101 complexity

    eval(script) shouldBe 'left
  }
}
