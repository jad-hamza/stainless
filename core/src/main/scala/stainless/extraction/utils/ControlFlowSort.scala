/* Copyright 2009-2021 EPFL, Lausanne */

package stainless
package extraction
package utils

trait ControlFlowSort {
  val s: stainless.ast.Trees
  val t: stainless.ast.Trees

  // ControlFlowSort represents the following class:
  // sealed abstract class ControlFlow[Ret, Cur]
  // case class Return[Ret, Cur](value: Ret)  extends ControlFlow[Ret, Cur]
  // case class Proceed[Ret, Cur](value: Cur) extends ControlFlow[Ret, Cur]
  protected object ControlFlowSort {
    import t._
    import t.dsl._

    val syntheticControlFlow: t.ADTSort = {
      val Seq(controlFlow, ret, proceed) =
        Seq("ControlFlow", "Return", "Proceed").map(name => ast.SymbolIdentifier("stainless.internal." + name))
      val retValue = FreshIdentifier("value")
      val proceedValue = FreshIdentifier("value")
      mkSort(controlFlow)("Ret", "Cur") { case Seq(retT, curT) =>
        Seq(
          (ret, Seq(t.ValDef(retValue, retT))),
          (proceed, Seq(t.ValDef(proceedValue, curT)))
        )
      }
    }

    val controlFlowId: Identifier = syntheticControlFlow.id
    val retId: Identifier = syntheticControlFlow.constructors.find(_.id.name == "Return").get.id
    val proceedId: Identifier = syntheticControlFlow.constructors.find(_.id.name == "Proceed").get.id

    object Return {
      def unapply(e: Expr): Option[Expr] = e match {
        case ADT(`retId`, Seq(_, _), Seq(arg)) => Some(arg)
        case _ => None
      }
    }

    object Proceed {
      def unapply(e: Expr): Option[Expr] = e match {
        case ADT(`proceedId`, Seq(_, _), Seq(arg)) => Some(arg)
        case _ => None
      }
    }

    def controlFlow(retT: Type, curT: Type): Type = ADTType(controlFlowId, Seq(retT, curT))
    def ret(retT: Type, curT: Type, e: Expr) = ADT(retId, Seq(retT, curT), Seq(e)).setPos(e)
    def proceed(retT: Type, curT: Type, e: Expr) = ADT(proceedId, Seq(retT, curT), Seq(e)).setPos(e)

    def buildMatch(
      retT: Type, curT: Type,
      scrut: Expr,
      retCase: Variable => Expr,
      proceedCase: Variable => Expr,
      pos: inox.utils.Position
    ): Expr = {
      val retVal = ValDef.fresh("retValue", retT).setPos(pos)
      val proceedVal = ValDef.fresh("proceedValue", curT).setPos(pos)
      MatchExpr(scrut, Seq(
        MatchCase(
          ADTPattern(None, retId, Seq(retT, curT), Seq(WildcardPattern(Some(retVal)))).setPos(pos),
          None,
          retCase(retVal.toVariable)
        ).setPos(pos),
        MatchCase(
          ADTPattern(None, proceedId, Seq(retT, curT), Seq(WildcardPattern(Some(proceedVal)))).setPos(pos),
          None,
          proceedCase(proceedVal.toVariable)
        ).setPos(pos),
      )).setPos(pos)
    }

    def andThen(retT: Type, curT: Type, nextT: Type, previous: Expr, next: Variable => Expr, pos: inox.utils.Position): Expr = {
      buildMatch(retT, curT, previous,
        rv => ret(retT, nextT, rv),
        next,
        pos
      )
    }
  }
}
