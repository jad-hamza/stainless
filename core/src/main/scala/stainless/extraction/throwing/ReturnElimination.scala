/* Copyright 2009-2021 EPFL, Lausanne */

package stainless
package extraction
package throwing

trait ReturnElimination
  extends oo.CachingPhase
    with IdentitySorts
    with SimplyCachedFunctions
    with SimpleFunctions
    with oo.IdentityTypeDefs
    with oo.IdentityClasses
    with utils.SyntheticSorts { self =>

  val s: Trees
  val t: s.type

  import s._

  // private val cachedExistsMap = collection.mutable.Map[Expr, Boolean]()
  // private def cachedExists(e: Expr): Boolean = cachedExistsMap.getOrElseUpdate(e,
  //   exprOps.exists {
  //     case Return(_) => true
  //     case _ => false
  //   }(e)
  // )
  // private def hasReturn(e: Expr): Boolean = cachedExists(e)

  protected class TransformerContext(val symbols: Symbols) {
    // we precompute the set of expressions that contain a return
    val exprHasReturn = collection.mutable.Set[Expr]()
    for (fd <- symbols.functions.values) {
      exprOps.postTraversal {
        case e @ Return(_) => exprHasReturn += e
        case e @ Operator(es, _) if (es.exists(exprHasReturn)) => exprHasReturn += e
        case _ => ()
      }(fd.fullBody)
    }

    val funHasReturn: Set[Identifier] = symbols.functions.values.collect {
      case fd if exprHasReturn(fd.fullBody) => fd.id
    }.toSet
  }

  override protected def getContext(symbols: Symbols) = new TransformerContext(symbols)

  protected def extractFunction(tc: TransformerContext, fd: FunDef): FunDef = {
    implicit val symboms = tc.symbols

    if (tc.funHasReturn(fd.id)) {
      val retType = fd.returnType
      val specced = exprOps.BodyWithSpecs(fd.fullBody)
      val returnedExpr = ValDef.fresh("returnedExpr", ADTType(OptionSort.option, Seq(retType)))

      def mkNone = ADT(OptionSort.none, Seq(retType), Seq())
      def mkSome(e: Expr) = ADT(OptionSort.some, Seq(retType), Seq(e))


      class ReturnEliminationTransformer(returnedExpr: ValDef, retType: Type) extends SelfTreeTransformer {

        private def continueWith(e: Expr): Expr = {
          IfExpr(
            Equals(returnedExpr.toVariable, mkNone),
            e,
            FunctionInvocation(OptionSort.get, Seq(retType), Seq(returnedExpr.toVariable))
          )
        }

        override def transform(expr: Expr): Expr = expr match {
          case _ if !tc.exprHasReturn(expr) => expr

          case Return(e) =>
            val toReturnVal = ValDef.fresh("toReturn", retType)
            Let(toReturnVal, e, Block(
              Seq(Assignment(returnedExpr.toVariable, mkSome(toReturnVal.toVariable)).setPos(expr)),
              toReturnVal.toVariable
            ).setPos(expr)).setPos(expr)

          case Let(vd, e, body) =>
            if (tc.exprHasReturn(e))
              Let(vd, transform(e), continueWith(transform(body)).setPos(expr)).setPos(expr)
            else
              Let(vd, e, transform(body)).setPos(expr)

          case Block(es, last) =>
            def processBlockExpressions(es: Seq[Expr]): Seq[Expr] = es match {
              case Seq(e) => Seq(transform(e))
              case e +: rest if (tc.exprHasReturn(e)) =>
                val transformedRest = rest.map(transform)
                Seq(
                  transform(e),
                  continueWith(
                    Block(transformedRest.init, transformedRest.last).setPos(rest.head)
                  ).setPos(rest.head)
                )
              case e +: rest => transform(e) +: processBlockExpressions(rest)
            }
            val res = processBlockExpressions(es :+ last)
            Block(res.init, res.last).setPos(expr)

          case _ => super.transform(expr)
        }
      }


      // var returnedExpr = None
      // transform(body)
      val newBody = specced.bodyOpt.map(body =>
        LetVar(returnedExpr,
          ADT(OptionSort.none, Seq(retType), Seq()),
          new ReturnEliminationTransformer(returnedExpr, retType).transform(body)
        )
      )
      fd.copy(fullBody = specced.withBody(newBody, retType).reconstructed).setPos(fd)
    } else fd
  }

  override protected def extractSymbols(context: TransformerContext, symbols: s.Symbols): t.Symbols = {
    super.extractSymbols(context, symbols)
      .withFunctions(OptionSort.functions(symbols))
      .withSorts(OptionSort.sorts(symbols))
  }
}

object ReturnElimination {
  def apply(trees: Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new ReturnElimination {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  }
}
