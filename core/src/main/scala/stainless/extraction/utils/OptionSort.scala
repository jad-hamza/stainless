/* Copyright 2009-2021 EPFL, Lausanne */

package stainless
package extraction
package utils

trait OptionSort {

  val s: stainless.ast.Trees
  val t: inox.ast.Trees

  import t._
  import t.dsl._

  private[this] val uncheckedFlag: Option[t.Flag] = {
    if (t.isInstanceOf[stainless.ast.Trees])
      Some(t.asInstanceOf[stainless.ast.Trees].Unchecked.asInstanceOf[t.Flag])
    else
      None
  }

  private[this] val syntheticFlag: Option[t.Flag] = {
    if (t.isInstanceOf[stainless.ast.Trees])
      Some(t.asInstanceOf[stainless.ast.Trees].Synthetic.asInstanceOf[t.Flag])
    else
      None
  }

  private[this] def require(pred: t.Expr, body: t.Expr): t.Expr = {
    if (t.isInstanceOf[stainless.ast.Trees])
      stainless.trees.Require(
        pred.asInstanceOf[stainless.trees.Expr],
        body.asInstanceOf[stainless.trees.Expr],
      ).asInstanceOf[t.Expr]
    else
      body
  }

  private[this] val syntheticOption: t.ADTSort = {
    val Seq(option, some, none) =
      Seq("Option", "Some", "None").map(name => ast.SymbolIdentifier("stainless.internal." + name))
    val value = FreshIdentifier("value")
    mkSort(option)("A") { case Seq(aT) => Seq((some, Seq(t.ValDef(value, aT))), (none, Seq())) }
  }

  private[this] val syntheticIsEmpty: s.Symbols => t.FunDef = {
    def createFunction(option: Identifier, none: Identifier): t.FunDef = {
      val isEmpty = ast.SymbolIdentifier("stainless.internal.Option.isEmpty")
      val fd = mkFunDef(isEmpty)("A") {
        case Seq(aT) => (Seq("x" :: T(option)(aT)), t.BooleanType(), { case Seq(v) => v is none })
      }
      fd.copy(flags = fd.flags ++ uncheckedFlag ++ syntheticFlag)
    }

    val syntheticFunction: t.FunDef = createFunction(
      syntheticOption.id,
      syntheticOption.constructors.find(_.fields.isEmpty).get.id)

    (symbols: s.Symbols) => symbols.lookup.get[s.ADTSort]("stainless.internal.Option") match {
      case Some(sort) =>
        createFunction(sort.id, sort.constructors.find(_.fields.isEmpty).get.id)
      case None => syntheticFunction
    }
  }

  private[this] val syntheticGet: s.Symbols => t.FunDef = {
    def createFunction(option: Identifier, some: Identifier, value: Identifier): t.FunDef = {
      val get = ast.SymbolIdentifier("stainless.internal.Option.get")
      val fd = mkFunDef(get)("A") {
        case Seq(aT) => (Seq("x" :: T(option)(aT)), aT, {
          case Seq(v) => require(v is some, v.getField(value))
        })
      }
      fd.copy(flags = fd.flags ++ uncheckedFlag ++ syntheticFlag)
    }

    val syntheticFunction: t.FunDef = {
      val some = syntheticOption.constructors.find(_.fields.nonEmpty).get
      createFunction(syntheticOption.id, some.id, some.fields.head.id)
    }

    (symbols: s.Symbols) => symbols.lookup.get[s.ADTSort]("stainless.internal.Option") match {
      case Some(sort) =>
        val some = sort.constructors.find(_.fields.nonEmpty).get
        createFunction(sort.id, some.id, some.fields.head.id)
      case None => syntheticFunction
    }
  }

  private[this] def optionSort(implicit symbols: s.Symbols): inox.ast.Trees#ADTSort =
    symbols.lookup.get[s.ADTSort]("stainless.internal.Option").getOrElse(syntheticOption)

  def option(implicit symbols: s.Symbols): Identifier = optionSort.id
  def some(implicit symbols: s.Symbols): Identifier = optionSort.constructors.find(_.fields.nonEmpty).get.id
  def none(implicit symbols: s.Symbols): Identifier = optionSort.constructors.find(_.fields.isEmpty).get.id

  // def value(implicit symbols: s.Symbols): Identifier = optionSort.constructors.flatMap(_.fields).head.id

  def optionIsEmpty(implicit symbols: s.Symbols): Identifier =
    symbols.lookup.get[s.FunDef]("stainless.internal.Option.isEmpty").getOrElse(syntheticIsEmpty(symbols)).id
  def optionGet(implicit symbols: s.Symbols): Identifier =
    symbols.lookup.get[s.FunDef]("stainless.internal.Option.get").getOrElse(syntheticGet(symbols)).id

  def optionSortOpt(implicit symbols: s.Symbols): Option[t.ADTSort] =
    symbols.lookup.get[s.ADTSort]("stainless.internal.Option") match {
      case Some(_) => None
      case None => Some(syntheticOption)
    }

  def optionFunctions(implicit symbols: s.Symbols): Seq[t.FunDef] =
    (symbols.lookup.get[s.FunDef]("stainless.internal.Option.isEmpty") match {
      case Some(_) => Seq()
      case None => Seq(syntheticIsEmpty(symbols))
    }) ++ (symbols.lookup.get[s.FunDef]("stainless.internal.Option.get") match {
      case Some(_) => Seq()
      case None => Seq(syntheticGet(symbols))
    })

  // def optionKey(implicit symbols: s.Symbols): CacheKey = new SeqKey(
  //   symbols.lookup.get[s.ADTSort]("stainless.internal.Option").map(SortKey(_)).toSeq ++
  //   symbols.lookup.get[s.FunDef]("stainless.internal.Option.isEmpty").map(FunctionKey(_)) ++
  //   symbols.lookup.get[s.FunDef]("stainless.internal.Option.get").map(FunctionKey(_))
  // )
}
