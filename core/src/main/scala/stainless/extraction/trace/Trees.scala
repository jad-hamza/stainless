/* Copyright 2009-2021 EPFL, Lausanne */

package stainless
package extraction
package trace

trait Trees extends termination.Trees { self =>

  case object TraceInduct extends Flag("traceInduct", Seq())

  override def extractFlag(name: String, args: Seq[Expr]): Flag = (name, args) match {
    case ("traceInduct", Seq()) => TraceInduct
    case _ => super.extractFlag(name, args)
  }

  override def getDeconstructor(that: inox.ast.Trees): inox.ast.TreeDeconstructor { val s: self.type; val t: that.type } = that match {
    case tree: Trees => new TreeDeconstructor {
      protected val s: self.type = self
      protected val t: tree.type = tree
    }.asInstanceOf[TreeDeconstructor { val s: self.type; val t: that.type }]

    case _ => super.getDeconstructor(that)
  }
}

trait Printer extends termination.Printer {
  protected val trees: Trees
}

trait TreeDeconstructor extends termination.TreeDeconstructor {
  protected val s: Trees
  protected val t: Trees

  override def deconstruct(f: s.Flag): DeconstructedFlag = f match {
    case s.TraceInduct => (Seq(), Seq(), Seq(), (_, _, _) => t.TraceInduct)
    case _ => super.deconstruct(f)
  }
}
