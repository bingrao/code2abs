package org.ucf.ml
package parser


import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, MethodDeclaration, VariableDeclarator}
import com.github.javaparser.ast.visitor.{TreeVisitor, VoidVisitorAdapter}

import scala.collection.mutable.ListBuffer
import com.github.javaparser.ast.{Node, PackageDeclaration}
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, Type}
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.expr.{AssignExpr, FieldAccessExpr, MethodCallExpr, MethodReferenceExpr, ObjectCreationExpr, SimpleName}

import scala.collection.JavaConversions._
import java.util.stream.Collectors

import com.github.javaparser.ast.stmt.ReturnStmt
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import org.ucf.ml.utils.Context
import tree.EnrichedTrees

trait Visitor extends EnrichedTrees {

  case class TypeCalculatorVisitor() extends VoidVisitorAdapter[JavaParserFacade] {
    override def visit(n:ReturnStmt, arg:JavaParserFacade):Unit = {
      super.visit(n, arg)
      if (n.getExpression.isPresent)
        logger.info(s"${n.getExpression.get().toString} " +
          s"has type of ${arg.getType(n.getExpression.get())}")
    }

    override def visit(n: MethodCallExpr, arg: JavaParserFacade): Unit = {
      super.visit(n, arg)

      logger.info(s"${n.getName} has type of ${arg.getType(n).describe()}")

      if (arg.getType(n).isReference) {
        arg.getType(n)
          .asReferenceType()
          .getAllAncestors
          .foreach(tp => logger.info(s"Ancestor ${tp.describe()}"))
      }
    }
  }


  case class VariableDecl() extends VoidVisitorAdapter[ListBuffer[String]] {
    override def visit(md:VariableDeclarator, c:ListBuffer[String]): Unit = {
      c.+=(md.getNameAsString)
      super.visit(md,c)
    }
  }

  @deprecated
  case class MethodNameCollector() extends VoidVisitorAdapter[ListBuffer[String]] {
    override def visit(md:MethodDeclaration, c:ListBuffer[String]): Unit = {
      c.+=(md.getNameAsString)
      super.visit(md,c)
    }
  }


  case class ScopeCollector() extends VoidVisitorAdapter[ListBuffer[Node]] {
    override def visit(n:MethodCallExpr, c:ListBuffer[Node]): Unit = {
      if (n.getScope.isPresent) c.+=(n)
      super.visit(n,c)
    }

    override def visit(n:MethodReferenceExpr, c:ListBuffer[Node]): Unit = {
      c.+=(n)
      super.visit(n,c)
    }

    override def visit(n:ObjectCreationExpr, c:ListBuffer[Node]): Unit = {
      if (n.getScope.isPresent) c.+=(n)
      super.visit(n,c)
    }

    override def visit(n:FieldAccessExpr, c:ListBuffer[Node]): Unit = {
      c.+=(n.getScope)
      super.visit(n,c)
    }

    override def visit(n:ClassOrInterfaceType, c:ListBuffer[Node]): Unit = {
      if (n.getScope.isPresent) c.+=(n.getScope.get())
      super.visit(n,c)
    }
  }

  @deprecated
  case class addPositionVisitor(ctx:Context) extends TreeVisitor {
    override def process(node: Node): Unit = {
      node match {
        case p: PackageDeclaration =>
          logger.info(f"${p.getName} -> ${p.getPosition(ctx)}")
        case c: ClassOrInterfaceDeclaration =>
          logger.info(f"${c.getName} -> ${c.getPosition(ctx)}")
        case m: MethodDeclaration =>
          logger.info(f"${m.getName} -> ${m.getPosition(ctx)}")
        case _ =>
      }
    }
  }

  /**
   *  A tree visitor to generate the corresponding node's positional embedding
   * @param ctx Context
   */
  case class genPostionVisitor(ctx:Context) extends TreeVisitor {
    override def process(node: Node): Unit = node match {

      // We initially set up positional embedding of a method as start point: [0.0]
      case _: MethodDeclaration =>
        val pos = List.fill(1)(0)
        ctx.addPositionalEmbedding(node, pos)
      case _ => node.genPosition(ctx)
    }
  }

  def getBytePairEncodingFromCompilation(cu: CompilationUnit) = {
    val identifier = "[_a-zA-Z][_a-zA-Z0-9]*"

    def get_max_pairs(tokens: List[String]) = {
      val left = tokens.drop(1).asInstanceOf[List[String]] :+ "NULL"
      val zipped_tokens = tokens.zip(left)
      val count = zipped_tokens.groupBy(identity).mapValues(_.size)
      val condidates = count.filter {
        case ((key1, key2), value) => {
          (key1.matches(s"${identifier}[.${identifier}]*") && key2 == ".") ||
//            (key1.matches(s"${identifier}[.${identifier}]*[.]") && key2.matches(s"${identifier}[.${identifier}]*[.]")) ||
            (key1.matches(s"${identifier}[.${identifier}]*[.]") && key2.matches(s"${identifier}[.${identifier}]*"))
        }
      }.toList.sortBy {
        case ((key1, key2), value) => tokens.indexOf(key1)
      }

      if (condidates.isEmpty)
        null
      else
        condidates.maxBy(_._2)
    }

    val code_tokens = cu.getTokenRange.get()
      .toList.filter(
      e => e.getText != " " && e.getText != "\n").map(_.getText)

    val mergedList = new ListBuffer[String]
    var scan = true
    var skip = false
    var nums_token = code_tokens.size

    while(scan) {
      val tokens = if (mergedList.isEmpty)
        code_tokens
      else {
        mergedList.toList
      }
      val max_pairs = get_max_pairs(tokens)
      if (max_pairs != null) {
        val ((key1, key2), value) = max_pairs
        if (value > 1) {
          mergedList.clear()
          for (i <- 0 until tokens.size - 1) {
            if (!skip) {
              if (tokens(i) == key1 && tokens(i + 1) == key2) {
                mergedList.append(key1 + key2)
                skip = true
              } else {
                mergedList.append(tokens(i))
              }
            } else {
              skip = false
            }
          }
        } else {
          scan = false
        }
      } else
        scan = false
    }

    val results = mergedList.filter(
//      ele => ele.split("\\.").size > 1 && ele.matches("^[a-zA-Z_.][a-zA-Z0-9_.]*")
      ele => ele.split("\\.").size > 1 && ele.matches(s"${identifier}[.${identifier}]*")
    ).distinct.map(ele =>
      if (ele.last == '.') ele.dropRight(1) else ele
    )

    results.toList
  }



  def getClassOrInterfaceDeclaration(cu:CompilationUnit): List[ClassOrInterfaceDeclaration] =
    cu.findAll(classOf[ClassOrInterfaceDeclaration])
      .stream()
      .collect(Collectors.toList[ClassOrInterfaceDeclaration]())
      .toList

  def getMethodCall(cu:CompilationUnit): List[MethodCallExpr] =
    cu.findAll(classOf[MethodCallExpr])
      .stream()
      .collect(Collectors.toList[MethodCallExpr]())
      .toList

  def getMethodDecl(cu:CompilationUnit): List[MethodDeclaration] =
    cu.findAll(classOf[MethodDeclaration])
      .stream()
      .collect(Collectors.toList[MethodDeclaration]())
      .toList

  def getMethodRef(cu:CompilationUnit): List[MethodReferenceExpr] =
    cu.findAll(classOf[MethodReferenceExpr])
      .stream()
      .collect(Collectors.toList[MethodReferenceExpr]())
      .toList

  def getTypes(cu:CompilationUnit): List[Type] =
    cu.findAll(classOf[Type])
      .stream()
      .collect(Collectors.toList[Type]())
      .toList

  def getSimpleName(cu:CompilationUnit): List[SimpleName] =
    cu.findAll(classOf[SimpleName])
      .stream()
      .collect(Collectors.toList[SimpleName]())
      .toList

  def getVariableDeclarator(cu:CompilationUnit): List[VariableDeclarator] =
    cu.findAll(classOf[VariableDeclarator])
      .stream()
      .collect(Collectors.toList[VariableDeclarator]())
      .toList

  def getAssignExpr(cu:CompilationUnit): List[AssignExpr] =
    cu.findAll(classOf[AssignExpr])
      .stream()
      .collect(Collectors.toList[AssignExpr]())
      .toList

  @deprecated
  def addPosition(ctx:Context, cu:CompilationUnit): Unit = addPositionVisitor(ctx).visitBreadthFirst(cu)

  def genAbstractCode(ctx:Context, cu:CompilationUnit): Unit = {
    cu.genCode(ctx)
  }

  def genPositionEmbedding(ctx:Context, cu:CompilationUnit): Unit = genPostionVisitor(ctx).visitBreadthFirst(cu)
}
