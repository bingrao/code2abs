package org.ucf.ml
package parser


import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
import com.github.javaparser.ast.visitor.{TreeVisitor, VoidVisitorAdapter}

import scala.collection.mutable.ListBuffer
import com.github.javaparser.ast.{Node, PackageDeclaration}
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.{MethodCallExpr, MethodReferenceExpr, SimpleName}

import scala.collection.JavaConversions._
import java.util.stream.Collectors

import com.github.javaparser.ast.stmt.ReturnStmt
import com.github.javaparser.symbolsolver.javaparsermodel.JavaParserFacade
import tree.EnrichedTrees

trait Visitor extends EnrichedTrees {


  case class TypeCalculatorVisitor() extends VoidVisitorAdapter[JavaParserFacade] {
    override def visit(n:ReturnStmt, arg:JavaParserFacade) = {
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

  case class MethodNameCollector() extends VoidVisitorAdapter[ListBuffer[String]] {
    override def visit(md:MethodDeclaration, c:ListBuffer[String]): Unit = {
      c.+=(md.getNameAsString)
      super.visit(md,c)
    }
  }

  @deprecated
  case class addPositionVisitor(ctx:Context) extends TreeVisitor {
    override def process(node: Node): Unit = {
      node match {
        case p: PackageDeclaration => {
          logger.info(f"${p.getName} -> ${p.getPosition(ctx)}")
        }
        case c: ClassOrInterfaceDeclaration => {
          logger.info(f"${c.getName} -> ${c.getPosition(ctx)}")
        }
        case m: MethodDeclaration => {
          logger.info(f"${m.getName} -> ${m.getPosition(ctx)}")
        }
        case _ =>{}
      }
    }
  }

  def getMethodCall(cu:CompilationUnit) =
    cu.findAll(classOf[MethodCallExpr])
      .stream()
      .collect(Collectors.toList[MethodCallExpr]())
      .toList

  def getMethodDecl(cu:CompilationUnit) =
    cu.findAll(classOf[MethodDeclaration])
      .stream()
      .collect(Collectors.toList[MethodDeclaration]())
      .toList

  def getMethodRef(cu:CompilationUnit) =
    cu.findAll(classOf[MethodReferenceExpr])
      .stream()
      .collect(Collectors.toList[MethodReferenceExpr]())
      .toList

  def getTypes(cu:CompilationUnit) =
    cu.findAll(classOf[Type])
      .stream()
      .collect(Collectors.toList[Type]())
      .toList

  def getSimpleName(cu:CompilationUnit) =
    cu.findAll(classOf[SimpleName])
      .stream()
      .collect(Collectors.toList[SimpleName]())
      .toList

  @deprecated
  def addPosition(ctx:Context, cu:CompilationUnit) = {
    val tree = addPositionVisitor(ctx)
    tree.visitBreadthFirst(cu)
  }

  def addPositionWithGenCode(ctx:Context, cu:CompilationUnit) = cu.genCode(ctx)
}
