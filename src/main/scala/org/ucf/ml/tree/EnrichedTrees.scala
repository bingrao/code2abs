package org.ucf.ml
package tree

import com.github.javaparser.ast.body._
import com.github.javaparser.ast._
import com.github.javaparser.ast.`type`._
import com.github.javaparser.ast.body.Parameter
import com.github.javaparser.ast.expr.{FieldAccessExpr, MethodCallExpr, Name, SimpleName}
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.body.VariableDeclarator
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.visitor.VoidVisitorAdapter

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

trait EnrichedTrees extends utils.Common {


  def getPath(src:Node, tgt:Node) = {

    def getPathRecursive(src:Node, tgt:Node,
                         up:ListBuffer[Node], down:ListBuffer[Node]):Unit = {


      val srcParent = src.getParentNode
      val tgtParent = tgt.getParentNode

      if (srcParent.isPresent) {
        up += srcParent.get()
        // tgt node first get the joint node.
        if (down.contains(srcParent.get())) return
      }

      if (tgtParent.isPresent) {
        down += tgtParent.get()
        // src node first get the joint node
        if (up.contains(tgtParent.get())) return
      }

      if ((!srcParent.isPresent) && (!tgtParent.isPresent)) return

      getPathRecursive(srcParent.get(), tgtParent.get(), up, down)
    }

    val srcUpList = new ListBuffer[Node]
    val tgtDownList = new ListBuffer[Node]
    srcUpList += src
    tgtDownList += tgt

    getPathRecursive(src, tgt, srcUpList, tgtDownList)

    val path = new StringBuilder

    val jointNode = if (srcUpList.contains(tgtDownList.last)) tgtDownList.last else srcUpList.last

    val srcJointIndex = srcUpList.indexOf(jointNode)
    val tgtJointIndex = tgtDownList.indexOf(jointNode)
    val srcPath = srcUpList.slice(0, srcJointIndex + 1).toList
    val tgtPath = tgtDownList.slice(0, tgtJointIndex).toList.reverse

    srcPath.foreach(n => {
      if (n == srcPath.last)
        path.append(s"[${n.getClass.getSimpleName}] -> ")
      else
        path.append(s"${n.getClass.getSimpleName} -> ")
    })

    tgtPath.foreach(n => {
      if (n == tgtPath.last)
        path.append(s"${n.getClass.getSimpleName}")
      else
        path.append(s"${n.getClass.getSimpleName} -> ")
    })

    path.toString()

  }

  implicit class genCompilationUnit(node:CompilationUnit) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {


      // 1. package declaration
      val package_decl = node.getPackageDeclaration
      if (package_decl.isPresent) package_decl.get().genCode(ctx, numsIntent)

      // 2. Import Statements
      node.getImports.foreach(impl => {
        impl.genCode(ctx, numsIntent)
        ctx.appendNewLine()
      })

      // 3. A list of defined types, such as Class, Interface, Enum, Annotation ...
      node.getTypes.foreach(typeDecl => typeDecl.genCode(ctx, numsIntent))

      logger.error(getPath(ctx.src, ctx.tgt))
    }
  }

  implicit class genPackageDeclaration(node: PackageDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("package") else "package")

      node.getName.genCode(ctx)
      ctx.append(";")
      ctx.appendNewLine()
    }
  }

  /**
   *  import java.io.*;
   *  import static java.io.file.out;
   * @param node
   */
  implicit class genImportDeclaration(node:ImportDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {

      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("import") else "import")

      if (node.isStatic) ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("static") else "static")

      node.getName.genCode(ctx, numsIntent)

      if (node.isAsterisk) {
        ctx.append(".")
        ctx.append("*")
      }
      ctx.append(";")
      ctx.appendNewLine()
    }
  }

  implicit class genTypeDeclaration(node:TypeDeclaration[_]) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      node match {
        /**
         *  TypeDeclaration
         *     -- EnumDeclaration
         *     -- AnnotationDeclaration
         *     -- ClassOrInterfaceDeclaration
         */
        case n: EnumDeclaration => {n.genCode(ctx, numsIntent)}
        case n: AnnotationDeclaration => {n.genCode(ctx, numsIntent)}
        case n: ClassOrInterfaceDeclaration => {n.genCode(ctx, numsIntent)}
      }
      
    }
  }

  implicit class genEnumDeclaration(node:EnumDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {

      val modifier = node.getModifiers
      modifier.foreach(_.genCode(ctx, numsIntent))

      node.getName.genCode(ctx, numsIntent)

      ctx.append("{")
      val entries = node.getEntries
      entries.foreach(entry => {
        entry.genCode(ctx, numsIntent)
        if (entry != entries.last) ctx.append(",")
      })
      ctx.append("}")
      ctx.appendNewLine()
    }
  }

  implicit class genAnnotationDeclaration(node:AnnotationDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      //TODO, No implementation about annotation
      ctx.append(node.toString, numsIntent)
    }
  }

  implicit class genClassOrInterfaceDeclaration(node:ClassOrInterfaceDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {

      if (ctx.getGranularity == CLASS) {
        // 1. Class Modifiers, such as public/private
        val modifiers = node.getModifiers
        modifiers.foreach(modifier => modifier.genCode(ctx, numsIntent))

        if (node.isInterface)
          ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("interface") else "interface")
        else
          ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("class") else "class")

        // 2. Interface/Class Name
        if (ctx.isAbstract)
          ctx.append(ctx.type_maps.getNewContent(node.getNameAsString))
        else
          node.getName.genCode(ctx)

        // 3. type parameters public interface Predicate<T> {}
        val tps = node.getTypeParameters
        tps.foreach(_.genCode(ctx, numsIntent))

        ctx.append("{")
        ctx.appendNewLine()
      }
      // 3. Class Members: Filed and method, constructor
      val members = node.getMembers
      members.foreach(bodyDecl => bodyDecl.genCode(ctx, numsIntent))

      if (ctx.getGranularity == CLASS) ctx.append("}")
      ctx.appendNewLine()
    }
  }

  implicit class genBodyDeclaration(node:BodyDeclaration[_]){
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      //TODO, only implment [[CallableDeclaration]] to handle with Method Declare
      node match {
        case n: InitializerDeclaration => {n.genCode(ctx, numsIntent)}
        case n: FieldDeclaration => {n.genCode(ctx, numsIntent)}
        case n: TypeDeclaration[_] => n.genCode(ctx, numsIntent)
        case n: EnumConstantDeclaration => {n.genCode(ctx, numsIntent)}
        case n: AnnotationMemberDeclaration => {n.genCode(ctx, numsIntent)}
        case n: CallableDeclaration[_] => n.genCode(ctx, numsIntent)
      }
    }
  }

  implicit class genInitializerDeclaration(node:InitializerDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      //TODO, no implmentation for learning bugs
      ctx.append(node.toString(), numsIntent)
    }
  }

  implicit class genFieldDeclaration(node:FieldDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {

      node.getModifiers.foreach(_.genCode(ctx, numsIntent))

      val varibles = node.getVariables
      varibles.foreach(ele => {
        if (ele == varibles.head) ele.getType.genCode(ctx)
        if (ctx.isAbstract) ctx.append(ctx.variable_maps.getNewContent(ele.getNameAsString)) else ele.getName.genCode(ctx)

        if (ele.getInitializer.isPresent){
          ctx.append("=")
          ele.getInitializer.get().genCode(ctx)
        }

        if (ele != node.getVariables.last) ctx.append(",")
      })
      ctx.append(";")
      ctx.appendNewLine()
    }
  }

  implicit class genEnumConstantDeclaration(node:EnumConstantDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      //TODO, no implmentation for learning bugs
      ctx.append(node.toString, numsIntent)
    }
  }

  implicit class genAnnotationMemberDeclaration(node:AnnotationMemberDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      //TODO, no implmentation for learning bugs
      ctx.append(node.toString, numsIntent)
    }
  }

  implicit class genCallableDeclaration(node:CallableDeclaration[_]){
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      node match {
        case n: ConstructorDeclaration => n.genCode(ctx, numsIntent)
        case n: MethodDeclaration => n.genCode(ctx, numsIntent)
      }
    }
  }

  implicit class genConstructorDeclaration(node:ConstructorDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
//      node.getAccessSpecifier
      node.getModifiers.foreach(_.genCode(ctx, numsIntent))

      node.getTypeParameters.foreach(_.genCode(ctx, numsIntent))

      /*Method name, such as hello*/
      if (ctx.isAbstract)
        ctx.append(ctx.method_maps.getNewContent(node.getNameAsString))
      else
        node.getName.genCode(ctx)

      /*formal paramters*/
      ctx.append("(")
      val parameters = node.getParameters
      parameters.foreach(p => {
        p.genCode(ctx, numsIntent)
        if (p != parameters.last) ctx.append(",")
      })
      ctx.append(")")

      val exceptions = node.getThrownExceptions
      if (exceptions.size() != 0) {
        if (ctx.isAbstract)
          ctx.append(ctx.ident_maps.getNewContent("throws"))
        else
          ctx.append("throws")
        node.getThrownExceptions.foreach(exp => {
          exp.genCode(ctx)
          if (exp != exceptions.last) ctx.append(",")
        })
      }
      node.getBody.genCode(ctx, numsIntent)
    }
  }

  implicit class genMethodDeclaration(node:MethodDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {

      /*modifiers, such as public*/
      val modifiers = node.getModifiers
      modifiers.foreach(modifier => modifier.genCode(ctx, numsIntent))

      /*Method return type, such as void, int, string*/
      node.getType.genCode(ctx, numsIntent)

      /*Method name, such as hello*/
      if (ctx.isAbstract)
        ctx.append(ctx.method_maps.getNewContent(node.getNameAsString))
      else
        node.getName.genCode(ctx)


      /*formal paramters*/
      ctx.append("(")
      val parameters = node.getParameters
      parameters.foreach(p => {
        p.genCode(ctx, numsIntent)
        if (p != parameters.last) ctx.append(",")
      })
      ctx.append(")")

      val exceptions = node.getThrownExceptions
      if (exceptions.size() != 0) {
        if (ctx.isAbstract)
          ctx.append(ctx.ident_maps.getNewContent("throws"))
        else
          ctx.append("throws")
        node.getThrownExceptions.foreach(exp => {
          exp.genCode(ctx)
          if (exp != exceptions.last) ctx.append(",")
        })
      }
      /*Method Body*/
      val body = node.getBody
      if (body.isPresent) body.get().genCode(ctx, numsIntent)
    }
  }


  /************************** Statement ***************************/

  implicit class genStatement(node:Statement) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      node match {
        case n:ForEachStmt => n.genCode(ctx, numsIntent)
        case n:LocalClassDeclarationStmt => n.genCode(ctx, numsIntent)
        case n:ContinueStmt => n.genCode(ctx, numsIntent)
        case n:ExpressionStmt => n.genCode(ctx, numsIntent)
        case n:LabeledStmt => n.genCode(ctx, numsIntent)
        case n:YieldStmt => n.genCode(ctx, numsIntent)
        case n:ReturnStmt => n.genCode(ctx, numsIntent)
        case n:WhileStmt => n.genCode(ctx, numsIntent)
        case n:EmptyStmt => n.genCode(ctx, numsIntent)
        case n:UnparsableStmt => n.genCode(ctx, numsIntent)
        case n:IfStmt => n.genCode(ctx, numsIntent)
        case n:BreakStmt => n.genCode(ctx, numsIntent)
        case n:AssertStmt => n.genCode(ctx, numsIntent)
        case n:ExplicitConstructorInvocationStmt => n.genCode(ctx, numsIntent)
        case n:DoStmt => n.genCode(ctx, numsIntent)
        case n:ForStmt => n.genCode(ctx, numsIntent)
        case n:ThrowStmt => n.genCode(ctx, numsIntent)
        case n:TryStmt => n.genCode(ctx, numsIntent)
        case n:SwitchStmt => n.genCode(ctx, numsIntent)
        case n:SynchronizedStmt => n.genCode(ctx, numsIntent)
        case n:BlockStmt => n.genCode(ctx, numsIntent)
      }

    }
  }

  implicit class genForEachStmt(node:ForEachStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val integral = node.getIterable
      val variable = node.getVariable
      val body = node.getBody

      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("for") else "for")
      ctx.append("(")
      variable.genCode(ctx)
      ctx.append(":")
      integral.genCode(ctx)
      ctx.append(")")
      body.genCode(ctx)
    }
  }

  implicit class genLocalClassDeclarationStmt(node:LocalClassDeclarationStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      node.getClassDeclaration.genCode(ctx)
    }
  }

  implicit class genContinueStmt(node:ContinueStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      val label =  node.getLabel
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("continue") else "continue")
      if (label.isPresent) label.get().genCode(ctx, numsIntent)
      ctx.append(";")
      ctx.appendNewLine()
    }
  }

  implicit class genExpressionStmt(node:ExpressionStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      node.getExpression.genCode(ctx, numsIntent)
      if (!node.getParentNode.get().isInstanceOf[Expression]) {
        ctx.append(";")
        ctx.appendNewLine()
      }
    }
  }

  implicit class genLabeledStmt(node:LabeledStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      val label = node.getLabel
      val sts = node.getStatement

      label.genCode(ctx, numsIntent)
      ctx.append(":")
      sts.genCode(ctx, numsIntent)

      ctx.append(";")
      ctx.appendNewLine()

    }
  }

  implicit class genYieldStmt(node:YieldStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("yield") else "yield")
      node.getExpression.genCode(ctx, numsIntent)
      ctx.append(";")
      ctx.appendNewLine()
    }
  }

  implicit class genReturnStmt(node:ReturnStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("return") else "return")
      val expr = node.getExpression
      if (expr.isPresent) expr.get().genCode(ctx, numsIntent)
      ctx.append(";")
      ctx.appendNewLine()
    }
  }

  implicit class genWhileStmt(node:WhileStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      val body = node.getBody
      val condition = node.getCondition
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("while") else "while")
      ctx.append("(")
      condition.genCode(ctx, numsIntent)
      ctx.append(")")
      ctx.appendNewLine()
      body.genCode(ctx, numsIntent)
    }
  }

  implicit class genEmptyStmt(node:EmptyStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      ctx.append(";")
      ctx.appendNewLine()
    }
  }

  implicit class genUnparsableStmt(node:UnparsableStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      //TODO, not for this project
      ctx.append(node.toString, numsIntent)
    }
  }

  implicit class genIfStmt(node:IfStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      val condition = node.getCondition
      val thenStmt = node.getThenStmt
      val elseStmt = node.getElseStmt

      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("if") else "if")
      ctx.append("(")
      condition.genCode(ctx, numsIntent)
      ctx.append(")")

      ctx.appendNewLine()
      thenStmt.genCode(ctx, numsIntent)

      if (elseStmt.isPresent){
        ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("else") else "else")
        ctx.appendNewLine()
        elseStmt.get().genCode(ctx, numsIntent)
      }
    }
  }

  implicit class genBreakStmt(node:BreakStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      val label = node.getLabel
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("break") else "break")
      if (label.isPresent) label.get().genCode(ctx, numsIntent)

      ctx.append(";")
      ctx.appendNewLine()
    }
  }

  implicit class genAssertStmt(node:AssertStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      val check = node.getCheck
      val msg = node.getMessage

      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("assert") else "assert")
      check.genCode(ctx, numsIntent)

      if (msg.isPresent) {
        ctx.append(":")
        msg.get().genCode(ctx, numsIntent)
      }

      ctx.append(";")
      ctx.appendNewLine()
    }
  }

  implicit class genExplicitConstructorInvocationStmt(node:ExplicitConstructorInvocationStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      //TODO, not for this project
      ctx.append(node.toString, numsIntent)
    }
  }

  implicit class genDoStmt(node:DoStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      val body = node.getBody
      val condition = node.getCondition

      body.genCode(ctx, numsIntent)
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("while") else "while")
      ctx.append("(")
      condition.genCode(ctx, numsIntent)
      ctx.append(")")

      ctx.append(";")
      ctx.appendNewLine()
    }
  }

  implicit class genForStmt(node:ForStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("for") else "for")
      ctx.append("(")
      val initial = node.getInitialization
      initial.foreach(init => {
        init.genCode(ctx, numsIntent)
        if (init != initial.last) ctx.append(",")
      })
      ctx.append(";")

      val compare = node.getCompare
      if (compare.isPresent) compare.get().genCode(ctx, numsIntent)

      ctx.append(";")

      val update = node.getUpdate
      update.foreach(up => {
        up.genCode(ctx, numsIntent)
        if (up != update.last) ctx.append(",")
      })
      ctx.append(")")

      val body = node.getBody
      body.genCode(ctx, numsIntent)
    }
  }

  implicit class genThrowStmt(node:ThrowStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("throw") else "throw")
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("new") else "new")
      node.getExpression.genCode(ctx)
      ctx.appendNewLine()
    }
  }

  implicit class genTryStmt(node:TryStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      //TODO not for this project
      val tryResources = node.getResources
      val tryCatch = node.getCatchClauses
      val tryFinally = node.getFinallyBlock
      val tryBlock = node.getTryBlock

      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("try") else "try")
      if (tryResources.size() != 0){
        ctx.append("(")
        tryResources.foreach(expr => {
          expr.genCode(ctx, numsIntent)
          if (expr != tryResources.last) ctx.append(",")
        })
        ctx.append(")")
      }

      tryBlock.genCode(ctx)

      tryCatch.foreach(_.genCode(ctx, numsIntent))

      if (tryFinally.isPresent) tryFinally.get().genCode(ctx, numsIntent)
    }
  }

  implicit class genCatchClause(node:CatchClause) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      val parameter = node.getParameter
      val body = node.getBody
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("catch") else "catch")
      ctx.append("(")
      parameter.genCode(ctx, numsIntent)
      ctx.append(")")
      body.genCode(ctx)
      ctx.appendNewLine()
    }
  }


  implicit class genSwitchStmt(node:SwitchStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {

      val entries = node.getEntries
      val selector = node.getSelector
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("switch") else "switch")

      ctx.append("(")
      selector.genCode(ctx, numsIntent)
      ctx.append(")")
      ctx.appendNewLine()

      ctx.append("{")
      entries.foreach(_.genCode(ctx))
      ctx.append("}")
      ctx.appendNewLine()
    }
  }

  implicit class genSynchronizedStmt(node:SynchronizedStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("synchronized") else "synchronized")
      ctx.append("(")
      node.getExpression.genCode(ctx, numsIntent)
      ctx.append(")")
      node.getBody.genCode(ctx)
    }
  }

  implicit class genBlockStmt(node:BlockStmt) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit  = {
      ctx.append("{")
      ctx.appendNewLine()
      node.getStatements.foreach(sts => sts.genCode(ctx, numsIntent))
      ctx.append("}")
      ctx.appendNewLine()
    }
  }


  /******************************* Expression ********************************/
  implicit class genExpression(node:Expression){
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      node match {
        case expr:ArrayAccessExpr  => expr.genCode(ctx, numsIntent)
        case expr:ClassExpr  => expr.genCode(ctx, numsIntent)
        case expr:LambdaExpr  => expr.genCode(ctx, numsIntent)
        case expr:ArrayCreationExpr  => expr.genCode(ctx, numsIntent)
        case expr:ConditionalExpr  => expr.genCode(ctx, numsIntent)
        case expr:MethodCallExpr  => expr.genCode(ctx, numsIntent)
        case expr:AnnotationExpr  => expr.genCode(ctx, numsIntent)
        case expr:AssignExpr  => expr.genCode(ctx, numsIntent)
        case expr:InstanceOfExpr  => expr.genCode(ctx, numsIntent)
        case expr:ThisExpr  => expr.genCode(ctx, numsIntent)
        case expr:NameExpr  => expr.genCode(ctx, numsIntent)
        case expr:CastExpr  => expr.genCode(ctx, numsIntent)
        case expr:MethodReferenceExpr  => expr.genCode(ctx, numsIntent)
        case expr:EnclosedExpr  => expr.genCode(ctx, numsIntent)
        case expr:VariableDeclarationExpr  => expr.genCode(ctx, numsIntent)
        case expr:SwitchExpr  => expr.genCode(ctx, numsIntent)
        case expr:LiteralExpr => expr.genCode(ctx, numsIntent)
        case expr:ObjectCreationExpr  => expr.genCode(ctx, numsIntent)
        case expr:SuperExpr  => expr.genCode(ctx, numsIntent)
        case expr:UnaryExpr  => expr.genCode(ctx, numsIntent)
        case expr:BinaryExpr  => expr.genCode(ctx, numsIntent)
        case expr:FieldAccessExpr  => expr.genCode(ctx, numsIntent)
        case expr:TypeExpr  => expr.genCode(ctx, numsIntent)
        case expr:ArrayInitializerExpr  => expr.genCode(ctx, numsIntent)
      }
    }
  }


  implicit class genArrayAccessExpr(node:ArrayAccessExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val name = node.getName
      val index = node.getIndex
      name.genCode(ctx, numsIntent)
      ctx.append("[")
      index.genCode(ctx, numsIntent)
      ctx.append("]")
    }
  }


  implicit class genClassExpr(node:ClassExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit= {

      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("Object") else "Object")
      ctx.append(".")
      node.getType.genCode(ctx)
      ctx.appendNewLine()
    }
  }


  implicit class genLambdaExpr(node:LambdaExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {

      val parameters = node.getParameters

      if (parameters.size > 1)
        ctx.append("(")

      parameters.foreach(p => {
        p.genCode(ctx, numsIntent)
        if (p != parameters.last) ctx.append(",")
      })

      if (parameters.size > 1)
        ctx.append(")")

      ctx.append("->")

      val body = node.getBody

      body.genCode(ctx, numsIntent)

    }
  }


  implicit class genArrayCreationExpr(node:ArrayCreationExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {

      val eleType = node.getElementType
      val initial = node.getInitializer
      val levels = node.getLevels

      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("new") else "new")
      eleType.genCode(ctx, numsIntent)
      for (level <- levels){
        ctx.append("[")
        val dim = level.getDimension
        if (dim.isPresent) dim.get().genCode(ctx, numsIntent)
        ctx.append("]")
      }

      if (initial.isPresent) initial.get().genCode(ctx, numsIntent)
    }
  }

  implicit class genConditionalExpr(node:ConditionalExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val condition = node.getCondition
      val thenExpr = node.getThenExpr
      val elseExpr = node.getElseExpr

      condition.genCode(ctx, numsIntent)
      ctx.append("?")
      thenExpr.genCode(ctx, numsIntent)
      ctx.append(":")
      elseExpr.genCode(ctx, numsIntent)
    }
  }

  implicit class genMethodCallExpr(node:MethodCallExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {

      val scope = node.getScope
      val arguments = node.getArguments

      if (scope.isPresent) {
        if (isScopeExpand(scope.get(), ctx)){
          scope.get().genCode(ctx, numsIntent)
        } else {
          if (ctx.isAbstract) {
            val scope_value = ctx.variable_maps.getNewContent(scope.get().toString)
            ctx.append(scope_value)
          } else
            scope.get().genCode(ctx, numsIntent)
        }
        ctx.append(".")
      }

      if (ctx.isAbstract) {
        val funcName = ctx.method_maps.getNewContent(node.getName.asString())
        ctx.append(funcName)
      } else node.getName.genCode(ctx)

      ctx.append("(")
      arguments.foreach(expr => {
        expr.genCode(ctx, numsIntent)
        if (expr != arguments.last) ctx.append(",")
      })
      ctx.append(")")
    }
  }

  implicit class genAnnotationExpr(node:AnnotationExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      //TODO, not for this projects
      ctx.append(node.toString)
    }
  }

  implicit class genAssignExpr(node:AssignExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit= {
      val left = node.getTarget
      val right = node.getValue
      val op = node.getOperator

      left.genCode(ctx, numsIntent)

      ctx.append(op.asString())

      right.genCode(ctx, numsIntent)


    }
  }

  implicit class genInstanceOfExpr(node:InstanceOfExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      node.getExpression.genCode(ctx)
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("instanceof") else "instanceof")
      node.getType.genCode(ctx)
    }
  }

  implicit class genThisExpr(node:ThisExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("this") else "this")
    }
  }

  implicit class genNameExpr(node:NameExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      if (ctx.isAbstract) {
        ctx.append(ctx.variable_maps.getNewContent(node.getName.asString()))
      } else node.getName.genCode(ctx)

    }
  }

  implicit class genCastExpr(node:CastExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      //TODO no implement for this project
      ctx.append("(")
      node.getType.genCode(ctx)
      ctx.append(")")
      ctx.append("(")
      node.getExpression.genCode(ctx)
      ctx.append(")")
    }
  }

  implicit class genMethodReferenceExpr(node:MethodReferenceExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val ident = node.getIdentifier
      val scope = node.getScope
      if (isScopeExpand(scope, ctx)) {
        scope.genCode(ctx, numsIntent)
      } else {
        if (ctx.isAbstract)
          ctx.append(ctx.ident_maps.getNewContent(scope.toString))
        else
          scope.genCode(ctx, numsIntent)
      }
      ctx.append("::")

      if (ctx.isAbstract) ctx.append(ctx.method_maps.getNewContent(ident)) else ctx.append(ident)
    }
  }

  implicit class genEnclosedExpr(node:EnclosedExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit= {
      ctx.append("(")
      node.getInner.genCode(ctx)
      ctx.append(")")
    }
  }

  implicit class genVariableDeclarationExpr(node:VariableDeclarationExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      node.getModifiers.foreach(_.genCode(ctx, numsIntent))
      val varibles = node.getVariables
      varibles.foreach(_.genCode(ctx, numsIntent))
    }
  }

  implicit class genSwitchExpr(node:SwitchExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("switch") else "switch")
      ctx.append("(")
      node.getSelector.genCode(ctx)
      ctx.append(")")
      ctx.append("{")

      node.getEntries.foreach(_.genCode(ctx))

      ctx.append("}")
      ctx.appendNewLine()
    }
  }

  implicit class genSwitchEntry(node:SwitchEntry) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      //TODO
      val lables = node.getLabels
      val sts = node.getStatements

      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("case") else "case")

      lables.foreach(expr => {
        expr.genCode(ctx)
        if (expr != lables.last) ctx.append(",")
      })

      ctx.append("->")

      ctx.append("{")
      sts.foreach(_.genCode(ctx))
      ctx.append("}")

    }
  }

  // subclass
  implicit class genLiteralExpr(node:LiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      node match {
        case expr:NullLiteralExpr => ctx.append(expr.toString)
        case expr:BooleanLiteralExpr => ctx.append(expr.getValue.toString)
        case expr:LiteralStringValueExpr  => expr.genCode(ctx, numsIntent)
      }
    }
  }

  implicit class genLiteralStringValueExpr(node:LiteralStringValueExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      node match {
        case expr: TextBlockLiteralExpr => expr.genCode(ctx, numsIntent)
        case expr: CharLiteralExpr => expr.genCode(ctx, numsIntent)
        case expr: DoubleLiteralExpr => expr.genCode(ctx, numsIntent)
        case expr: LongLiteralExpr => expr.genCode(ctx, numsIntent)
        case expr: StringLiteralExpr => expr.genCode(ctx, numsIntent)
        case expr: IntegerLiteralExpr => expr.genCode(ctx, numsIntent)
      }
    }
  }

  implicit class genTextBlockLiteralExpr(node:TextBlockLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val value = if (ctx.isAbstract) ctx.textBlock_maps.getNewContent(node.getValue) else node.asString()
      ctx.append(value)
    }
  }

  implicit class genCharLiteralExpr(node:CharLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val value = if (ctx.isAbstract) ctx.char_maps.getNewContent(node.getValue) else node.asChar().toString
      ctx.append(value)
    }
  }

  implicit class genDoubleLiteralExpr(node:DoubleLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val value = if (ctx.isAbstract)  ctx.double_maps.getNewContent(node.getValue) else node.asDouble().toString
      ctx.append(value)
    }
  }

  implicit class genLongLiteralExpr(node:LongLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val value = if (ctx.isAbstract)  ctx.long_maps.getNewContent(node.getValue) else node.asNumber().toString
      ctx.append(value)
    }
  }

  implicit class genStringLiteralExpr(node:StringLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val value = if (ctx.isAbstract)  ctx.string_maps.getNewContent(node.getValue) else node.asString()
      ctx.append("\"" + value + "\"")
    }
  }

  implicit class genIntegerLiteralExpr(node:IntegerLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val value = if (ctx.isAbstract)  ctx.int_maps.getNewContent(node.getValue) else node.asNumber().toString
      ctx.append(value)
    }
  }

  /**
   *  new B().new C();
   *  scope --> new B()
   *  type --> new C()
   * @param node
   */
  implicit class genObjectCreationExpr(node:ObjectCreationExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val arguments = node.getArguments
      val scope = node.getScope
      val tp = node.getType

      if (scope.isPresent) {
        if (isScopeExpand(scope.get(),ctx)){
          scope.get().genCode(ctx, numsIntent)
        } else {
          if (ctx.isAbstract)
            ctx.append(ctx.ident_maps.getNewContent(scope.toString))
          else
            scope.get().genCode(ctx, numsIntent)
        }
        ctx.append(".")
      }

      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("new") else "new")

      tp.genCode(ctx, numsIntent)

      ctx.append("(")
      arguments.foreach(expr =>{
        expr.genCode(ctx, numsIntent)
        if (expr != arguments.last) ctx.append(",")
      })
      ctx.append(")")
    }
  }


  implicit class genSuperExpr(node:SuperExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {

      val tpName = node.getTypeName

      if (tpName.isPresent){
        tpName.get().genCode(ctx)
        ctx.append(".")
      }
      ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("super") else "super")
    }
  }

  implicit class genUnaryExpr(node:UnaryExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val op = node.getOperator.asString()
      val expr = node.getExpression
      if (node.isPostfix) {
        expr.genCode(ctx, numsIntent)
        ctx.append(op)
      }
      if (node.isPrefix) {
        ctx.append(op)
        expr.genCode(ctx, numsIntent)
      }
    }
  }

  implicit class genBinaryExpr(node:BinaryExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val left = node.getLeft
      val op = node.getOperator.asString()
      val right = node.getRight
      left.genCode(ctx, numsIntent)
      ctx.append(op)
      right.genCode(ctx, numsIntent)
    }
  }

  implicit class genFieldAccessExpr(node:FieldAccessExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {

      if (isScopeExpand(node.getScope, ctx)){
        node.getScope.genCode(ctx, numsIntent)
      } else {
        if (ctx.isAbstract) {
          val scope_value = ctx.variable_maps.getNewContent(node.getScope.toString)
          ctx.append(scope_value)
        } else
          node.getScope.genCode(ctx, numsIntent)
      }
      ctx.append(".")

      // filed
      if (ctx.isAbstract) {
        val name = ctx.ident_maps.getNewContent(node.getName.asString())
        ctx.append(name)
      } else
        node.getName.genCode(ctx)
    }
  }

  implicit class genTypeExpr(node:TypeExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      node.getType.genCode(ctx, numsIntent)
    }
  }

  implicit class genArrayInitializerExpr(node:ArrayInitializerExpr) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val values = node.getValues
      ctx.append("{")
      values.foreach(ele => {
        ele.genCode(ctx, numsIntent)
        if (ele != values.last) ctx.append(",")
      })
      ctx.append("}")
    }
  }

  /******************************* Variable ********************************/
  implicit class genVariableDeclarator(node: VariableDeclarator) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val tp = node.getType
      val name = node.getName
      val init = node.getInitializer

      tp.genCode(ctx, numsIntent)

      if (ctx.isAbstract) {
        val value =  ctx.variable_maps.getNewContent(name.asString())
        ctx.append(value)
      } else name.genCode(ctx)


      if (init.isPresent){
        ctx.append("=")
        init.get().genCode(ctx, numsIntent)
      }
    }
  }

  /******************************* Node ************************/

  implicit class addPosition(node:Node) {
    def getPosition(ctx:Context, numsIntent:Int=0) = ctx.getNewPosition
  }

  implicit class genSimpleName(node:SimpleName) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      ctx.append(node.getIdentifier)
      val ident = node.getIdentifier
      if (ident == "String")  ctx.tgt = node.asInstanceOf[Node]
    }
  }

  implicit class genModifier(node: Modifier) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      ctx.append(node.getKeyword.asString())
    }
  }

  implicit class genType(node:Type) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = if (!node.asString().isEmpty){
      node match {
        case tp:UnionType  =>{
          ctx.append(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString())
        }
        case tp:VarType  =>{
          ctx.append(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString())
        }
        case tp:ReferenceType  => tp.genCode(ctx, numsIntent)
        case tp:UnknownType  => {
          ctx.append(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString())
        }
        case tp:PrimitiveType  =>{
          ctx.append(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString())
        }
        case tp:WildcardType  =>{
          ctx.append(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString())
        }
        case tp:VoidType  =>{
          ctx.append(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString())
        }
        case tp:IntersectionType  =>{
          ctx.append(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString())
        }
      }
    }
  }

  implicit class genReferenceType(node:ReferenceType) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      node match {
        case tp: ArrayType => tp.genCode(ctx, numsIntent)
        case tp: TypeParameter => tp.genCode(ctx, numsIntent)
        case tp: ClassOrInterfaceType => tp.genCode(ctx, numsIntent)
      }
    }
  }

  /**
   * So, int[][] becomes ArrayType(ArrayType(int)).
   * @param node
   */
  implicit class genArrayType(node:ArrayType) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      //TODO, need more details about
      val origin = node.getOrigin
      val comType = node.getComponentType
      comType.genCode(ctx, numsIntent)
      ctx.append("[")
      ctx.append("]")
    }
  }

  implicit class genTypeParameter(node:TypeParameter) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val name = node.getName
      val typeBound = node.getTypeBound

      ctx.append("<")

      if (ctx.isAbstract) ctx.append(ctx.type_maps.getNewContent(name.asString())) else name.genCode(ctx, numsIntent)
      if (typeBound.size() != 0){
        ctx.append(if (ctx.isAbstract) ctx.ident_maps.getNewContent("extends") else "extends")
        typeBound.foreach(bound => {
          bound.genCode(ctx, numsIntent)
          if (bound != typeBound.last) ctx.append("&")
        })
      }
      ctx.append(">")
    }
  }

  implicit class genClassOrInterfaceType(node:ClassOrInterfaceType) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val scope = node.getScope
      val name = node.getName
      val tps = node.getTypeArguments

      if (ctx.isAbstract) {
        val value = (if (scope.isPresent) scope.get().asString() + "." else EmptyString) + name.asString()
        ctx.append(ctx.type_maps.getNewContent(value))
      } else {
        if (scope.isPresent) {
          scope.get().genCode(ctx, numsIntent)
          ctx.append(".")
        }
        name.genCode(ctx, numsIntent)
      }

      if (tps.isPresent){
        ctx.append("<")
        tps.get().foreach(_.genCode(ctx, numsIntent))
        ctx.append(">")
      }
    }
  }


  implicit class genParameter(node:Parameter) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {
      val modifiers = node.getModifiers
      val tp = node.getType
      val name = node.getName

      modifiers.foreach(_.genCode(ctx))

      tp.genCode(ctx, numsIntent)

      if (ctx.isAbstract) {
        ctx.append(ctx.variable_maps.getNewContent(name.asString()))
      } else
        name.genCode(ctx, numsIntent)
    }
  }

  implicit class genName(node:Name) {
    def genCode(ctx:Context, numsIntent:Int=0):Unit = {

      //TODO with Qualifier
      val qualifier = node.getQualifier
      if (qualifier.isPresent){
        if (ctx.isAbstract)
          ctx.append(ctx.ident_maps.getNewContent(qualifier.get().asString()))
        else
          qualifier.get().genCode(ctx, numsIntent)
        ctx.append(".")
      }
      if (ctx.isAbstract) ctx.append(ctx.variable_maps.getNewContent(node.getIdentifier)) else ctx.append(node.getIdentifier)

      if (node.getIdentifier == "org")  ctx.src = node.asInstanceOf[Node]

    }
  }

  /**
   *  Collect all nodes have sub scope
   *  Usage: ScopeNodeCollector().visit(node, new ListBuffer[Node])
   */
  case class ScopeNodeCollector() extends VoidVisitorAdapter[ListBuffer[Node]] {
    private def hasScope(scope:Node):Boolean = scope match {
      case _:MethodCallExpr => true
      case _:MethodReferenceExpr => true
      case _:ObjectCreationExpr => true
      case _:FieldAccessExpr => true
      case _:ClassOrInterfaceType => true
      case _ => false
    }

    def visit(node:Node, c:ListBuffer[Node]): Unit = node match {
      case n:MethodCallExpr => this.visit(n, c)
      case n:MethodReferenceExpr => this.visit(n, c)
      case n:ClassOrInterfaceType => this.visit(n,c)
      case n:FieldAccessExpr => this.visit(n,c)
    }

    override def visit(n:MethodCallExpr, c:ListBuffer[Node]): Unit = {
      c.+=(n)
      if ((n.getScope.isPresent) && (!hasScope(n.getScope.get())))
        c.+=(n.getScope.get())
      super.visit(n,c)
    }
    override def visit(n:MethodReferenceExpr, c:ListBuffer[Node]): Unit = {
      c.+=(n)
      if (!hasScope(n.getScope))
        c.+=(n.getScope)
      super.visit(n,c)
    }
    override def visit(n:ClassOrInterfaceType, c:ListBuffer[Node]): Unit = {
      c.+=(n)
      if ((n.getScope.isPresent) && (!hasScope(n.getScope.get())))
        c.+=(n.getScope.get())
      super.visit(n,c)
    }
    override def visit(n:FieldAccessExpr, c:ListBuffer[Node]): Unit = {
      c.+=(n)
      if (!hasScope(n.getScope))
        c.+=(n.getScope)
      super.visit(n,c)
    }
  }

  def isScopeExpand(scope:Node, ctx:Context):Boolean = {
    val allPaths = new ListBuffer[Node]
    getAllScopePath(scope, allPaths)

//    if (logger.isDebugEnabled) {
    ////      val scope_name = getScopeNodeName(scope)
    ////      for (elem <- allPaths) {
    ////        val name = getScopeNodeName(elem)
    ////        logger.debug(f"${scope_name} - ${name} ############## ${elem.getClass}")
    ////      }
    ////    }

    /**
     * If scope is a format of method call, then we need to expand it,
     * such as a.b().c
     */
    if (allPaths.head.isInstanceOf[MethodCallExpr])
      return true

    /** If scope is a format of array access, then we need to expand it
     *  such as a[index].b
     * */
    if (allPaths.head.isInstanceOf[ArrayAccessExpr])
      return true

    /**
     * Along with scope data path, if exist one of nodes' name is already
     * defined, then we need to expand it.
     *
     * such as: a.b.c() --> a or b is defined
     */
    allPaths.toList.map(node => node match {
      case node:NameExpr => ctx.variable_maps.contain(node.getNameAsString)
      case node:MethodCallExpr => ctx.method_maps.contain(node.getNameAsString)
      case expr:MethodReferenceExpr => ctx.method_maps.contain(expr.getIdentifier)
      case node:ObjectCreationExpr => ctx.type_maps.contain(node.getType.asString())
      case node:FieldAccessExpr => ctx.ident_maps.contain(node.getNameAsString)
      case node:ClassOrInterfaceType => ctx.type_maps.contain(node.getNameAsString)
      case _ => false
    }).reduce(_ || _)
  }

  def getScopeNodeName(scope:Node):String = scope match {
    case expr:MethodCallExpr => expr.getNameAsString
    case expr:MethodReferenceExpr => expr.getIdentifier
    case expr:ObjectCreationExpr => expr.getType.getNameAsString
    case expr:FieldAccessExpr => expr.getNameAsString
    case expr:ClassOrInterfaceType => expr.getNameAsString
    case node:NameExpr => node.getNameAsString
    case _ => f"[Error]-${scope}"
  }


  def getAllScopePath(scope:Node, collector:ListBuffer[Node]):Unit = {
    collector.+=(scope)
    val node = scope match {
      case expr:MethodCallExpr => {
        if (expr.getScope.isPresent) expr.getScope.get() else null
      }
      case expr:MethodReferenceExpr => expr.getScope
      case expr:ObjectCreationExpr => {
        if (expr.getScope.isPresent) expr.getScope.get() else null
      }
      case expr:FieldAccessExpr => expr.getScope
      case expr:ClassOrInterfaceType => {
        if (expr.getScope.isPresent) expr.getScope.get() else null
      }
      case _ => null
    }
    if (node != null)
      getAllScopePath(node, collector)
  }
}
