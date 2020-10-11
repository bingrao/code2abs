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
import org.ucf.ml.utils.Context

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

trait EnrichedTrees extends utils.Common {
  
  /**
   *  Get Path between two node in a ast
   * @param src, source node
   * @param tgt, target node
   * @return
   */
  def getPath(src:Node, tgt:Node):String = {

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

    return EmptyString

    val srcUpList = new ListBuffer[Node]
    val tgtDownList = new ListBuffer[Node]
    srcUpList += src
    tgtDownList += tgt

    getPathRecursive(src, tgt, srcUpList, tgtDownList)

    val path = new StringBuilder

    val jointNode = if (srcUpList.contains(tgtDownList.last)) tgtDownList.last else srcUpList.last

    val srcJointIndex = srcUpList.indexOf(jointNode)
    val tgtJointIndex = tgtDownList.indexOf(jointNode)
    val srcPath = (if (srcJointIndex == 0) new ListBuffer[Node] else srcUpList.slice(0, srcJointIndex)).toList
    val tgtPath = tgtDownList.slice(0, tgtJointIndex + 1).toList.reverse


    srcPath.foreach(n => {
      path.append(s"${n.getClass.getSimpleName}").append(getUpArrow)
    })

    tgtPath.foreach(n => {
      if (n == tgtPath.head)
        path.append(s"[${n.getClass.getSimpleName}]")
      else
        path.append(s"${n.getClass.getSimpleName}")
      if (n != tgtPath.last) path.append(getDownArrow)
    })

    path.toString()

  }

  implicit class genCompilationUnit(node:CompilationUnit) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      // 1. package declaration
      val package_decl = node.getPackageDeclaration
      if (package_decl.isPresent) package_decl.get().genCode(ctx, numsIntent, tgt, 0)

      // 2. Import Statements
      node.getImports.foreach(impl => {
        impl.genCode(ctx, numsIntent, tgt, 0)
        
      })

      // 3. A list of defined types, such as Class, Interface, Enum, Annotation ...
      node.getTypes.foreach(_.genCode(ctx, numsIntent, tgt, 0))
    }
  }

  implicit class genPackageDeclaration(node: PackageDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("package", code_scope) else "package",
        position = node.genPosition(ctx))

      node.getName.genCode(ctx, numsIntent, tgt, code_scope)

      ctx.appendPosition(";", index = -1, parent = node)
      
    }
  }

  /**
   *  import java.io.*;
   *  import static java.io.file.out;
   * @param node
   */
  implicit class genImportDeclaration(node:ImportDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("import", code_scope) else "import",
        position = node.genPosition(ctx))

      if (node.isStatic) ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("static", code_scope) else "static",
        index = 0, parent = node)

      node.getName.genCode(ctx, numsIntent, tgt, code_scope)

      if (node.isAsterisk) {
        ctx.appendPosition(".", index = 1, parent = node)
        ctx.appendPosition("*", index = 2, parent = node)
      }
      ctx.appendPosition(";", index = -1, parent = node)
      
    }
  }

  implicit class genTypeDeclaration(node:TypeDeclaration[_]) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      node match {
        /**
         * TypeDeclaration
         * -- EnumDeclaration
         * -- AnnotationDeclaration
         * -- ClassOrInterfaceDeclaration
         */
        case n: EnumDeclaration => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n: AnnotationDeclaration => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n: ClassOrInterfaceDeclaration => n.genCode(ctx, numsIntent, tgt, code_scope)
      }
    }
  }

  implicit class genEnumDeclaration(node:EnumDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      val modifier = node.getModifiers
      modifier.foreach(_.genCode(ctx, numsIntent, tgt, code_scope))

      if (ctx.isAbstract)
        ctx.appendPosition(ctx.type_maps.getNewContent(node.getNameAsString, code_scope),
          position = node.getName.genPosition(ctx))
      else
        node.getName.genCode(ctx, numsIntent, tgt, code_scope)

      ctx.appendPosition("{", index = 0, parent = node)
      val entries = node.getEntries

      for (i <- 0 until(entries.size())){
        entries.get(i).genCode(ctx, numsIntent, tgt, code_scope + 1)
        if (i < entries.size() - 1)
          ctx.appendPosition(",", index = 1, parent = node)
      }
      ctx.appendPosition("}", index = -1, parent = node)
      
    }
  }

  implicit class genAnnotationDeclaration(node:AnnotationDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      //TODO, No implementation about annotation
      ctx.appendPosition(node.toString, numsIntent, position = node.genPosition(ctx))
    }
  }

  implicit class genClassOrInterfaceDeclaration(node:ClassOrInterfaceDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      val local_class = node.getParentNode.isPresent && node.getParentNode.get().isInstanceOf[LocalClassDeclarationStmt]

      if ((ctx.getGranularity == CLASS) || (!ctx.isAbstract) || local_class) {
        // 1. Class Modifiers, such as public/private
        val modifiers = node.getModifiers
        modifiers.foreach(_.genCode(ctx, numsIntent, tgt, code_scope))

        if (node.isInterface)
          ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("interface", code_scope) else "interface",
            index = 0, parent = node)
        else
          ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("class", code_scope) else "class",
            index = 0, parent = node)

        // 2. Interface/Class Name
        if (ctx.isAbstract)
          ctx.appendPosition(ctx.type_maps.getNewContent(node.getNameAsString, code_scope), position = node.getName.genPosition(ctx))
        else
          node.getName.genCode(ctx, numsIntent, tgt, code_scope)

        // 3. type parameters public interface Predicate<T> {}
        val tps = node.getTypeParameters
        tps.foreach(_.genCode(ctx, numsIntent, tgt, code_scope))

        ctx.appendPosition("{", index = 1, parent = node)
        
      }
      // 3. Class Members: Filed and method, constructor
      val members = node.getMembers
      members.foreach(_.genCode(ctx, numsIntent, tgt, code_scope + 1))

      if ((ctx.getGranularity == CLASS) || (!ctx.isAbstract) || local_class)
        ctx.appendPosition("}", index = -1, parent = node)
      
    }
  }

  implicit class genBodyDeclaration(node:BodyDeclaration[_]){
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      //TODO, only implment [[CallableDeclaration]] to handle with Method Declare
      node match {
        case n: InitializerDeclaration => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n: FieldDeclaration => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n: TypeDeclaration[_] => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n: EnumConstantDeclaration => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n: AnnotationMemberDeclaration => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n: CallableDeclaration[_] => n.genCode(ctx, numsIntent, tgt, code_scope)
      }
    }
  }

  implicit class genInitializerDeclaration(node:InitializerDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      //TODO, no implmentation for learning bugs
      if (node.isStatic) {
        if (ctx.isAbstract)
          ctx.appendPosition(ctx.ident_maps.getNewContent("static", code_scope), index = -2, parent = node)
        else
          ctx.appendPosition("static", index = -1, parent = node)
      }

      node.getBody.genCode(ctx, numsIntent, node, code_scope + 1)

//      ctx.appendPosition(node.toString(), numsIntent, position = node.genPosition(ctx))
    }
  }

  implicit class genFieldDeclaration(node:FieldDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      if (tgt != null) logger.debug(getPath(node, tgt))

      node.getModifiers.foreach(_.genCode(ctx, numsIntent, node, code_scope))

      val varibles = node.getVariables

      for (i <- 0 until(varibles.size())){
        val ele = varibles.get(i)

        if (i == 0) ele.getType.genCode(ctx, numsIntent, node, code_scope)
        if (ctx.isAbstract)
          ctx.appendPosition(ctx.variable_maps.getNewContent(ele.getNameAsString, code_scope),
            position = ele.getName.genPosition(ctx))
        else
          ele.getName.genCode(ctx, numsIntent, node, code_scope)

//        println(s"########### variable paraent: [${code_scope}]-${node}")


        if (ele.getInitializer.isPresent){
          ctx.appendPosition("=", index = 0, parent = node)
          ele.getInitializer.get().genCode(ctx, numsIntent, node, code_scope)
        }

        if (i < varibles.size() - 1)
          ctx.appendPosition(",", index = 1, parent = node)
      }
      ctx.appendPosition(";", index = -1, parent = node)
      
    }
  }

  implicit class genEnumConstantDeclaration(node:EnumConstantDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      //TODO, no implmentation for learning bugs
      ctx.appendPosition(node.toString, numsIntent, position = node.genPosition(ctx))
    }
  }

  implicit class genAnnotationMemberDeclaration(node:AnnotationMemberDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      //TODO, no implmentation for learning bugs
      ctx.appendPosition(node.toString, numsIntent, position = node.genPosition(ctx))
    }
  }

  implicit class genCallableDeclaration(node:CallableDeclaration[_]){
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      node match {
        case n: ConstructorDeclaration => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n: MethodDeclaration => n.genCode(ctx, numsIntent, tgt, code_scope)
      }
    }
  }

  implicit class genConstructorDeclaration(node:ConstructorDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
//      node.getAccessSpecifier
      node.getModifiers.foreach(_.genCode(ctx, numsIntent, tgt, code_scope))

      node.getTypeParameters.foreach(_.genCode(ctx, numsIntent, tgt, code_scope))

      /*Method name, such as hello*/
      if (ctx.isAbstract)
        ctx.appendPosition(ctx.method_maps.getNewContent(node.getNameAsString, code_scope), position = node.getName.genPosition(ctx))
      else
        node.getName.genCode(ctx, numsIntent, tgt, code_scope)

      /*formal paramters*/
      ctx.appendPosition("(", index = 0, parent = node)
      val parameters = node.getParameters

      for (i <- 0 until(parameters.size())){
        parameters.get(i).genCode(ctx, numsIntent, tgt, code_scope)
        if (i < parameters.size() - 1)
          ctx.appendPosition(",", index = 1, parent = node)
      }

      ctx.appendPosition(")", index = 2, parent = node)

      val exceptions = node.getThrownExceptions
      if (exceptions.size() != 0) {
        if (ctx.isAbstract)
          ctx.appendPosition(ctx.ident_maps.getNewContent("throws", code_scope), index = -2, parent = node)
        else
          ctx.appendPosition("throws", index = -1, parent = node)

        for (i <- 0 until(node.getThrownExceptions.size())){
          node.getThrownExceptions.get(i).genCode(ctx, numsIntent, tgt, code_scope)
          if (i < node.getThrownExceptions.size() - 1)
            ctx.appendPosition(",", index = -1, parent = node)
        }

      }
      node.getBody.genCode(ctx, numsIntent, tgt, code_scope + 1)
    }
  }

  implicit class genMethodDeclaration(node:MethodDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      /*modifiers, such as public*/
      val modifiers = node.getModifiers
      modifiers.foreach(modifier => modifier.genCode(ctx, numsIntent, node.getName, code_scope))

      /*Method return type, such as void, int, string*/
      node.getType.genCode(ctx, numsIntent, node.getName, code_scope)

      /*Method name, such as hello*/
      if (ctx.isAbstract)
        ctx.appendPosition(ctx.method_maps.getNewContent(node.getNameAsString, code_scope), position = node.getName.genPosition(ctx))
      else
        node.getName.genCode(ctx, numsIntent, node.getName, code_scope)

      /*formal paramters*/
      ctx.appendPosition("(", index = 0, parent = node)
      val parameters = node.getParameters

      for (i <- 0 until(parameters.size())){
        parameters.get(i).genCode(ctx, numsIntent, node.getName, code_scope + 1)
        if (i < parameters.size() - 1)
          ctx.appendPosition(",", index = 1, parent = node)
      }

      ctx.appendPosition(")", index = 2, parent = node)

      val exceptions = node.getThrownExceptions
      if (exceptions.size() != 0) {
        if (ctx.isAbstract)
          ctx.appendPosition(ctx.ident_maps.getNewContent("throws", code_scope), index = -3, parent = node)
        else
          ctx.appendPosition("throws", index = -3, parent = node)

        for (i <- 0 until(node.getThrownExceptions.size())){
          node.getThrownExceptions.get(i).genCode(ctx, numsIntent, node.getName, code_scope)
          if (i < node.getThrownExceptions.size() - 1)
            ctx.appendPosition(",", index = -2, parent = node)
        }

      }
      /*Method Body*/
      val body = node.getBody
      if (body.isPresent)
        body.get().genCode(ctx, numsIntent, node.getName, code_scope + 1)
      else
        ctx.appendPosition(";", index = -1, parent = node)
    }
  }


  /************************** Statement ***************************/

  implicit class genStatement(node:Statement) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      node match {
        case n:ForEachStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:LocalClassDeclarationStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:ContinueStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:ExpressionStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:LabeledStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:YieldStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:ReturnStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:WhileStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:EmptyStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:UnparsableStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:IfStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:BreakStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:AssertStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:ExplicitConstructorInvocationStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:DoStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:ForStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:ThrowStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:TryStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:SwitchStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:SynchronizedStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
        case n:BlockStmt => n.genCode(ctx, numsIntent, tgt, code_scope)
      }
    }
  }

  implicit class genForEachStmt(node:ForEachStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      if (tgt != null) logger.debug(f"for" + getUpArrow + getPath(node, tgt))
      val integral = node.getIterable
      val variable = node.getVariable
      val body = node.getBody

      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("for", code_scope) else "for", position = node.genPosition(ctx))
      ctx.appendPosition("(", index = 0, parent = node)
      variable.genCode(ctx, numsIntent, node, code_scope + 1)
      ctx.appendPosition(":", index = 1, parent = node)
      integral.genCode(ctx, numsIntent, node, code_scope + 1)
      ctx.appendPosition(")", index = 2, parent = node)
      body.genCode(ctx, numsIntent, node, code_scope + 1)
    }
  }

  implicit class genLocalClassDeclarationStmt(node:LocalClassDeclarationStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(getPath(node, tgt))

      node.getClassDeclaration.genCode(ctx, numsIntent, node, code_scope + 1)
    }
  }

  implicit class genContinueStmt(node:ContinueStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"continue" + getUpArrow + getPath(node, tgt))
      val label =  node.getLabel
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("continue", code_scope) else "continue", position = node.genPosition(ctx))
      if (label.isPresent) {
        if (ctx.isAbstract)
          ctx.appendPosition(ctx.variable_maps.getNewContent(label.get().getIdentifier, code_scope))
        else
          label.get().genCode(ctx, numsIntent, node, code_scope)
      }
      ctx.appendPosition(";", index = -1, parent = node)
      
    }
  }

  implicit class genExpressionStmt(node:ExpressionStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"[${node.toString}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
      node.getExpression.genCode(ctx, numsIntent, node, code_scope)
      if (!node.getParentNode.get().isInstanceOf[Expression]) {
        ctx.appendPosition(";", index = -1, parent = node)
        
      }
    }
  }

  implicit class genLabeledStmt(node:LabeledStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"Label" + getUpArrow + getPath(node, tgt))
      val label = node.getLabel
      val sts = node.getStatement

      if (ctx.isAbstract)
        ctx.appendPosition(ctx.variable_maps.getNewContent(label.getIdentifier, code_scope))
      else
        label.genCode(ctx, numsIntent, node, code_scope)


      ctx.appendPosition(":", index = 0, parent = node)
      sts.genCode(ctx, numsIntent, node, code_scope)

      ctx.appendPosition(";", index = -1, parent = node)
      

    }
  }

  implicit class genYieldStmt(node:YieldStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"yield" + getUpArrow + getPath(node, tgt))
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("yield", code_scope) else "yield", position = node.genPosition(ctx))
      node.getExpression.genCode(ctx, numsIntent, node, code_scope)
      ctx.appendPosition(";", index = -1, parent = node)
      
    }
  }

  implicit class genReturnStmt(node:ReturnStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"return" + getUpArrow + getPath(node, tgt))
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("return", code_scope) else "return", position = node.genPosition(ctx))
      val expr = node.getExpression
      if (expr.isPresent) expr.get().genCode(ctx, numsIntent, node, code_scope)
      ctx.appendPosition(";", index = -1, parent = node)
      
    }
  }

  implicit class genWhileStmt(node:WhileStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"while" + getUpArrow + getPath(node, tgt))
      val body = node.getBody
      val condition = node.getCondition

      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("while", code_scope) else "while", position = node.genPosition(ctx))
      ctx.appendPosition("(", index = 0, parent = node)
      condition.genCode(ctx, numsIntent, node, code_scope + 1)
      ctx.appendPosition(")", index = 1, parent = node)
      

      if (! body.isInstanceOf[BlockStmt]) ctx.appendPosition("{", index = 0, parent = node)
      body.genCode(ctx, numsIntent, node, code_scope + 1)
      if (! body.isInstanceOf[BlockStmt]) ctx.appendPosition("}", index = 0, parent = node)
    }
  }

  implicit class genEmptyStmt(node:EmptyStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"empty" + getUpArrow + getPath(node, tgt))
      ctx.appendPosition(";", index = -1, parent = node)
      
    }
  }

  implicit class genUnparsableStmt(node:UnparsableStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      //TODO, not for this project
      ctx.appendPosition(node.toString, numsIntent, position = node.genPosition(ctx))
    }
  }

  implicit class genIfStmt(node:IfStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0): Unit  = {
      if (tgt != null) logger.debug(f"if" + getUpArrow + getPath(node, tgt))
      val condition = node.getCondition
      val thenStmt = node.getThenStmt
      val elseStmt = node.getElseStmt

      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("if", code_scope) else "if", position = node.genPosition(ctx))
      ctx.appendPosition("(", index = 0, parent = node)
      condition.genCode(ctx, numsIntent, node, code_scope + 1)
      ctx.appendPosition(")", index = 1, parent = node)


      if (!thenStmt.isInstanceOf[BlockStmt])
        ctx.appendPosition("{", index = 0, parent = node)
      
      thenStmt.genCode(ctx, numsIntent, node, code_scope + 1)
      if (!thenStmt.isInstanceOf[BlockStmt])
        ctx.appendPosition("}", index = 0, parent = node)

      if (elseStmt.isPresent){
        ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("else", code_scope) else "else", position = elseStmt.get().genPosition(ctx))
        if (!elseStmt.get().isInstanceOf[BlockStmt]) ctx.appendPosition("{", index = 0, parent = node)
        
        elseStmt.get().genCode(ctx, numsIntent, node, code_scope + 1)
        if (!elseStmt.get().isInstanceOf[BlockStmt]) ctx.appendPosition("}", index = 0, parent = node)
      }
    }
  }

  implicit class genBreakStmt(node:BreakStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"break" + getUpArrow + getPath(node, tgt))
      val label = node.getLabel
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("break", code_scope) else "break", position = node.genPosition(ctx))
      if (label.isPresent) {

        if (ctx.isAbstract)
          ctx.appendPosition(ctx.variable_maps.getNewContent(label.get().getIdentifier, code_scope))
        else
          label.get().genCode(ctx, numsIntent, node, code_scope)
      }

      ctx.appendPosition(";", index = -1, parent = node)
      
    }
  }

  implicit class genAssertStmt(node:AssertStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"assert" + getUpArrow + getPath(node, tgt))
      val check = node.getCheck
      val msg = node.getMessage

      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("assert", code_scope) else "assert", position = node.genPosition(ctx))
      check.genCode(ctx, numsIntent, node, code_scope)

      if (msg.isPresent) {
        ctx.appendPosition(":", index = 0, parent = node)
        msg.get().genCode(ctx, numsIntent, node, code_scope)
      }

      ctx.appendPosition(";", index = -1, parent = node)
      
    }
  }

  implicit class genExplicitConstructorInvocationStmt(node:ExplicitConstructorInvocationStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      //TODO, not for this project
      ctx.appendPosition(node.toString, numsIntent, parent = node)
    }
  }

  implicit class genDoStmt(node:DoStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"do-while" + getUpArrow + getPath(node, tgt))
      val body = node.getBody
      val condition = node.getCondition

      body.genCode(ctx, numsIntent, node, code_scope + 1)
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("while", code_scope) else "while", position = node.genPosition(ctx))
      ctx.appendPosition("(", index = 0, parent = node)
      condition.genCode(ctx, numsIntent, node, code_scope +1)
      ctx.appendPosition(")", index = 1, parent = node)

      ctx.appendPosition(";", index = -1, parent = node)
      
    }
  }

  implicit class genForStmt(node:ForStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"for" + getUpArrow + getPath(node, tgt))
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("for", code_scope) else "for", position = node.genPosition(ctx))
      ctx.appendPosition("(", index = 0, parent = node)
      val initial = node.getInitialization

      for (i <- 0 until(initial.size())){
        initial.get(i).genCode(ctx, numsIntent, node, code_scope + 1)
        if (i < initial.size() - 1)
          ctx.appendPosition(",", index = 1, parent = node)
      }

      ctx.appendPosition(";", index = 2, parent = node)

      val compare = node.getCompare
      if (compare.isPresent) compare.get().genCode(ctx, numsIntent, node, code_scope + 1)

      ctx.appendPosition(";", index = -3, parent = node)

      val update = node.getUpdate

      for (i <- 0 until(update.size())){
        update.get(i).genCode(ctx, numsIntent, node, code_scope +1)
        if (i < update.size() - 1)
          ctx.appendPosition(",", index = -2, parent = node)
      }


      ctx.appendPosition(")", index = -1, parent = node)

      val body = node.getBody
      if (! body.isInstanceOf[BlockStmt]) ctx.appendPosition("{", index = 0, parent = node)
      body.genCode(ctx, numsIntent, node, code_scope + 1)
      if (! body.isInstanceOf[BlockStmt]) ctx.appendPosition("}", index = 0, parent = node)
    }
  }

  implicit class genThrowStmt(node:ThrowStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"throw" + getUpArrow + getPath(node, tgt))

      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("throw", code_scope) else "throw", position = node.genPosition(ctx))
      node.getExpression.genCode(ctx, numsIntent, node, code_scope)

      if (node.getParentNode.isPresent && (node.getParentNode.get().isInstanceOf[MethodDeclaration] == false))
        ctx.appendPosition(";", index = 0, parent = node)

      
    }
  }

  implicit class genTryStmt(node:TryStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"try" + getUpArrow + getPath(node, tgt))

      val tryResources = node.getResources
      val tryCatch = node.getCatchClauses
      val tryFinally = node.getFinallyBlock
      val tryBlock = node.getTryBlock

      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("try", code_scope) else "try", position = node.genPosition(ctx))
      if (tryResources.size() != 0){
        ctx.appendPosition("(", index = 0, parent = node)

        for (i <- 0 until(tryResources.size())){
          tryResources.get(i).genCode(ctx, numsIntent, node, code_scope + 1)
          if (i < tryResources.size() - 1)
            ctx.appendPosition(";", index = 1, parent = node)
        }

        ctx.appendPosition(")", index = 2, parent = node)
      }

      tryBlock.genCode(ctx, numsIntent, node, code_scope + 1)

      tryCatch.foreach(_.genCode(ctx, numsIntent, node, code_scope + 1))

      if (tryFinally.isPresent) {
        ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("finally", code_scope) else "finally", position = node.genPosition(ctx))
        tryFinally.get().genCode(ctx, numsIntent, node, code_scope + 1)
      }
    }
  }

  implicit class genCatchClause(node:CatchClause) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      val parameter = node.getParameter
      val body = node.getBody
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("catch", code_scope) else "catch", position = node.genPosition(ctx))
      ctx.appendPosition("(", index = 0, parent = node)
      parameter.genCode(ctx, numsIntent, node, code_scope)
      ctx.appendPosition(")", index = 1, parent = node)
      body.genCode(ctx, numsIntent, tgt, code_scope + 1)
      
    }
  }


  implicit class genSwitchStmt(node:SwitchStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"switch" + getUpArrow + getPath(node, tgt))
      val entries = node.getEntries
      val selector = node.getSelector
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("switch", code_scope) else "switch", position = node.genPosition(ctx))

      ctx.appendPosition("(", index = 0, parent = node)
      selector.genCode(ctx, numsIntent, node, code_scope + 1)
      ctx.appendPosition(")", index = 1, parent = node)
      

      ctx.appendPosition("{", index = -3, parent = node)
      entries.foreach(_.genCode(ctx, numsIntent, node, code_scope + 1))
      ctx.appendPosition("}", index = -2, parent = node)
      
    }
  }

  implicit class genSynchronizedStmt(node:SynchronizedStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {
      if (tgt != null) logger.debug(f"synchronized" + getUpArrow + getPath(node, tgt))
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("synchronized", code_scope) else "synchronized", position = node.genPosition(ctx))
      ctx.appendPosition("(", index = 0, parent = node)
      node.getExpression.genCode(ctx, numsIntent, node, code_scope)
      ctx.appendPosition(")", index = 1, parent = node)
      node.getBody.genCode(ctx, numsIntent, node, code_scope)
    }
  }

  implicit class genBlockStmt(node:BlockStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit  = {

      ctx.appendPosition("{", index = 0, parent = node)
      
      node.getStatements.foreach(sts => sts.genCode(ctx, numsIntent, node, code_scope))
      ctx.appendPosition("}", index = 1, parent = node)
      
    }
  }


  /******************************* Expression ********************************/
  implicit class genExpression(node:Expression){
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      node match {
        case expr:ArrayAccessExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:ClassExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:LambdaExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:ArrayCreationExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:ConditionalExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:MethodCallExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:AnnotationExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:AssignExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:InstanceOfExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:ThisExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:NameExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:CastExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:MethodReferenceExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:EnclosedExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:VariableDeclarationExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:SwitchExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:LiteralExpr => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:ObjectCreationExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:SuperExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:UnaryExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:BinaryExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:FieldAccessExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:TypeExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr:ArrayInitializerExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
      }
    }
  }


  implicit class genArrayAccessExpr(node:ArrayAccessExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val name = node.getName
      val index = node.getIndex
      name.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition("[", index = 0, parent = node)
      index.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition("]", index = 1, parent = node)
    }
  }


  implicit class genClassExpr(node:ClassExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit= {

      node.getType.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition(".", index = 0, parent = node)
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("class", code_scope) else "class", position = node.genPosition(ctx))
      
    }
  }


  implicit class genLambdaExpr(node:LambdaExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      val parameters = node.getParameters

      ctx.appendPosition("(", index = 0, parent = node)

      for (i <- 0 until(parameters.size())){
        parameters.get(i).genCode(ctx, numsIntent, tgt, code_scope + 1)
        if (i < parameters.size() - 1)
          ctx.appendPosition(",", index = 1, parent = node)
      }

      ctx.appendPosition(")", index = 2, parent = node)


      ctx.appendPosition("->", index = -3, parent = node)

      val body = node.getBody

      body.genCode(ctx, numsIntent, tgt, code_scope + 1)

    }
  }


  implicit class genArrayCreationExpr(node:ArrayCreationExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      val eleType = node.getElementType
      val initial = node.getInitializer
      val levels = node.getLevels

      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("new", code_scope) else "new", position = node.genPosition(ctx))
      eleType.genCode(ctx, numsIntent, tgt, code_scope)
      for (level <- levels){
        ctx.appendPosition("[", index = 0, parent = node)
        val dim = level.getDimension
        if (dim.isPresent) dim.get().genCode(ctx, numsIntent, tgt, code_scope)
        ctx.appendPosition("]", index = 1, parent = node)
      }

      if (initial.isPresent) initial.get().genCode(ctx, numsIntent, tgt, code_scope)
    }
  }

  implicit class genConditionalExpr(node:ConditionalExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val condition = node.getCondition
      val thenExpr = node.getThenExpr
      val elseExpr = node.getElseExpr

      condition.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition("?", index = 0, parent = node)
      thenExpr.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition(":", index = 1, parent = node)
      elseExpr.genCode(ctx, numsIntent, tgt, code_scope)
    }
  }

  implicit class genMethodCallExpr(node:MethodCallExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      val scope = node.getScope
      val arguments = node.getArguments

      if (scope.isPresent) {
        val full_name = scope.get().toString + "." + node.getName.asString()
        if (ctx.ident_maps.getIdioms.contains(full_name) && ctx.isAbstract)
          ctx.appendPosition(full_name, index = 0, parent = node)
        else {
          if (isScopeExpand(scope.get(), ctx)) {
            scope.get().genCode(ctx, numsIntent, tgt, code_scope)
          } else {
            if (ctx.isAbstract) {
              val scope_value = if (ctx.variable_maps.contain(scope.get().toString, code_scope))
                ctx.variable_maps.get_match_content(scope.get().toString, code_scope)
              else
                ctx.ident_maps.getNewContent(scope.get().toString, code_scope)
              ctx.appendPosition(scope_value, position = scope.get().genPosition(ctx))
            } else
              scope.get().genCode(ctx, numsIntent, tgt, code_scope)
          }
          ctx.appendPosition(".", index = 0, parent = node)

          if (ctx.isAbstract) {
            val funcName = ctx.method_maps.getNewContent(node.getName.asString(), code_scope)
            ctx.appendPosition(funcName, position = node.getName.genPosition(ctx))
          } else
            node.getName.genCode(ctx, numsIntent, tgt, code_scope)
        }
      } else {
        if (ctx.isAbstract) {
          val funcName = ctx.method_maps.getNewContent(node.getName.asString(), code_scope)
          ctx.appendPosition(funcName, position = node.getName.genPosition(ctx))
        } else
          node.getName.genCode(ctx, numsIntent, tgt, code_scope)
      }



      ctx.appendPosition("(", index = 0, parent = node)

      for(i <- 0 until arguments.size()){
        arguments.get(i).genCode(ctx, numsIntent, tgt, code_scope)
        if (i < arguments.size() - 1 )
          ctx.appendPosition(",", index = 1, parent = node)
      }

      ctx.appendPosition(")", index = 2, parent = node)
    }
  }

  implicit class genAnnotationExpr(node:AnnotationExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      //TODO, not for this projects
      ctx.appendPosition(node.toString, position = node.genPosition(ctx))
    }
  }

  implicit class genAssignExpr(node:AssignExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit= {
      val left = node.getTarget
      val right = node.getValue
      val op = node.getOperator

      left.genCode(ctx, numsIntent, tgt, code_scope)

      ctx.appendPosition(op.asString(), index = 0, parent = node)

      right.genCode(ctx, numsIntent, tgt, code_scope)

    }
  }

  implicit class genInstanceOfExpr(node:InstanceOfExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      node.getExpression.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("instanceof", code_scope) else "instanceof", position = node.genPosition(ctx))
      node.getType.genCode(ctx, numsIntent, tgt, code_scope)
    }
  }

  implicit class genThisExpr(node:ThisExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("this", code_scope) else "this", position = node.genPosition(ctx))
    }
  }

  implicit class genNameExpr(node:NameExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      if (ctx.isAbstract) {
        val name = node.getName.asString()
        val value = if (ctx.variable_maps.contain(name, code_scope))
          ctx.variable_maps.get_match_content(name, code_scope)
        else
          ctx.ident_maps.getNewContent(name, code_scope)
        ctx.appendPosition(value, position = node.getName.genPosition(ctx))
      } else
        node.getName.genCode(ctx, numsIntent, tgt, code_scope)
    }
  }

  implicit class genCastExpr(node:CastExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      ctx.appendPosition("(", index = 0, parent = node)
      node.getType.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition(")", index = 1, parent = node)
      ctx.appendPosition("(", index = -3, parent = node)
      node.getExpression.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition(")", index = -2, parent = node)
    }
  }

  implicit class genMethodReferenceExpr(node:MethodReferenceExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val ident = node.getIdentifier
      val scope = node.getScope
      if (isScopeExpand(scope, ctx)) {
        scope.genCode(ctx, numsIntent, tgt, code_scope)
      } else {
        if (ctx.isAbstract)
          ctx.appendPosition(ctx.ident_maps.getNewContent(scope.toString, code_scope), position = scope.genPosition(ctx))
        else
          scope.genCode(ctx, numsIntent, tgt, code_scope)
      }
      ctx.appendPosition("::", index = 0, parent = node)

      ctx.appendPosition(if (ctx.isAbstract) ctx.method_maps.getNewContent(ident, code_scope) else ident, index = 1, parent = node)
    }
  }

  implicit class genEnclosedExpr(node:EnclosedExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit= {
      ctx.appendPosition("(", index = 0, parent = node)
      node.getInner.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition(")", index = 1, parent = node)
    }
  }

  implicit class genVariableDeclarationExpr(node:VariableDeclarationExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      node.getModifiers.foreach(_.genCode(ctx, numsIntent, tgt, code_scope))
      val varibles = node.getVariables

      /**
       * Variable expression should share a identical type
       * For example:
       * String a, b=4;
       */
      for (i <- 0 until (varibles.size())) {
        if (i == 0)
          varibles.get(i).genCode(ctx, numsIntent, tgt, code_scope)
        else {
          if (ctx.isAbstract)
            ctx.appendPosition(ctx.variable_maps.getNewContent(varibles.get(i).getName.getIdentifier, code_scope))
          else
            varibles.get(i).getName.genCode(ctx, numsIntent, tgt, code_scope)

          if (varibles.get(i).getInitializer.isPresent) {
            ctx.appendPosition("=", index = 0, parent = node)
            varibles.get(i).getInitializer.get().genCode(ctx, numsIntent, tgt, code_scope)
          }
        }
        if (i < varibles.size() - 1)
          ctx.appendPosition(",", index = 0, parent = node)
      }
    }
  }

  implicit class genSwitchExpr(node:SwitchExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("switch", code_scope) else "switch", position = node.genPosition(ctx))

      ctx.appendPosition("(", index = 0, parent = node)
      node.getSelector.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition(")", index = 1, parent = node)

      ctx.appendPosition("{", index = -3, parent = node)

      node.getEntries.foreach(_.genCode(ctx, numsIntent, tgt, code_scope))

      ctx.appendPosition("}", index = -2, parent = node)
      
    }
  }

  implicit class genSwitchEntry(node:SwitchEntry) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val lables = node.getLabels
      val sts = node.getStatements
      if (lables.size() == 0)
        ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("default", code_scope) else "default", position = node.genPosition(ctx))
      else {

        ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("case", code_scope) else "case", position = node.genPosition(ctx))

        for (i <- 0 until (lables.size())) {
          lables.get(i).genCode(ctx, numsIntent, tgt, code_scope)
          if (i < lables.size() - 1)
            ctx.appendPosition(",", index = 0, parent = node)
        }

      }
      ctx.appendPosition(":", index = 1, parent = node)

      sts.foreach(_.genCode(ctx, numsIntent, tgt, code_scope))

    }
  }

  // subclass
  implicit class genLiteralExpr(node:LiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      node match {
        case expr:NullLiteralExpr => ctx.appendPosition(expr.toString, position = node.genPosition(ctx))
        case expr:BooleanLiteralExpr => ctx.appendPosition(expr.getValue.toString, position = node.genPosition(ctx))
        case expr:LiteralStringValueExpr  => expr.genCode(ctx, numsIntent, tgt, code_scope)
      }
    }
  }

  implicit class genLiteralStringValueExpr(node:LiteralStringValueExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      node match {
        case expr: TextBlockLiteralExpr => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr: CharLiteralExpr => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr: DoubleLiteralExpr => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr: LongLiteralExpr => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr: StringLiteralExpr => expr.genCode(ctx, numsIntent, tgt, code_scope)
        case expr: IntegerLiteralExpr => expr.genCode(ctx, numsIntent, tgt, code_scope)
      }
    }
  }

  implicit class genTextBlockLiteralExpr(node:TextBlockLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val value = if (ctx.isAbstract) ctx.textBlock_maps.getNewContent(node.getValue, code_scope) else node.asString()
      ctx.appendPosition(value, position = node.genPosition(ctx))
    }
  }

  implicit class genCharLiteralExpr(node:CharLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val value = if (ctx.isAbstract) ctx.char_maps.getNewContent(node.getValue, code_scope) else node.asChar().toString
      ctx.appendPosition(value, position = node.genPosition(ctx))
    }
  }

  implicit class genDoubleLiteralExpr(node:DoubleLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val value = if (ctx.isAbstract)  ctx.double_maps.getNewContent(node.getValue, code_scope) else node.asDouble().toString
      ctx.appendPosition(value, position = node.genPosition(ctx))
    }
  }

  implicit class genLongLiteralExpr(node:LongLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val value = if (ctx.isAbstract)  ctx.long_maps.getNewContent(node.getValue, code_scope) else node.asNumber().toString
      ctx.appendPosition(value, position = node.genPosition(ctx))
    }
  }

  implicit class genStringLiteralExpr(node:StringLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val value = if (ctx.isAbstract)  ctx.string_maps.getNewContent(node.getValue, code_scope) else node.asString()
      ctx.appendPosition(value, position = node.genPosition(ctx))
    }
  }

  implicit class genIntegerLiteralExpr(node:IntegerLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val value = if (ctx.isAbstract)  ctx.int_maps.getNewContent(node.getValue, code_scope) else node.asNumber().toString
      ctx.appendPosition(value, position = node.genPosition(ctx))
    }
  }

  /**
   *  new B().new C();
   *  scope --> new B()
   *  type --> new C()
   * @param node
   */
  implicit class genObjectCreationExpr(node:ObjectCreationExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val arguments = node.getArguments
      val scope = node.getScope
      val tp = node.getType

      if (scope.isPresent) {
        if (isScopeExpand(scope.get(),ctx)){
          scope.get().genCode(ctx, numsIntent, tgt, code_scope)
        } else {
          if (ctx.isAbstract)
            ctx.appendPosition(ctx.ident_maps.getNewContent(scope.toString, code_scope), position = scope.get().genPosition(ctx))
          else
            scope.get().genCode(ctx, numsIntent, tgt, code_scope)
        }
        ctx.appendPosition(".", index = 0, parent = node)
      }

      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("new", code_scope) else "new", index = 1, parent = node)

      tp.genCode(ctx, numsIntent, tgt, code_scope)

      ctx.appendPosition("(", index = 2, parent = node)

      for (i <- 0 until(arguments.size())){
        arguments.get(i).genCode(ctx, numsIntent, tgt, code_scope)
        if (i < arguments.size() - 1)
          ctx.appendPosition(",", index = -3, parent = node)
      }

      ctx.appendPosition(")", index = -2, parent = node)

      val anonyBody = node.getAnonymousClassBody
      if (anonyBody.isPresent) {
        ctx.appendPosition("{", index = -1, parent = node)
        anonyBody.get().toList.foreach(_.genCode(ctx, numsIntent, tgt, code_scope))
        ctx.appendPosition("}", index = -1, parent = node)
      }
    }
  }


  implicit class genSuperExpr(node:SuperExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      val tpName = node.getTypeName

      if (tpName.isPresent){
        tpName.get().genCode(ctx, numsIntent, tgt, code_scope)
        ctx.appendPosition(".", index = 0, parent = node)
      }
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("super", code_scope) else "super", position = node.genPosition(ctx))
    }
  }

  implicit class genUnaryExpr(node:UnaryExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val op = node.getOperator.asString()
      val expr = node.getExpression
      if (node.isPostfix) {
        expr.genCode(ctx, numsIntent, tgt, code_scope)
        ctx.appendPosition(op, index = 0, parent = node)
      }
      if (node.isPrefix) {
        ctx.appendPosition(op, index = 1, parent = node)
        expr.genCode(ctx, numsIntent, tgt, code_scope)
      }
    }
  }

  implicit class genBinaryExpr(node:BinaryExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val left = node.getLeft
      val op = node.getOperator.asString()
      val right = node.getRight
      left.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition(op, index = 0, parent = node)
      right.genCode(ctx, numsIntent, tgt, code_scope)
    }
  }

  implicit class genFieldAccessExpr(node:FieldAccessExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      val full_name = node.toString
      if (ctx.ident_maps.getIdioms.contains(full_name) && ctx.isAbstract)
        ctx.appendPosition(full_name, index = 0, parent = node)
      else {
        if (isScopeExpand(node.getScope, ctx)) {
          node.getScope.genCode(ctx, numsIntent, tgt, code_scope)
        } else {
          if (ctx.isAbstract) {
            val scope_value = ctx.ident_maps.getNewContent(node.getScope.toString, code_scope)
            ctx.appendPosition(scope_value, position = node.getScope.genPosition(ctx))
          } else
            node.getScope.genCode(ctx, numsIntent, tgt, code_scope)
        }
        ctx.appendPosition(".", index = 0, parent = node)

        node.getName.genCode(ctx, numsIntent, tgt, code_scope)
      }
    }
  }

  implicit class genTypeExpr(node:TypeExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      node.getType.genCode(ctx, numsIntent, tgt, code_scope)
    }
  }

  implicit class genArrayInitializerExpr(node:ArrayInitializerExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val values = node.getValues
      ctx.appendPosition("{", index = 0, parent = node)

      for (i <- 0 until(values.size())){
        values.get(i).genCode(ctx, numsIntent, tgt, code_scope)
        if (i < values.size() - 1)
          ctx.appendPosition(",", index = 1, parent = node)
      }

      ctx.appendPosition("}", index = 2, parent = node)
    }
  }

  /******************************* Variable ********************************/
  implicit class genVariableDeclarator(node: VariableDeclarator) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val tp = node.getType
      val name = node.getName
      val init = node.getInitializer

      tp.genCode(ctx, numsIntent, tgt, code_scope)

      if (ctx.isAbstract) {
        val value =  ctx.variable_maps.getNewContent(name.asString(), code_scope)
        ctx.appendPosition(value, position = name.genPosition(ctx))
      } else name.genCode(ctx, numsIntent, tgt, code_scope)


      if (init.isPresent){
        ctx.appendPosition("=", index = 0, parent = node)
        init.get().genCode(ctx, numsIntent, tgt, code_scope)
      }
    }
  }

  /******************************* Node ************************/

  implicit class addPosition(node:Node) {
    def getPosition(ctx:Context, numsIntent:Int=0) = ctx.getNewPosition
  }

  implicit class genSimpleName(node:SimpleName) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val value = if (ctx.isAbstract) {
        ctx.ident_maps.getNewContent(node.getIdentifier, code_scope)
      } else
        node.getIdentifier

      ctx.appendPosition(value, position = node.genPosition(ctx))

      if (tgt != null) logger.debug(f"[${node.getIdentifier}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
    }
  }

  implicit class genModifier(node: Modifier) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val value = if (ctx.isAbstract)
        ctx.ident_maps.getNewContent(node.getKeyword.asString(), code_scope)
      else
        node.getKeyword.asString()
      ctx.appendPosition(value, position = node.genPosition(ctx))

      if (tgt != null) logger.debug(f"[${node.getKeyword.asString()}]"
        + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString}>")
    }
  }

  implicit class genType(node:Type) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = if (!node.asString().isEmpty){
      node match {
        case tp:UnionType  =>{
          ctx.appendPosition(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString(), code_scope) else node.asString(), position = node.genPosition(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:VarType  =>{
          ctx.appendPosition(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString(), code_scope) else node.asString(), position = node.genPosition(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:ReferenceType  => tp.genCode(ctx, numsIntent, tgt, code_scope)
        case tp:UnknownType  => {
          ctx.appendPosition(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString(), code_scope) else node.asString(), position = node.genPosition(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:PrimitiveType  =>{
          val typeName = tp.getType.asString()
          ctx.appendPosition(if (ctx.isAbstract) ctx.type_maps.getNewContent(typeName, code_scope) else typeName, position = node.genPosition(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:WildcardType  =>{
          ctx.appendPosition(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString(), code_scope) else node.asString(), position = node.genPosition(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:VoidType  =>{
          ctx.appendPosition(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString(), code_scope) else node.asString(), position = node.genPosition(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:IntersectionType  =>{
          ctx.appendPosition(if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString(), code_scope) else node.asString(), position = node.genPosition(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
      }
    }
  }

  implicit class genReferenceType(node:ReferenceType) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      node match {
        case tp: ArrayType => tp.genCode(ctx, numsIntent, tgt, code_scope)
        case tp: TypeParameter => tp.genCode(ctx, numsIntent, tgt, code_scope)
        case tp: ClassOrInterfaceType => tp.genCode(ctx, numsIntent, tgt, code_scope)
      }
    }
  }

  /**
   * So, int[][] becomes ArrayType(ArrayType(int)).
   * @param node
   */
  implicit class genArrayType(node:ArrayType) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      //TODO, need more details about
      val origin = node.getOrigin
      val comType = node.getComponentType

      comType.genCode(ctx, numsIntent, tgt, code_scope)
      ctx.appendPosition("[", index = 0, parent = node)
      ctx.appendPosition("]", index = 1, parent = node)
    }
  }

  implicit class genTypeParameter(node:TypeParameter) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val name = node.getName
      val typeBound = node.getTypeBound

      ctx.appendPosition("<", index = 0, parent = node)

      if (ctx.isAbstract)
        ctx.appendPosition(ctx.type_maps.getNewContent(name.asString(), code_scope), position = name.genPosition(ctx))
      else
        name.genCode(ctx, numsIntent, tgt, code_scope)

      if (typeBound.size() != 0){
        ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent("extends", code_scope) else "extends", index = 1, parent = node)

        for (i <- 0 until(typeBound.size())){
          typeBound.get(i).genCode(ctx, numsIntent, tgt, code_scope)
          if (i < typeBound.size() - 1)
            ctx.appendPosition("&", index = 2, parent = node)
        }

      }
      ctx.appendPosition(">", index = -3, parent = node)
    }
  }

  implicit class genClassOrInterfaceType(node:ClassOrInterfaceType) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val scope = node.getScope
      val name = node.getName
      val tps = node.getTypeArguments

      if (scope.isPresent) {
        val full_name = scope.get().asString() + "." + name.asString()
        if (ctx.ident_maps.getIdioms.contains(full_name) && ctx.isAbstract)
          ctx.appendPosition(full_name, index = 0, parent = node)
        else {
          if (isScopeExpand(scope.get(), ctx)) {
            scope.get().genCode(ctx, numsIntent, tgt, code_scope)
          } else {
            if (ctx.isAbstract) {
              ctx.appendPosition(ctx.ident_maps.getNewContent(scope.get().asString(), code_scope), position = name.genPosition(ctx))
            } else {
              scope.get().genCode(ctx, numsIntent, tgt, code_scope)
            }
          }
          ctx.appendPosition(".", index = 0, parent = node)
          // Name
          if (ctx.isAbstract) {
            ctx.appendPosition(ctx.type_maps.getNewContent(name.asString(), code_scope), position = name.genPosition(ctx))
          } else {
            name.genCode(ctx, numsIntent, tgt, code_scope)
          }
        }
      } else {
        // Name
        if (ctx.isAbstract) {
          ctx.appendPosition(ctx.type_maps.getNewContent(name.asString(), code_scope), position = name.genPosition(ctx))
        } else {
          name.genCode(ctx, numsIntent, tgt, code_scope)
        }
      }

      if (tps.isPresent){
        ctx.appendPosition("<", index = 1, parent = node)

        val typelist = tps.get()
        for (i <- 0 until typelist.size()){
          typelist.get(i).genCode(ctx, numsIntent, tgt, code_scope)
          if (i < typelist.size() - 1)
            ctx.appendPosition(",", index = -1, parent = node)
        }


        ctx.appendPosition(">", index = 2, parent = node)
      }
    }
  }


  implicit class genParameter(node:Parameter) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {
      val modifiers = node.getModifiers
      val tp = node.getType
      val name = node.getName

      modifiers.foreach(_.genCode(ctx,numsIntent, tgt))

      tp.genCode(ctx, numsIntent, tgt, code_scope)

      if (ctx.isAbstract) {
        ctx.appendPosition(ctx.variable_maps.getNewContent(name.asString(), code_scope), position = name.genPosition(ctx))
      } else
        name.genCode(ctx, numsIntent, tgt, code_scope)
    }
  }

  implicit class genName(node:Name) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null, code_scope:Int=0):Unit = {

      //TODO with Qualifier
      val qualifier = node.getQualifier
      if (qualifier.isPresent){
        if (ctx.isAbstract)
          ctx.appendPosition(ctx.ident_maps.getNewContent(qualifier.get().asString(), code_scope), position = qualifier.get().genPosition(ctx))
        else
          qualifier.get().genCode(ctx, numsIntent, tgt, code_scope)
        ctx.appendPosition(".", index = 0, parent = node)
      }
      ctx.appendPosition(if (ctx.isAbstract) ctx.ident_maps.getNewContent(node.getIdentifier, code_scope) else node.getIdentifier, position = node.genPosition(ctx))

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

    if (ctx.ident_maps.getIdioms.contains(scope.toString) ||
      ctx.ident_maps.getKeywords.contains(scope.toString()))
      return true

//    if (logger.isDebugEnabled) {
//      val scope_name = getScopeNodeName(scope)
//      for (elem <- allPaths) {
//        val name = getScopeNodeName(elem)
//        logger.debug(f"${scope_name} - ${name} ############## ${elem.getClass}")
//      }
//    }

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
