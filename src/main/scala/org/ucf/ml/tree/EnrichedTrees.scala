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



  def appendPositionalEmbedding(ctx:Context, parent:Node, term:String, index:Int) = {

    val parant_pos = parent.genPositionalEmbedding(ctx)
    val pos_size = parent.getChildNodes.size() + nums_wrap_position
    val newIndex = if (index >=0) index else (pos_size + index)
    val newNode = new SimpleName().setId(term).setParentNode(parent)
    val position = List.fill(pos_size)(0.0).updated(newIndex, 1.0) ::: parant_pos
    ctx.addPositionalEmbedding(newNode, position)
    ctx.append(content = term, position = position)
  }



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
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      // 1. package declaration
      val package_decl = node.getPackageDeclaration
      if (package_decl.isPresent) package_decl.get().genCode(ctx, numsIntent)

      // 2. Import Statements
      node.getImports.foreach(impl => {
        impl.genCode(ctx, numsIntent)
        ctx.appendNewLine()
      })

      // 3. A list of defined types, such as Class, Interface, Enum, Annotation ...
      node.getTypes.foreach(_.genCode(ctx, numsIntent))

    }
  }

  implicit class genPackageDeclaration(node: PackageDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      val pakcage_value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("package") else "package"
      appendPositionalEmbedding(ctx, node, pakcage_value, 0)

      node.getName.genCode(ctx)

      appendPositionalEmbedding(ctx, node, ";", -1)
      ctx.appendNewLine()
    }

  }

  /**
   *  import java.io.*;
   *  import static java.io.file.out;
   * @param node
   */
  implicit class genImportDeclaration(node:ImportDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      val import_value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("import") else "import"
      appendPositionalEmbedding(ctx, node, import_value, 0)


      if (node.isStatic) {
        val static_value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("static") else "static"
        appendPositionalEmbedding(ctx, node, static_value, 1)
      }

      node.getName.genCode(ctx, numsIntent)

      if (node.isAsterisk) {
        appendPositionalEmbedding(ctx, node, ".", -3)
        appendPositionalEmbedding(ctx, node, "*", -2)
      }
      appendPositionalEmbedding(ctx, node, ";", -1)
      ctx.appendNewLine()
    }
  }

  implicit class genTypeDeclaration(node:TypeDeclaration[_]) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      node match {
        /**
         *  TypeDeclaration
         *     -- EnumDeclaration
         *     -- AnnotationDeclaration
         *     -- ClassOrInterfaceDeclaration
         */
        case n: EnumDeclaration => n.genCode(ctx, numsIntent)
        case n: AnnotationDeclaration => n.genCode(ctx, numsIntent)
        case n: ClassOrInterfaceDeclaration => n.genCode(ctx, numsIntent)
      }
      
    }
  }

  implicit class genEnumDeclaration(node:EnumDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      val modifier = node.getModifiers
      modifier.foreach(_.genCode(ctx, numsIntent))

      node.getName.genCode(ctx, numsIntent)

      appendPositionalEmbedding(ctx, node, "{", 0)


      val entries = node.getEntries
      entries.foreach(entry => {
        entry.genCode(ctx, numsIntent)
        if (entry != entries.last) {
          appendPositionalEmbedding(ctx, node, ",", 0)
        }
      })

      appendPositionalEmbedding(ctx, node, "}", -1)
      ctx.appendNewLine()
    }
  }

  implicit class genAnnotationDeclaration(node:AnnotationDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      //TODO, No implementation about annotation
      ctx.append(node.toString, numsIntent)
    }
  }

  implicit class genClassOrInterfaceDeclaration(node:ClassOrInterfaceDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      if ((ctx.getGranularity == CLASS) || (!ctx.isAbstract)) {
        // 1. Class Modifiers, such as public/private
        val modifiers = node.getModifiers
        modifiers.foreach(_.genCode(ctx, numsIntent))

        if (node.isInterface) {
          val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("interface") else "interface"
          appendPositionalEmbedding(ctx, node, value, 0)
        } else {
          val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("class") else "class"
          appendPositionalEmbedding(ctx, node, value, 0)
        }

        // 2. Interface/Class Name
        if (ctx.isAbstract) {
          val value = ctx.type_maps.getNewContent(node.getNameAsString)
          ctx.append(content = value, position = node.getName.genPositionalEmbedding(ctx))
        } else
          node.getName.genCode(ctx, numsIntent)

        // 3. type parameters public interface Predicate<T> {}
        val tps = node.getTypeParameters
        tps.foreach(_.genCode(ctx, numsIntent))

        appendPositionalEmbedding(ctx, node, "{", 1)
        ctx.appendNewLine()
      }
      // 3. Class Members: Filed and method, constructor
      val members = node.getMembers
      members.foreach(_.genCode(ctx, numsIntent))

      if ((ctx.getGranularity == CLASS) || (!ctx.isAbstract))
        appendPositionalEmbedding(ctx, node, "}", -1)
      ctx.appendNewLine()
    }
  }

  implicit class genBodyDeclaration(node:BodyDeclaration[_]){
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      //TODO, only implment [[CallableDeclaration]] to handle with Method Declare
      node match {
        case n: InitializerDeclaration => n.genCode(ctx, numsIntent, tgt)
        case n: FieldDeclaration => n.genCode(ctx, numsIntent, tgt)
        case n: TypeDeclaration[_] => n.genCode(ctx, numsIntent, tgt)
        case n: EnumConstantDeclaration => n.genCode(ctx, numsIntent, tgt)
        case n: AnnotationMemberDeclaration => n.genCode(ctx, numsIntent, tgt)
        case n: CallableDeclaration[_] => n.genCode(ctx, numsIntent, tgt)
      }
    }
  }

  implicit class genInitializerDeclaration(node:InitializerDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      //TODO, no implmentation for learning bugs
      ctx.append(node.toString(), numsIntent)
    }
  }

  implicit class genFieldDeclaration(node:FieldDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      if (tgt != null) logger.debug(getPath(node, tgt))

      node.getModifiers.foreach(_.genCode(ctx, numsIntent, node))

      val varibles = node.getVariables
      varibles.foreach(ele => {
        if (ele == varibles.head) ele.getType.genCode(ctx, numsIntent, node)
        if (ctx.isAbstract) {
          val value = ctx.variable_maps.getNewContent(ele.getNameAsString)
          appendPositionalEmbedding(ctx, node, value, 0)
        } else
          ele.getName.genCode(ctx, numsIntent, node)

        if (ele.getInitializer.isPresent){
          appendPositionalEmbedding(ctx, node, "=", 1)
          ele.getInitializer.get().genCode(ctx, numsIntent, node)
        }

        if (ele != node.getVariables.last) appendPositionalEmbedding(ctx, node, ",", 2)
      })
      appendPositionalEmbedding(ctx, node, ";", -1)
      ctx.appendNewLine()
    }
  }

  implicit class genEnumConstantDeclaration(node:EnumConstantDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      //TODO, no implmentation for learning bugs
      ctx.append(node.toString, numsIntent)
    }
  }

  implicit class genAnnotationMemberDeclaration(node:AnnotationMemberDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      //TODO, no implmentation for learning bugs
      ctx.append(node.toString, numsIntent)
    }
  }

  implicit class genCallableDeclaration(node:CallableDeclaration[_]){
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      node match {
        case n: ConstructorDeclaration => n.genCode(ctx, numsIntent, tgt)
        case n: MethodDeclaration => n.genCode(ctx, numsIntent, tgt)
      }
    }
  }

  implicit class genConstructorDeclaration(node:ConstructorDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
//      node.getAccessSpecifier
      node.getModifiers.foreach(_.genCode(ctx, numsIntent))

      node.getTypeParameters.foreach(_.genCode(ctx, numsIntent))

      /*Method name, such as hello*/
      if (ctx.isAbstract) {
        val value = ctx.method_maps.getNewContent(node.getNameAsString)
        ctx.append(content = value, position = node.getName.genPositionalEmbedding(ctx))
      } else
        node.getName.genCode(ctx)

      /*formal paramters*/
      appendPositionalEmbedding(ctx, node, "(", 0)
      val parameters = node.getParameters
      parameters.foreach(p => {
        p.genCode(ctx, numsIntent)
        if (p != parameters.last) appendPositionalEmbedding(ctx, node, ",", 1)
      })
      appendPositionalEmbedding(ctx, node, ")", -1)

      val exceptions = node.getThrownExceptions
      if (exceptions.size() != 0) {
        val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("throws") else "throws"
        appendPositionalEmbedding(ctx, node, value, 2)
        node.getThrownExceptions.foreach(exp => {
          exp.genCode(ctx)
          if (exp != exceptions.last) appendPositionalEmbedding(ctx, node, ",", 1)
        })
      }
      node.getBody.genCode(ctx, numsIntent)
    }
  }

  implicit class genMethodDeclaration(node:MethodDeclaration) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      /*modifiers, such as public*/
      val modifiers = node.getModifiers
      modifiers.foreach(modifier => modifier.genCode(ctx, numsIntent, node.getName))

      /*Method return type, such as void, int, string*/
      node.getType.genCode(ctx, numsIntent, node.getName)

      /*Method name, such as hello*/
      if (ctx.isAbstract) {
        val value = ctx.method_maps.getNewContent(node.getNameAsString)
        ctx.append(content = value, position = node.getName.genPositionalEmbedding(ctx))
      } else
        node.getName.genCode(ctx, numsIntent, node.getName)


      /*formal paramters*/
      appendPositionalEmbedding(ctx, node, "(", 0)
      val parameters = node.getParameters
      parameters.foreach(p => {
        p.genCode(ctx, numsIntent, node.getName)
        if (p != parameters.last) appendPositionalEmbedding(ctx, node, ",", 1)
      })
      appendPositionalEmbedding(ctx, node, ")", 2)

      val exceptions = node.getThrownExceptions
      if (exceptions.size() != 0) {
        val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("throws") else "throws"
        appendPositionalEmbedding(ctx, node, value, 1)
        node.getThrownExceptions.foreach(exp => {
          exp.genCode(ctx, numsIntent, node.getName)
          if (exp != exceptions.last) appendPositionalEmbedding(ctx, node, ",", 1)
        })
      }
      /*Method Body*/
      val body = node.getBody
      if (body.isPresent) body.get().genCode(ctx, numsIntent, node.getName)
    }
  }


  /************************** Statement ***************************/

  implicit class genStatement(node:Statement) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      node match {
        case n:ForEachStmt => n.genCode(ctx, numsIntent, tgt)
        case n:LocalClassDeclarationStmt => n.genCode(ctx, numsIntent, tgt)
        case n:ContinueStmt => n.genCode(ctx, numsIntent, tgt)
        case n:ExpressionStmt => n.genCode(ctx, numsIntent, tgt)
        case n:LabeledStmt => n.genCode(ctx, numsIntent, tgt)
        case n:YieldStmt => n.genCode(ctx, numsIntent, tgt)
        case n:ReturnStmt => n.genCode(ctx, numsIntent, tgt)
        case n:WhileStmt => n.genCode(ctx, numsIntent, tgt)
        case n:EmptyStmt => n.genCode(ctx, numsIntent, tgt)
        case n:UnparsableStmt => n.genCode(ctx, numsIntent, tgt)
        case n:IfStmt => n.genCode(ctx, numsIntent, tgt)
        case n:BreakStmt => n.genCode(ctx, numsIntent, tgt)
        case n:AssertStmt => n.genCode(ctx, numsIntent, tgt)
        case n:ExplicitConstructorInvocationStmt => n.genCode(ctx, numsIntent, tgt)
        case n:DoStmt => n.genCode(ctx, numsIntent, tgt)
        case n:ForStmt => n.genCode(ctx, numsIntent, tgt)
        case n:ThrowStmt => n.genCode(ctx, numsIntent, tgt)
        case n:TryStmt => n.genCode(ctx, numsIntent, tgt)
        case n:SwitchStmt => n.genCode(ctx, numsIntent, tgt)
        case n:SynchronizedStmt => n.genCode(ctx, numsIntent, tgt)
        case n:BlockStmt => n.genCode(ctx, numsIntent, tgt)
      }
    }
  }

  implicit class genForEachStmt(node:ForEachStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      if (tgt != null) logger.debug(f"for" + getUpArrow + getPath(node, tgt))
      val integral = node.getIterable
      val variable = node.getVariable
      val body = node.getBody
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("for") else "for"
      appendPositionalEmbedding(ctx, node, value, 0)

      appendPositionalEmbedding(ctx, node, "(", 1)
      variable.genCode(ctx, numsIntent, node)

      appendPositionalEmbedding(ctx, node, ":", 2)
      integral.genCode(ctx, numsIntent, node)

      appendPositionalEmbedding(ctx, node, ")", -1)
      body.genCode(ctx, numsIntent, node)
    }
  }

  implicit class genLocalClassDeclarationStmt(node:LocalClassDeclarationStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(getPath(node, tgt))
      node.getClassDeclaration.genCode(ctx, numsIntent, node)
    }
  }

  implicit class genContinueStmt(node:ContinueStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"continue" + getUpArrow + getPath(node, tgt))

      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("continue") else "continue"
      appendPositionalEmbedding(ctx, node, value, 0)

      val label =  node.getLabel
      if (label.isPresent) label.get().genCode(ctx, numsIntent, node)

      appendPositionalEmbedding(ctx, node, ";", -1)
      ctx.appendNewLine()
    }

  }

  implicit class genExpressionStmt(node:ExpressionStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"[${node.toString}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
      node.getExpression.genCode(ctx, numsIntent, node)
      if (!node.getParentNode.get().isInstanceOf[Expression]) {
        appendPositionalEmbedding(ctx, node, ";", -1)
        ctx.appendNewLine()
      }
    }
  }

  implicit class genLabeledStmt(node:LabeledStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"Label" + getUpArrow + getPath(node, tgt))
      val label = node.getLabel
      val sts = node.getStatement

      label.genCode(ctx, numsIntent, node)

      appendPositionalEmbedding(ctx, node, ":", 0)

      sts.genCode(ctx, numsIntent, node)

      appendPositionalEmbedding(ctx, node, ";", -1)
      ctx.appendNewLine()

    }
  }

  implicit class genYieldStmt(node:YieldStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"yield" + getUpArrow + getPath(node, tgt))

      val value =if (ctx.isAbstract) ctx.ident_maps.getNewContent("yield") else "yield"
      appendPositionalEmbedding(ctx, node, value, 0)

      node.getExpression.genCode(ctx, numsIntent, node)

      appendPositionalEmbedding(ctx, node, ";", -1)
      ctx.appendNewLine()
    }
  }

  implicit class genReturnStmt(node:ReturnStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"return" + getUpArrow + getPath(node, tgt))
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("return") else "return"
      appendPositionalEmbedding(ctx, node, value, 0)

      val expr = node.getExpression
      if (expr.isPresent) expr.get().genCode(ctx, numsIntent, node)

      appendPositionalEmbedding(ctx, node, ";", -1)
      ctx.appendNewLine()
    }
  }

  implicit class genWhileStmt(node:WhileStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"while" + getUpArrow + getPath(node, tgt))
      val body = node.getBody
      val condition = node.getCondition
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("while") else "while"
      appendPositionalEmbedding(ctx, node, value, 0)


      appendPositionalEmbedding(ctx, node, "(", 0)

      condition.genCode(ctx, numsIntent, node)

      appendPositionalEmbedding(ctx, node, ")", 0)
      ctx.appendNewLine()

      body.genCode(ctx, numsIntent, node)
    }
  }

  implicit class genEmptyStmt(node:EmptyStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"empty" + getUpArrow + getPath(node, tgt))
      appendPositionalEmbedding(ctx, node, ";", -1)
      ctx.appendNewLine()
    }
  }

  implicit class genUnparsableStmt(node:UnparsableStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      //TODO, not for this project
      ctx.append(node.toString, numsIntent)
    }
  }

  implicit class genIfStmt(node:IfStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"if" + getUpArrow + getPath(node, tgt))
      val condition = node.getCondition
      val thenStmt = node.getThenStmt
      val elseStmt = node.getElseStmt

      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("if") else "if"
      appendPositionalEmbedding(ctx, node, value, 0)

      appendPositionalEmbedding(ctx, node, "(", 0)
      condition.genCode(ctx, numsIntent, node)
      appendPositionalEmbedding(ctx, node, ")", 0)

      ctx.appendNewLine()
      thenStmt.genCode(ctx, numsIntent, node)

      if (elseStmt.isPresent){
        val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("else") else "else"
        appendPositionalEmbedding(ctx, node, value, 0)
        ctx.appendNewLine()
        elseStmt.get().genCode(ctx, numsIntent, node)
      }
    }
  }

  implicit class genBreakStmt(node:BreakStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"break" + getUpArrow + getPath(node, tgt))
      val label = node.getLabel
      val value =if (ctx.isAbstract) ctx.ident_maps.getNewContent("break") else "break"
      appendPositionalEmbedding(ctx, node, value, 0)
      if (label.isPresent) label.get().genCode(ctx, numsIntent, node)

      appendPositionalEmbedding(ctx, node, ";", -1)
      ctx.appendNewLine()
    }
  }

  implicit class genAssertStmt(node:AssertStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"assert" + getUpArrow + getPath(node, tgt))
      val check = node.getCheck
      val msg = node.getMessage

      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("assert") else "assert"
      appendPositionalEmbedding(ctx, node, value, 0)
      check.genCode(ctx, numsIntent, node)

      if (msg.isPresent) {
        appendPositionalEmbedding(ctx, node, ":", 0)
        msg.get().genCode(ctx, numsIntent, node)
      }

      appendPositionalEmbedding(ctx, node, ";", -1)
      ctx.appendNewLine()
    }
  }

  implicit class genExplicitConstructorInvocationStmt(node:ExplicitConstructorInvocationStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      //TODO, not for this project
      ctx.append(node.toString, numsIntent)
    }
  }

  implicit class genDoStmt(node:DoStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"do-while" + getUpArrow + getPath(node, tgt))
      val body = node.getBody
      val condition = node.getCondition

      body.genCode(ctx, numsIntent, node)
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("while") else "while"
      appendPositionalEmbedding(ctx, node, value, 0)
      appendPositionalEmbedding(ctx, node, "(", 0)
      condition.genCode(ctx, numsIntent, node)
      appendPositionalEmbedding(ctx, node, ")", 0)

      appendPositionalEmbedding(ctx, node, ";", -1)
      ctx.appendNewLine()
    }
  }

  implicit class genForStmt(node:ForStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"for" + getUpArrow + getPath(node, tgt))
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("for") else "for"
      appendPositionalEmbedding(ctx, node, value, 0)
      appendPositionalEmbedding(ctx, node, "(", 0)
      val initial = node.getInitialization
      initial.foreach(init => {
        init.genCode(ctx, numsIntent, node)
        if (init != initial.last) appendPositionalEmbedding(ctx, node, ",", 0)
      })
      appendPositionalEmbedding(ctx, node, ";", 0)

      val compare = node.getCompare
      if (compare.isPresent) compare.get().genCode(ctx, numsIntent, node)

      appendPositionalEmbedding(ctx, node, ";", 0)

      val update = node.getUpdate
      update.foreach(up => {
        up.genCode(ctx, numsIntent, node)
        if (up != update.last) appendPositionalEmbedding(ctx, node, ",", 0)
      })
      appendPositionalEmbedding(ctx, node, ")", 0)

      val body = node.getBody
      body.genCode(ctx, numsIntent, node)
    }
  }

  implicit class genThrowStmt(node:ThrowStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"throw" + getUpArrow + getPath(node, tgt))
      val value_throw = if (ctx.isAbstract) ctx.ident_maps.getNewContent("throw") else "throw"
      appendPositionalEmbedding(ctx, node, value_throw, 0)
      val value_new = if (ctx.isAbstract) ctx.ident_maps.getNewContent("new") else "new"
      appendPositionalEmbedding(ctx, node, value_new, 0)
      node.getExpression.genCode(ctx, numsIntent, node)
      ctx.appendNewLine()
    }
  }

  implicit class genTryStmt(node:TryStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"try" + getUpArrow + getPath(node, tgt))

      val tryResources = node.getResources
      val tryCatch = node.getCatchClauses
      val tryFinally = node.getFinallyBlock
      val tryBlock = node.getTryBlock

      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("try") else "try"
      appendPositionalEmbedding(ctx, node, value, 0)
      if (tryResources.size() != 0){
        appendPositionalEmbedding(ctx, node, "(", 0)
        tryResources.foreach(expr => {
          expr.genCode(ctx, numsIntent, node)
          if (expr != tryResources.last) appendPositionalEmbedding(ctx, node, ",", 0)
        })
        appendPositionalEmbedding(ctx, node, ")", 0)
      }

      tryBlock.genCode(ctx, numsIntent, node)

      tryCatch.foreach(_.genCode(ctx, numsIntent))

      if (tryFinally.isPresent) tryFinally.get().genCode(ctx, numsIntent, node)
    }
  }

  implicit class genCatchClause(node:CatchClause) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      val parameter = node.getParameter
      val body = node.getBody
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("catch") else "catch"
      appendPositionalEmbedding(ctx, node, value, 0)
      appendPositionalEmbedding(ctx, node, "(", 0)
      parameter.genCode(ctx, numsIntent)
      appendPositionalEmbedding(ctx, node, ")", 0)
      body.genCode(ctx)
      ctx.appendNewLine()
    }
  }


  implicit class genSwitchStmt(node:SwitchStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"switch" + getUpArrow + getPath(node, tgt))
      val entries = node.getEntries
      val selector = node.getSelector
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("switch") else "switch"
      appendPositionalEmbedding(ctx, node, value, 0)

      appendPositionalEmbedding(ctx, node, "(", 0)
      selector.genCode(ctx, numsIntent, node)
      appendPositionalEmbedding(ctx, node, ")", 0)
      ctx.appendNewLine()

      appendPositionalEmbedding(ctx, node, "{", 0)

      entries.foreach(_.genCode(ctx, numsIntent, node))

      appendPositionalEmbedding(ctx, node, "}", 0)
      ctx.appendNewLine()
    }
  }

  implicit class genSynchronizedStmt(node:SynchronizedStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {
      if (tgt != null) logger.debug(f"synchronized" + getUpArrow + getPath(node, tgt))
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("synchronized") else "synchronized"
      appendPositionalEmbedding(ctx, node, value, 0)

      appendPositionalEmbedding(ctx, node, "(", 0)
      node.getExpression.genCode(ctx, numsIntent, node)
      appendPositionalEmbedding(ctx, node, ")", 0)
      node.getBody.genCode(ctx, numsIntent, node)
    }
  }

  implicit class genBlockStmt(node:BlockStmt) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit  = {

      appendPositionalEmbedding(ctx, node, "{", 0)
      ctx.appendNewLine()
      node.getStatements.foreach(sts => sts.genCode(ctx, numsIntent, tgt))

      appendPositionalEmbedding(ctx, node, "}", 0)
      ctx.appendNewLine()
    }
  }


  /******************************* Expression ********************************/
  implicit class genExpression(node:Expression){
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      node match {
        case expr:ArrayAccessExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:ClassExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:LambdaExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:ArrayCreationExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:ConditionalExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:MethodCallExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:AnnotationExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:AssignExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:InstanceOfExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:ThisExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:NameExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:CastExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:MethodReferenceExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:EnclosedExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:VariableDeclarationExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:SwitchExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:LiteralExpr => expr.genCode(ctx, numsIntent, tgt)
        case expr:ObjectCreationExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:SuperExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:UnaryExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:BinaryExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:FieldAccessExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:TypeExpr  => expr.genCode(ctx, numsIntent, tgt)
        case expr:ArrayInitializerExpr  => expr.genCode(ctx, numsIntent, tgt)
      }
    }
  }


  implicit class genArrayAccessExpr(node:ArrayAccessExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val name = node.getName
      val index = node.getIndex
      name.genCode(ctx, numsIntent)
      appendPositionalEmbedding(ctx, node, "[", 0)
      index.genCode(ctx, numsIntent)
      appendPositionalEmbedding(ctx, node, "]", 0)
    }
  }


  implicit class genClassExpr(node:ClassExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit= {

      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("Object") else "Object"
      appendPositionalEmbedding(ctx, node, value, 0)
      appendPositionalEmbedding(ctx, node, ".", 0)
      node.getType.genCode(ctx)
      ctx.appendNewLine()
    }
  }


  implicit class genLambdaExpr(node:LambdaExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      val parameters = node.getParameters

      if (parameters.size > 1)
        appendPositionalEmbedding(ctx, node, "(", 0)

      parameters.foreach(p => {
        p.genCode(ctx, numsIntent)
        if (p != parameters.last) appendPositionalEmbedding(ctx, node, ",", 0)
      })

      if (parameters.size > 1)
        appendPositionalEmbedding(ctx, node, ")", 0)

      appendPositionalEmbedding(ctx, node, "->", 0)

      val body = node.getBody

      body.genCode(ctx, numsIntent)

    }
  }


  implicit class genArrayCreationExpr(node:ArrayCreationExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      val eleType = node.getElementType
      val initial = node.getInitializer
      val levels = node.getLevels

      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("new") else "new"
      appendPositionalEmbedding(ctx, node, value, 0)
      eleType.genCode(ctx, numsIntent)
      for (level <- levels){
        appendPositionalEmbedding(ctx, node, "[", 0)
        val dim = level.getDimension
        if (dim.isPresent) dim.get().genCode(ctx, numsIntent)
        appendPositionalEmbedding(ctx, node, "]", 0)
      }

      if (initial.isPresent) initial.get().genCode(ctx, numsIntent)
    }
  }

  implicit class genConditionalExpr(node:ConditionalExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val condition = node.getCondition
      val thenExpr = node.getThenExpr
      val elseExpr = node.getElseExpr

      condition.genCode(ctx, numsIntent)
      appendPositionalEmbedding(ctx, node, "?", 0)
      thenExpr.genCode(ctx, numsIntent)
      appendPositionalEmbedding(ctx, node, ":", 0)
      elseExpr.genCode(ctx, numsIntent)
    }
  }

  implicit class genMethodCallExpr(node:MethodCallExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      val scope = node.getScope
      val arguments = node.getArguments

      if (scope.isPresent) {
        if (isScopeExpand(scope.get(), ctx)){
          scope.get().genCode(ctx, numsIntent)
        } else {
          if (ctx.isAbstract) {
            val scope_value = ctx.variable_maps.getNewContent(scope.get().toString)
            ctx.append(content = scope_value, position = scope.get().genPositionalEmbedding(ctx))
          } else
            scope.get().genCode(ctx, numsIntent)
        }
        appendPositionalEmbedding(ctx, node, ".", 0)
      }

      if (ctx.isAbstract) {
        val funcName = ctx.method_maps.getNewContent(node.getName.asString())
        ctx.append(content = funcName, position = node.getName.genPositionalEmbedding(ctx))
      } else node.getName.genCode(ctx)

      appendPositionalEmbedding(ctx, node, "(", 0)
      arguments.foreach(expr => {
        expr.genCode(ctx, numsIntent)
        if (expr != arguments.last) appendPositionalEmbedding(ctx, node, ",", 0)
      })
      appendPositionalEmbedding(ctx, node, ")", 0)
    }
  }

  implicit class genAnnotationExpr(node:AnnotationExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      //TODO, not for this projects
      ctx.append(node.toString)
    }
  }

  implicit class genAssignExpr(node:AssignExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit= {
      val left = node.getTarget
      val right = node.getValue
      val op = node.getOperator

      left.genCode(ctx, numsIntent)

      val value = op.asString()
      appendPositionalEmbedding(ctx, node, value, 0)

      right.genCode(ctx, numsIntent)


    }
  }

  implicit class genInstanceOfExpr(node:InstanceOfExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      node.getExpression.genCode(ctx)
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("instanceof") else "instanceof"
      appendPositionalEmbedding(ctx, node, value, 0)
      node.getType.genCode(ctx)
    }
  }

  implicit class genThisExpr(node:ThisExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("this") else "this"
      appendPositionalEmbedding(ctx, node, value, 0)
    }
  }

  implicit class genNameExpr(node:NameExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      if (ctx.isAbstract) {
        val value = ctx.variable_maps.getNewContent(node.getName.asString())
        ctx.append(content = value, position = node.getName.genPositionalEmbedding(ctx))

      } else node.getName.genCode(ctx, numsIntent, tgt)

    }
  }

  implicit class genCastExpr(node:CastExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      //TODO no implement for this project
      appendPositionalEmbedding(ctx, node, "(", 0)
      node.getType.genCode(ctx)
      appendPositionalEmbedding(ctx, node, ")", 0)
      appendPositionalEmbedding(ctx, node, "(", 0)
      node.getExpression.genCode(ctx)
      appendPositionalEmbedding(ctx, node, ")", 0)
    }
  }

  implicit class genMethodReferenceExpr(node:MethodReferenceExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val ident = node.getIdentifier
      val scope = node.getScope
      if (isScopeExpand(scope, ctx)) {
        scope.genCode(ctx, numsIntent)
      } else {
        if (ctx.isAbstract)
          ctx.append(content = ctx.ident_maps.getNewContent(scope.toString),
            position = scope.genPositionalEmbedding(ctx))

        else
          scope.genCode(ctx, numsIntent)
      }
      appendPositionalEmbedding(ctx, node, "::", 0)

      val value = if (ctx.isAbstract) ctx.method_maps.getNewContent(ident) else ident
      appendPositionalEmbedding(ctx, node, value, 0)
    }
  }

  implicit class genEnclosedExpr(node:EnclosedExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit= {
      appendPositionalEmbedding(ctx, node, "(", 0)
      node.getInner.genCode(ctx)
      appendPositionalEmbedding(ctx, node, ")", 0)
    }
  }

  implicit class genVariableDeclarationExpr(node:VariableDeclarationExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      node.getModifiers.foreach(_.genCode(ctx, numsIntent, tgt))
      val varibles = node.getVariables
      varibles.foreach(_.genCode(ctx, numsIntent, tgt))
    }
  }

  implicit class genSwitchExpr(node:SwitchExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("switch") else "switch"
      appendPositionalEmbedding(ctx, node, value, 0)

      appendPositionalEmbedding(ctx, node, "(", 0)
      node.getSelector.genCode(ctx)
      appendPositionalEmbedding(ctx, node, ")", 0)

      appendPositionalEmbedding(ctx, node, "{", 0)

      node.getEntries.foreach(_.genCode(ctx))

      appendPositionalEmbedding(ctx, node, "}", 0)
      ctx.appendNewLine()
    }
  }

  implicit class genSwitchEntry(node:SwitchEntry) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      //TODO
      val lables = node.getLabels
      val sts = node.getStatements

      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("case") else "case"
      appendPositionalEmbedding(ctx, node, value, 0)
      lables.foreach(expr => {
        expr.genCode(ctx)
        if (expr != lables.last) appendPositionalEmbedding(ctx, node, ",", 0)
      })

      appendPositionalEmbedding(ctx, node, "->", 0)

      appendPositionalEmbedding(ctx, node, "{", 0)
      sts.foreach(_.genCode(ctx))
      appendPositionalEmbedding(ctx, node, "}", 0)

    }
  }

  // subclass
  implicit class genLiteralExpr(node:LiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      node match {
        case expr:NullLiteralExpr => ctx.append(expr.toString)
        case expr:BooleanLiteralExpr => ctx.append(expr.getValue.toString)
        case expr:LiteralStringValueExpr  => expr.genCode(ctx, numsIntent)
      }
    }
  }

  implicit class genLiteralStringValueExpr(node:LiteralStringValueExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
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
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val value = if (ctx.isAbstract) ctx.textBlock_maps.getNewContent(node.getValue) else node.asString()
      ctx.append(value)
    }
  }

  implicit class genCharLiteralExpr(node:CharLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val value = if (ctx.isAbstract) ctx.char_maps.getNewContent(node.getValue) else node.asChar().toString
      ctx.append(value)
    }
  }

  implicit class genDoubleLiteralExpr(node:DoubleLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val value = if (ctx.isAbstract)  ctx.double_maps.getNewContent(node.getValue) else node.asDouble().toString
      ctx.append(value)
    }
  }

  implicit class genLongLiteralExpr(node:LongLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val value = if (ctx.isAbstract)  ctx.long_maps.getNewContent(node.getValue) else node.asNumber().toString
      ctx.append(value)
    }
  }

  implicit class genStringLiteralExpr(node:StringLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val value = if (ctx.isAbstract)  ctx.string_maps.getNewContent(node.getValue) else node.asString()
      ctx.append("\"" + value + "\"")
    }
  }

  implicit class genIntegerLiteralExpr(node:IntegerLiteralExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
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
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
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

      appendPositionalEmbedding(ctx, node, "(", 0)
      arguments.foreach(expr =>{
        expr.genCode(ctx, numsIntent)
        if (expr != arguments.last) appendPositionalEmbedding(ctx, node, ",", 0)
      })
      appendPositionalEmbedding(ctx, node, ")", 0)
    }
  }


  implicit class genSuperExpr(node:SuperExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      val tpName = node.getTypeName

      if (tpName.isPresent){
        tpName.get().genCode(ctx)
        ctx.append(".")
      }
      val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("super") else "super"
      appendPositionalEmbedding(ctx, node, value, 0)
    }
  }

  implicit class genUnaryExpr(node:UnaryExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
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
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val left = node.getLeft
      val op = node.getOperator.asString()
      val right = node.getRight
      left.genCode(ctx, numsIntent, tgt)
      // TODO op need path
      appendPositionalEmbedding(ctx, node, op, 0)
      right.genCode(ctx, numsIntent, tgt)
    }
  }

  implicit class genFieldAccessExpr(node:FieldAccessExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      if (isScopeExpand(node.getScope, ctx)){
        node.getScope.genCode(ctx, numsIntent)
      } else {
        if (ctx.isAbstract) {
          val scope_value = ctx.variable_maps.getNewContent(node.getScope.toString)
          ctx.append(content = scope_value, position = node.getScope.genPositionalEmbedding(ctx))
        } else
          node.getScope.genCode(ctx, numsIntent)
      }
      appendPositionalEmbedding(ctx, node, ".", 0)

      // filed
      if (ctx.isAbstract) {
        val name = ctx.ident_maps.getNewContent(node.getName.asString())
        ctx.append(content = name, position = node.getName.genPositionalEmbedding(ctx))
      } else
        node.getName.genCode(ctx)
    }
  }

  implicit class genTypeExpr(node:TypeExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      node.getType.genCode(ctx, numsIntent)
    }
  }

  implicit class genArrayInitializerExpr(node:ArrayInitializerExpr) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val values = node.getValues
      appendPositionalEmbedding(ctx, node, "{", 0)
      values.foreach(ele => {
        ele.genCode(ctx, numsIntent)
        if (ele != values.last) appendPositionalEmbedding(ctx, node, ",", 0)
      })
      appendPositionalEmbedding(ctx, node, "}", 0)
    }
  }

  /******************************* Variable ********************************/
  implicit class genVariableDeclarator(node: VariableDeclarator) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val tp = node.getType
      val name = node.getName
      val init = node.getInitializer

      tp.genCode(ctx, numsIntent, tgt)

      if (ctx.isAbstract) {
        val value =  ctx.variable_maps.getNewContent(name.asString())
        ctx.append(content = value, position = name.genPositionalEmbedding(ctx))
      } else name.genCode(ctx, numsIntent, tgt)


      if (init.isPresent){
        appendPositionalEmbedding(ctx, node, "=", 0)
        init.get().genCode(ctx, numsIntent, tgt)
      }
    }
  }

  /******************************* Node ************************/

  implicit class addPosition(node:Node) {
    def getPosition(ctx:Context, numsIntent:Int=0) = ctx.getNewPosition
  }

  implicit class genSimpleName(node:SimpleName) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      ctx.append(content = node.getIdentifier, position = node.genPositionalEmbedding(ctx))
      if (tgt != null) logger.debug(f"[${node.getIdentifier}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
    }
  }

  implicit class genModifier(node: Modifier) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      ctx.append(content = node.getKeyword.asString(), position = node.genPositionalEmbedding(ctx))

      if (tgt != null) logger.debug(f"[${node.getKeyword.asString()}]"
        + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString}>")
    }
  }

  implicit class genType(node:Type) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = if (!node.asString().isEmpty){
      node match {
        case tp:UnionType  =>{
          val value = if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString()
          ctx.append(content = value, position = node.genPositionalEmbedding(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:VarType  =>{
          val value = if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString()
          ctx.append(content = value, position = node.genPositionalEmbedding(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:ReferenceType  => tp.genCode(ctx, numsIntent, tgt)
        case tp:UnknownType  => {
          val value = if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString()
          ctx.append(content = value, position = node.genPositionalEmbedding(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:PrimitiveType  =>{
          val value = if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString()
          ctx.append(content = value, position = node.genPositionalEmbedding(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:WildcardType  =>{
          val value = if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString()
          ctx.append(content = value, position = node.genPositionalEmbedding(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:VoidType  =>{
          val value = if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString()
          ctx.append(content = value, position = node.genPositionalEmbedding(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
        case tp:IntersectionType  =>{
          val value = if (ctx.isAbstract) ctx.type_maps.getNewContent(node.asString()) else node.asString()
          ctx.append(content = value, position = node.genPositionalEmbedding(ctx))
          if (tgt != null) logger.debug(f"[${node.asString()}]" + getUpArrow + getPath(node, tgt) + getDownArrow + s"<${tgt.toString()}>")
        }
      }
    }
  }

  implicit class genReferenceType(node:ReferenceType) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      node match {
        case tp: ArrayType => tp.genCode(ctx, numsIntent, tgt)
        case tp: TypeParameter => tp.genCode(ctx, numsIntent, tgt)
        case tp: ClassOrInterfaceType => tp.genCode(ctx, numsIntent, tgt)
      }
    }
  }

  /**
   * So, int[][] becomes ArrayType(ArrayType(int)).
   * @param node
   */
  implicit class genArrayType(node:ArrayType) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      //TODO, need more details about
      val origin = node.getOrigin
      val comType = node.getComponentType
      comType.genCode(ctx, numsIntent, tgt)
      appendPositionalEmbedding(ctx, node, "[", 0)
      appendPositionalEmbedding(ctx, node, "]", 0)
    }
  }

  implicit class genTypeParameter(node:TypeParameter) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val name = node.getName
      val typeBound = node.getTypeBound

      appendPositionalEmbedding(ctx, node, "<", 0)

      if (ctx.isAbstract)
        ctx.append(ctx.type_maps.getNewContent(name.asString()))
      else name.genCode(ctx, numsIntent, tgt)

      if (typeBound.size() != 0){
        val value = if (ctx.isAbstract) ctx.ident_maps.getNewContent("extends") else "extends"
        appendPositionalEmbedding(ctx, node, value, 0)
        typeBound.foreach(bound => {
          bound.genCode(ctx, numsIntent, tgt)
          if (bound != typeBound.last) appendPositionalEmbedding(ctx, node, "&", 0)
        })
      }
      appendPositionalEmbedding(ctx, node, ">", 0)
    }
  }

  implicit class genClassOrInterfaceType(node:ClassOrInterfaceType) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val scope = node.getScope
      val name = node.getName
      val tps = node.getTypeArguments

      if (ctx.isAbstract) {
        val value = (if (scope.isPresent) scope.get().asString() + "." else EmptyString) + name.asString()
        ctx.append(content = ctx.type_maps.getNewContent(value), position = name.genPositionalEmbedding(ctx))
      } else {
        if (scope.isPresent) {
          scope.get().genCode(ctx, numsIntent, tgt)
          appendPositionalEmbedding(ctx, node, ".", 0)
        }
        name.genCode(ctx, numsIntent, tgt)
      }

      if (tps.isPresent){
        appendPositionalEmbedding(ctx, node, "<", 0)
        tps.get().foreach(_.genCode(ctx, numsIntent, tgt))
        appendPositionalEmbedding(ctx, node, ">", 0)
      }
    }
  }


  implicit class genParameter(node:Parameter) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {
      val modifiers = node.getModifiers
      val tp = node.getType
      val name = node.getName

      modifiers.foreach(_.genCode(ctx,numsIntent, tgt))

      tp.genCode(ctx, numsIntent, tgt)

      if (ctx.isAbstract) {
        ctx.append(ctx.variable_maps.getNewContent(name.asString()), position = name.genPositionalEmbedding(ctx))
      } else
        name.genCode(ctx, numsIntent, tgt)
    }
  }

  implicit class genName(node:Name) {
    def genCode(ctx:Context, numsIntent:Int=0, tgt:Node=null):Unit = {

      val qualifier = node.getQualifier
      if (qualifier.isPresent){
        if (ctx.isAbstract) {
          val value = ctx.ident_maps.getNewContent(qualifier.get().asString())
          val pos = qualifier.get().genPositionalEmbedding(ctx)
          ctx.append(content = value, position = pos)
        }
        else
          qualifier.get().genCode(ctx, numsIntent)

        appendPositionalEmbedding(ctx,node, ".", -1)
      }
      val qual_value = if (ctx.isAbstract) ctx.variable_maps.getNewContent(node.getIdentifier) else node.getIdentifier
      val qual_pos = node.genPositionalEmbedding(ctx)
      ctx.append(content = qual_value, position = qual_pos)
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
