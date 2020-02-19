/***
 * a visitor generate s-expr from JAVA AST
 * (1) for simple, won't collect  Java annotation info, cause it is has no semantic operation during runtime
 * some precision will loose in Type, since it will not effect the soundness of alias/value flow analysis
 * and inner class new outside is not allow which means we don't support "Foo.new ..."
 *
 * (2) cause Java is using type wiping for generic, so the generic part will be unsound here, as AI no checking
 * too
 *
 * (3) do not support "Foo.super..."
 *
 * (4) only support array access like  "foo[...][...]...."
 *
 * (5) do not support lambda for now
 *
 * (6) post ++/-- will be messed up now... const pop base on this will be unsound, this will be fix with future
 *
 * (7) don't support cast with addition bounds. "(Foo & Bar) foobar"
 *
 * (8) don't support specialized method call
 *
 * (9) don't support enum for now
 *
 * (10) only support simple "continue;" "break;"
 *
 * (11) don not support sychonize semantic
 *
 * (12) do not support try with resource
 *
 * Yihao Sun <email>ysun67@syr.edu</email>
 * Syracuse 2020
 */

package com.syr.jindex.analyzer;

import com.syr.jindex.parser.Java8Parser;
import com.syr.jindex.parser.Java8ParserBaseVisitor;

public class JavaSexprVisitor extends Java8ParserBaseVisitor<String> {

//    StringBuilder sexprbuilder;
//
//    public JavaSexprVisitor() {
//        this.sexprbuilder = new StringBuilder("");
//    }
    int lineCounter;

    public JavaSexprVisitor() {
        lineCounter = 0;
    }

    /***
     * compilation-unit? := `((? pkg?) (? list/c import?) (? type?))
     * @param ctx
     * @return
     */
    @Override
    public String visitCompilationUnit(Java8Parser.CompilationUnitContext ctx) {
        String pkgDeclS = visit(ctx.packageDeclaration());
        StringBuilder importsSB = new StringBuilder("(");
        for (Java8Parser.ImportDeclarationContext impCtx : ctx.importDeclaration()) {
            importsSB.append(visit(impCtx));
            importsSB.append(' ');
        }
        importsSB.append(')');
        StringBuilder typeDeclSB = new StringBuilder();
        for (Java8Parser.TypeDeclarationContext typeCtx : ctx.typeDeclaration()) {
            typeDeclSB.append(visit(typeCtx));
        }
        return String.format("(%s\n %s\n %s)", pkgDeclS, importsSB.toString(), typeDeclSB.toString());
    }

    // import? := ((? or 'static' 'non-static') (? list/c symbol? name))
    @Override
    public String visitSingleTypeImportDeclaration(Java8Parser.SingleTypeImportDeclarationContext ctx) {
        StringBuilder importSB = new StringBuilder("(");
        for (String s : ctx.typeName().getText().split("\\.")) {
            importSB.append(s);
            importSB.append(' ');
        }
        importSB.append(')');
        return String.format("(non-static %s)", importSB.toString());
    }

    @Override
    public String visitTypeImportOnDemandDeclaration(Java8Parser.TypeImportOnDemandDeclarationContext ctx) {
        StringBuilder importSB = new StringBuilder("(");
        for (String s : ctx.packageOrTypeName().getText().split("\\.")) {
            importSB.append(s);
            importSB.append(' ');
        }
        importSB.append("*)");
        return String.format("(non-static %s)", importSB.toString());
    }

    @Override
    public String visitSingleStaticImportDeclaration(Java8Parser.SingleStaticImportDeclarationContext ctx) {
        StringBuilder importSB = new StringBuilder("(");
        for (String s : ctx.typeName().getText().split("\\.")) {
            importSB.append(s);
            importSB.append(' ');
        }
        importSB.append(ctx.Identifier().getText());
        importSB.append(')');
        return String.format("(static %s)", importSB.toString());
    }

    @Override
    public String visitStaticImportOnDemandDeclaration(Java8Parser.StaticImportOnDemandDeclarationContext ctx) {
        StringBuilder importSB = new StringBuilder("(");
        for (String s : ctx.typeName().getText().split("\\.")) {
            importSB.append(s);
            importSB.append(' ');
        }
        importSB.append("*)");
        return String.format("(static %s)", importSB.toString());
    }

    /***
     * pkg? := (? symbol? pkg-name)
     * NOTICE: package modifier will not be tracked here
     * @param ctx
     * @return
     */
    @Override
    public String visitPackageDeclaration(Java8Parser.PackageDeclarationContext ctx) {
        return ctx.packageName().getText();
    }

    // decl? (or/c interface? class?)

    /***
     * class? :=
     *        ((? symbol? name) (? list/c classModifier?)
     *         (? list/c typeParams?) (? superClass?)
     *         (? list/c superInterface?) (? classBody?))
     * @param ctx
     * @return
     */
    @Override
    public String visitNormalClassDeclaration(Java8Parser.NormalClassDeclarationContext ctx) {
        String name = ctx.Identifier().getText();
        StringBuilder classModifierSB = new StringBuilder("(");
        for (Java8Parser.ClassModifierContext cmCtx : ctx.classModifier()) {
            classModifierSB.append(cmCtx.getText());
            classModifierSB.append(' ');
        }
        classModifierSB.append(')');
        String typeParamsS = (ctx.typeParameters() != null) ? visit(ctx.typeParameters()) : "()";
        String superClassS = (ctx.superclass() != null) ? visit(ctx.superclass()) : "()";
        String superInterfaceS = (ctx.superinterfaces() != null) ? visit(ctx.superinterfaces()) : "()";
        String classBodyS = visit(ctx.classBody());
        return String.format("(Class %s\n %s\n %s\n %s\n %s\n %s)", name, classModifierSB.toString(),
                typeParamsS, superClassS, superInterfaceS, classBodyS);
    }

    /***
     *  typeParams? := (? list/c typeParam)
     *  useless in gen sexpr, can direct to next level
     * @param ctx
     * @return
     */
    @Override
    public String visitTypeParameters(Java8Parser.TypeParametersContext ctx) {
        return visit(ctx.typeParameterList());
    }

    @Override
    public String visitTypeParameterList(Java8Parser.TypeParameterListContext ctx) {
        StringBuilder tpSB = new StringBuilder("(");
        for (Java8Parser.TypeParameterContext tpCtx : ctx.typeParameter()) {
            tpSB.append(visit(tpCtx));
            tpSB.append(' ');
        }
        tpSB.append(")");
        return tpSB.toString();
    }

    /***
     * only take care of identifier info here
     * @param ctx
     * @return
     */
    @Override
    public String visitTypeParameter(Java8Parser.TypeParameterContext ctx) {
        return ctx.Identifier().getText();
    }

    /**
     * superclass? := (? symbol?)
     * directed to classtype
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitSuperclass(Java8Parser.SuperclassContext ctx) {
        return visit(ctx.classType());
    }

    /**
     * class-type? := ((? list? structural-name) (? list/c type-arg?))
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitShortClassType(Java8Parser.ShortClassTypeContext ctx) {
        String typeArgS = (ctx.typeArguments() != null) ? visit(ctx.typeArguments()) : "()";
        return String.format("(%s %s)", ctx.Identifier().getText(), typeArgS);
    }

    // TODO: support long class type name
    @Override
    public String visitLongClassType(Java8Parser.LongClassTypeContext ctx) {
        return super.visitLongClassType(ctx);
    }

    /***
     * super-interface? := (? list/c interface-type?)
     * @param ctx
     * @return
     */
    @Override
    public String visitSuperinterfaces(Java8Parser.SuperinterfacesContext ctx) {
        return visit(ctx.interfaceTypeList());
    }

    @Override
    public String visitInterfaceTypeList(Java8Parser.InterfaceTypeListContext ctx) {
        StringBuilder itSB = new StringBuilder("(");
        for (Java8Parser.InterfaceTypeContext iCtx : ctx.interfaceType()) {
            itSB.append(visit(iCtx));
            itSB.append(' ');
        }
        itSB.append(')');
        return itSB.toString();
    }

    @Override
    public String visitInterfaceType(Java8Parser.InterfaceTypeContext ctx) {
        return visit(ctx.classType());
    }

    /***
     * class-body (? vector/c class-body-decl?)
     * @param ctx
     * @return
     */
    @Override
    public String visitClassBody(Java8Parser.ClassBodyContext ctx) {
        StringBuilder cbSB = new StringBuilder("#(");
        for (Java8Parser.ClassBodyDeclarationContext cbdCtx : ctx.classBodyDeclaration()) {
            cbSB.append(visit(cbdCtx));
            cbSB.append('\n');
        }
        cbSB.append(')');
        return cbSB.toString();
    }

    /***
     * field-decl? := (FieldDecl (? list/c symbol?) (? unann-type?) (? list/c var-def?))
     * @param ctx
     * @return
     */
    @Override
    public String visitFieldDeclaration(Java8Parser.FieldDeclarationContext ctx) {
        StringBuilder fmodSB = new StringBuilder("(");
        for (Java8Parser.FieldModifierContext fmodCtx : ctx.fieldModifier()) {
            fmodSB.append(visit(fmodCtx));
            fmodSB.append(' ');
        }
        fmodSB.append(')');
        String unannTypeS = visit(ctx.unannType());
        String varDeclListS = visit(ctx.variableDeclaratorList());
        return String.format("(Field %s %s %s)", fmodSB.toString(), unannTypeS, varDeclListS);
    }

    @Override
    public String visitFieldModifier(Java8Parser.FieldModifierContext ctx) {
        return ctx.getText();
    }

    @Override
    public String visitUnannType(Java8Parser.UnannTypeContext ctx) {
        return ctx.getText();
    }

    @Override
    public String visitVariableDeclaratorList(Java8Parser.VariableDeclaratorListContext ctx) {
        StringBuilder varDeclSB = new StringBuilder("(");
        for (Java8Parser.VariableDeclaratorContext vdCtx : ctx.variableDeclarator()) {
            varDeclSB.append(visit(vdCtx));
            varDeclSB.append(' ');
        }
        varDeclSB.append(')');
        return varDeclSB.toString();
    }

    /**
     * var-def? := (= (? var-def-id?) (? val-init?))
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitVariableDeclarator(Java8Parser.VariableDeclaratorContext ctx) {
        String varDefIdS = visit(ctx.variableDeclaratorId());
        String valInitS = (ctx.variableInitializer() != null) ? visit(ctx.variableInitializer()) : "()";
        return String.format("(= %s %s)", varDefIdS, valInitS);
    }

    /**
     * var-def-id? := (,name ,(? number? dims))
     * mind array index access semantic, so dims is the dim number of a def
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitVariableDeclaratorId(Java8Parser.VariableDeclaratorIdContext ctx) {
        String nameS = ctx.Identifier().toString();
        long dimsN = 0;
        Java8Parser.DimsContext dimsCtx = ctx.dims();
        if (dimsCtx != null) {
            dimsN = dimsCtx.getText().chars().filter(c -> c == '[').count();
        }
        return String.format("(%s %d)", nameS, dimsN);
    }

    /**
     * constructor? := (Constructor (? *list/c modifier?) (? constructor-decl?) (? throw?)
     * (? *list/c insn?))
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitConstructorDeclaration(Java8Parser.ConstructorDeclarationContext ctx) {
        StringBuilder modSB = new StringBuilder("(");
        for (Java8Parser.ConstructorModifierContext modCtx : ctx.constructorModifier()) {
            modSB.append(modCtx.getText());
            modSB.append(' ');
        }
        modSB.append(')');
        String constructDeclS = visit(ctx.constructorDeclarator());
        String throwS = (ctx.throws_() != null) ? visit(ctx.throws_()) : "()";
        String bodyS = visit(ctx.constructorBody());
        return String.format("(Constructor %s %s %s %s)", modSB.toString(), constructDeclS, throwS, bodyS);
    }

    /***
     * constructor-decl? := (? ,name (? type-parms?) (? *list/c arg?))
     * @param ctx
     * @return
     */
    @Override
    public String visitConstructorDeclarator(Java8Parser.ConstructorDeclaratorContext ctx) {
        String typeParamS = (ctx.typeParameters() != null) ? visit(ctx.typeParameters()) : "()";
        String simpleTypeName = visit(ctx.simpleTypeName());
        String formalParamLS = (ctx.formalParameterList() != null) ? visit(ctx.formalParameterList()) : "()";
        return String.format("(%s %s (%s))", typeParamS, simpleTypeName, formalParamLS);
    }

    @Override
    public String visitSimpleTypeName(Java8Parser.SimpleTypeNameContext ctx) {
        return ctx.getText();
    }

//    @Override
//    public String visitLastFormalParameter(Java8Parser.LastFormalParameterContext ctx) {
//        return
//    }


    /**
     * FormalArg? := (Arg ,modfier ,type ,var)
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitFormalParameter(Java8Parser.FormalParameterContext ctx) {
        StringBuilder modSB = new StringBuilder("(");
        for (var modCtx : ctx.variableModifier()) {
            modSB.append(visit(modCtx));
            modSB.append(' ');
        }
        modSB.append(')');
        String type = visit(ctx.unannType());
        String name = visit(ctx.variableDeclaratorId());
        return String.format("(Arg %s %s %s)", modSB.toString(), type, name);
    }

    /**
     * do not support reciever arg and ... syntax
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitFormalParameters(Java8Parser.FormalParametersContext ctx) {
        StringBuilder fpsSB = new StringBuilder();
        for (var fCtx : ctx.formalParameter()) {
            fpsSB.append(visit(fCtx));
            fpsSB.append(' ');
        }
        return fpsSB.toString();
    }

    @Override
    public String visitFormalParamMulti(Java8Parser.FormalParamMultiContext ctx) {
        String head = visit(ctx.formalParameters());
        String last = visit(ctx.lastFormalParameter());
        return String.format("(%s %s)", head, last);
    }

    /**
     * throws? (Throw . (*list/c excpt?))
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitThrows_(Java8Parser.Throws_Context ctx) {
        String throwsS = visit(ctx.exceptionTypeList());
        return String.format("(Throw %s)", throwsS);
    }

    @Override
    public String visitExceptionTypeList(Java8Parser.ExceptionTypeListContext ctx) {
        StringBuilder exSB = new StringBuilder();
        for (var eCtx : ctx.exceptionType()) {
            exSB.append(visit(eCtx));
            exSB.append(' ');
        }
        return exSB.toString();
    }

    @Override
    public String visitConstructorBody(Java8Parser.ConstructorBodyContext ctx) {
        String consInvocS =
                (ctx.explicitConstructorInvocation() != null) ? visit(ctx.explicitConstructorInvocation()) : "()";
        String body = (ctx.blockStatements() != null) ? visit(ctx.blockStatements()) : "()";
        return String.format("(ConsBody %s %s)", consInvocS, body);
    }

    @Override
    public String visitBlock(Java8Parser.BlockContext ctx) {
        String body = (ctx.blockStatements() != null) ? visit(ctx.blockStatements()) : "()";
        return String.format("(Block %s)", body);
    }

    @Override
    public String visitBlockStatements(Java8Parser.BlockStatementsContext ctx) {
        StringBuilder bSB = new StringBuilder("(");
        for (var bCtx : ctx.blockStatement()) {
            bSB.append(visit(bCtx));
            bSB.append(" ");
        }
        bSB.append(")");
        return bSB.toString();
    }

    @Override
    public String visitLocalVariableDeclarationStatement(Java8Parser.LocalVariableDeclarationStatementContext ctx) {
        return visit(ctx.localVariableDeclaration());
    }

    @Override
    public String visitLocalVariableDeclaration(Java8Parser.LocalVariableDeclarationContext ctx) {
        StringBuilder modSB = new StringBuilder("(");
        for (var mCtx : ctx.variableModifier()) {
            modSB.append(visit(mCtx));
            modSB.append(' ');
        }
        modSB.append(')');
        String type = visit(ctx.unannType());
        String varList = visit(ctx.variableDeclaratorList());
        return String.format("(LocalVar %s %s %s)", modSB.toString(), type, varList);
    }

    // statement? := for-sm? / if-sm? / labeled-sm? / normal-sm? / empty?

    // normal?

    @Override
    public String visitEmptyStatement(Java8Parser.EmptyStatementContext ctx) {
        return "mt ";
    }

    @Override
    public String visitExpressionStatement(Java8Parser.ExpressionStatementContext ctx) {
        return visit(ctx.statementExpression());
    }

    /**
     * assign? := (Assign (? assign-op?) LHS RHS)
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitAssignment(Java8Parser.AssignmentContext ctx) {
        String opS = ctx.assignmentOperator().getText();
        String LHS = visit(ctx.leftHandSide());
        String RHS = visit(ctx.expression());
        return String.format("(Assign %s %s %s)", opS, LHS, RHS);
    }

    // loose some name precision here
    @Override
    public String visitExpressionName(Java8Parser.ExpressionNameContext ctx) {
        return ctx.getText();
    }

    // Expression for now do not support lambda !
//    @Override
//    public String visitExpression(Java8Parser.ExpressionContext ctx) {
//        return super.visitExpression(ctx);
//    }


//    @Override
//    public String visitAssignmentExpression(Java8Parser.AssignmentExpressionContext ctx) {
//        return super.visitAssignmentExpression(ctx);
//    }


    @Override
    public String visitConditionalOrExpression(Java8Parser.ConditionalOrExpressionContext ctx) {
        if (ctx.conditionalOrExpression() != null) {
            String orS = visit(ctx.conditionalOrExpression());
            String andS = visit(ctx.conditionalAndExpression());
            return String.format("(Or %s %s)", orS, andS);
        } else {
            return visit(ctx.conditionalOrExpression());
        }
    }

    @Override
    public String visitConditionalAndExpression(Java8Parser.ConditionalAndExpressionContext ctx) {
        if (ctx.conditionalAndExpression() != null) {
            String andS = visit(ctx.conditionalAndExpression());
            String orInS = visit(ctx.inclusiveOrExpression());
            return String.format("(And %s %s)", andS, orInS);
        } else {
            return visit(ctx.inclusiveOrExpression());
        }
    }

    @Override
    public String visitInclusiveOrExpression(Java8Parser.InclusiveOrExpressionContext ctx) {
        if (ctx.inclusiveOrExpression() != null) {
            String inclusiveOrS = visit(ctx.inclusiveOrExpression());
            String exclusiveAndS = visit(ctx.exclusiveOrExpression());
            return String.format("(InOr %s %s)", inclusiveOrS, exclusiveAndS);
        } else {
            return visit(ctx.exclusiveOrExpression());
        }
    }

    @Override
    public String visitExclusiveOrExpression(Java8Parser.ExclusiveOrExpressionContext ctx) {
        if (ctx.exclusiveOrExpression() != null) {
            String exclusiveOrS = visit(ctx.exclusiveOrExpression());
            String andS = visit(ctx.andExpression());
            return String.format("(ExOr %s %s)", exclusiveOrS, andS);
        } else {
            return visit(ctx.andExpression());
        }
    }

    @Override
    public String visitAndExpression(Java8Parser.AndExpressionContext ctx) {
        if (ctx.andExpression() != null) {
            String andS = visit(ctx.andExpression());
            String equalS = visit(ctx.equalityExpression());
            return String.format("(And %s %s)", andS, equalS);
        } else {
            return visit(ctx.andExpression());
        }
    }

    @Override
    public String visitEqEqExpr(Java8Parser.EqEqExprContext ctx) {
        String eqS = visit(ctx.equalityExpression());
        String relS = visit(ctx.relationalExpression());
        return String.format("(Eq %s %s)", eqS, relS);
    }

    @Override
    public String visitEqNotEqExpr(Java8Parser.EqNotEqExprContext ctx) {
        String eqS = visit(ctx.equalityExpression());
        String relS = visit(ctx.relationalExpression());
        return String.format("(NotEq %s %s)", eqS, relS);
    }

    @Override
    public String visitRelLtExpr(Java8Parser.RelLtExprContext ctx) {
        String relS = visit(ctx.relationalExpression());
        String shiftS = visit(ctx.shiftExpression());
        return String.format("(< %s %s)", relS, shiftS);
    }

    @Override
    public String visitRelGtExpr(Java8Parser.RelGtExprContext ctx) {
        String relS = visit(ctx.relationalExpression());
        String shiftS = visit(ctx.shiftExpression());
        return String.format("(> %s %s)", relS, shiftS);
    }

    @Override
    public String visitRelLeExpr(Java8Parser.RelLeExprContext ctx) {
        String relS = visit(ctx.relationalExpression());
        String shiftS = visit(ctx.shiftExpression());
        return String.format("(<= %s %s)", relS, shiftS);
    }

    @Override
    public String visitRelGeExpr(Java8Parser.RelGeExprContext ctx) {
        String relS = visit(ctx.relationalExpression());
        String shiftS = visit(ctx.shiftExpression());
        return String.format("(>= %s %s)", relS, shiftS);
    }

    @Override
    public String visitRelInstExpr(Java8Parser.RelInstExprContext ctx) {
        String relS = visit(ctx.relationalExpression());
        String refS = visit(ctx.referenceType());
        return String.format("(instanceof %s %s)", relS, refS);
    }

    @Override
    public String visitShiftLeftExpr(Java8Parser.ShiftLeftExprContext ctx) {
        String shiftS = visit(ctx.shiftExpression());
        String addS = visit(ctx.additiveExpression());
        return String.format("(<< %s %s)", shiftS, addS);
    }

    @Override
    public String visitShiftRightExpr(Java8Parser.ShiftRightExprContext ctx) {
        String shiftS = visit(ctx.shiftExpression());
        String addS = visit(ctx.additiveExpression());
        return String.format("(>> %s %s)", shiftS, addS);
    }

    @Override
    public String visitShiftUnsignLeftExpr(Java8Parser.ShiftUnsignLeftExprContext ctx) {
        String shiftS = visit(ctx.shiftExpression());
        String addS = visit(ctx.additiveExpression());
        return String.format("(>>> %s %s)", shiftS, addS);
    }

    @Override
    public String visitAddAddExpr(Java8Parser.AddAddExprContext ctx) {
        String addS = visit(ctx.additiveExpression());
        String multS = visit(ctx.multiplicativeExpression());
        return String.format("(+ %s %s)", addS, multS);
    }

    @Override
    public String visitAddSubExpr(Java8Parser.AddSubExprContext ctx) {
        String addS = visit(ctx.additiveExpression());
        String multS = visit(ctx.multiplicativeExpression());
        return String.format("(- %s %s)", addS, multS);
    }

    @Override
    public String visitMultMultExpr(Java8Parser.MultMultExprContext ctx) {
        String multS = visit(ctx.multiplicativeExpression());
        String unaryS = visit(ctx.unaryExpression());
        return String.format("(* %s %s)", multS, unaryS);
    }

    @Override
    public String visitMultDivExpr(Java8Parser.MultDivExprContext ctx) {
        String multS = visit(ctx.multiplicativeExpression());
        String unaryS = visit(ctx.unaryExpression());
        return String.format("(/ %s %s)", multS, unaryS);
    }

    @Override
    public String visitMultModExpr(Java8Parser.MultModExprContext ctx) {
        String multS = visit(ctx.multiplicativeExpression());
        String unaryS = visit(ctx.unaryExpression());
        return String.format("(Mod %s %s)", multS, unaryS);
    }

    @Override
    public String visitPreIncrementExpression(Java8Parser.PreIncrementExpressionContext ctx) {
        String unaryS = visit(ctx.unaryExpression());
        return String.format("(Add1 %s)", unaryS);
    }

    @Override
    public String visitPreDecrementExpression(Java8Parser.PreDecrementExpressionContext ctx) {
        String unaryS = visit(ctx.unaryExpression());
        return String.format("(Sub1 %s)", unaryS);
    }

    @Override
    public String visitUnaryPosExpr(Java8Parser.UnaryPosExprContext ctx) {
        return visit(ctx.unaryExpression());
    }

    @Override
    public String visitUnaryNegExpr(Java8Parser.UnaryNegExprContext ctx) {
        String unaryS = visit(ctx.unaryExpression());
        return String.format("(- %s)", unaryS);
    }

    @Override
    public String visitUnaryBitNotExpr(Java8Parser.UnaryBitNotExprContext ctx) {
        String unaryS = visit(ctx.unaryExpression());
        return String.format("(BitNot %s)", unaryS);
    }

    @Override
    public String visitUnaryNotExpr(Java8Parser.UnaryNotExprContext ctx) {
        String unaryS = visit(ctx.unaryExpression());
        return String.format("(Not %s)", unaryS);
    }

    @Override
    public String visitPostDecrementExpression(Java8Parser.PostDecrementExpressionContext ctx) {
        String unaryS = visit(ctx.postfixExpression());
        return String.format("(PSub1 %s)", unaryS);
    }

    @Override
    public String visitPostIncrementExpression(Java8Parser.PostIncrementExpressionContext ctx) {
        String unaryS = visit(ctx.postfixExpression());
        return String.format("(PAdd1 %s)", unaryS);
    }

    @Override
    public String visitPostfixExpression(Java8Parser.PostfixExpressionContext ctx) {
        String eS = (ctx.primary() != null) ? visit(ctx.primary()) : visit(ctx.expressionName());
        int count = 0;
        if (ctx.postDecrementExpression_lf_postfixExpression() != null) {
            count += ctx.postDecrementExpression_lf_postfixExpression().size();
        } else if (ctx.postIncrementExpression_lf_postfixExpression() != null) {
            count += ctx.postIncrementExpression_lf_postfixExpression().size();
        } else {
            return eS;
        }
        return String.format("(+ %d %s)", count, eS);
    }

    // actually in my analysis type won't help ..... and won't use lambda
    @Override
    public String visitCastPrimExpr(Java8Parser.CastPrimExprContext ctx) {
        String primS = visit(ctx.primitiveType());
        String unaryS = visit(ctx.unaryExpression());
        return String.format("(Cast %s %s)", primS, unaryS);
    }

    /**
     * cast? := (Cast type? expr?)
     * assume no additional bound
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitCastTypeExpr(Java8Parser.CastTypeExprContext ctx) {
        String refS = visit(ctx.referenceType());
        String expS = visit(ctx.unaryExpressionNotPlusMinus());
        return String.format("(Cast %s %s)", refS, expS);
    }

    @Override
    public String visitPrimFieldAccess(Java8Parser.PrimFieldAccessContext ctx) {
        return super.visitPrimFieldAccess(ctx);
    }

    @Override
    public String visitLiteral(Java8Parser.LiteralContext ctx) {
        return ctx.getText();
    }

    /**
     * using text of a type, this will lose some precision in Type and maybe alloc function
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitTypeName(Java8Parser.TypeNameContext ctx) {
        return ctx.getText();
    }

    @Override
    public String visitPrimaryNoNewArray_lfno_primaryLit(Java8Parser.PrimaryNoNewArray_lfno_primaryLitContext ctx) {
        return visit(ctx.literal());
    }

    @Override
    public String visitPrimaryNoNewArray_lfno_primaryRefl(Java8Parser.PrimaryNoNewArray_lfno_primaryReflContext ctx) {
        String typeS = visit(ctx.typeName());
        return String.format("(Refl [] %s)", typeS);
    }

    @Override
    public String visitPrimaryNoNewArray_lfno_primaryArrayRefl(
            Java8Parser.PrimaryNoNewArray_lfno_primaryArrayReflContext ctx) {
        String typeS = visit(ctx.unannPrimitiveType());
        return String.format("(Refl [] %s)", typeS);
    }

    @Override
    public String visitPrimaryNoNewArray_lfno_primaryVRefl(
            Java8Parser.PrimaryNoNewArray_lfno_primaryVReflContext ctx) {
        return "(Refl [] void)";
    }

    @Override
    public String visitPrimaryNoNewArray_lfno_primarySelf(
            Java8Parser.PrimaryNoNewArray_lfno_primarySelfContext ctx) {
        return "(THIS)";
    }

    @Override
    public String visitPrimaryNoNewArray_lfno_primaryClassSelf(
            Java8Parser.PrimaryNoNewArray_lfno_primaryClassSelfContext ctx) {
        String typeS = visit(ctx.typeName());
        return String.format("(THIS %s)", typeS);
    }

    @Override
    public String visitPrimaryNoNewArray_lfno_primaryParen(
            Java8Parser.PrimaryNoNewArray_lfno_primaryParenContext ctx) {
        String subS = visit(ctx.expression());
        return String.format("(%s)", subS);
    }

    /**
     * New? := (New ,class-name typeArgs? Args? Body?)
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitClassInstanceCreationExpression_lfno_primary(
            Java8Parser.ClassInstanceCreationExpression_lfno_primaryContext ctx) {
        String nameS = ctx.Identifier(0).getText();
        String typeArgS = (ctx.typeArgumentsOrDiamond() != null) ? visit(ctx.typeArgumentsOrDiamond()) : "()";
        String argS = (ctx.argumentList() != null) ? visit(ctx.argumentList()) : "()";
        String classBody = (ctx.classBody() != null) ? visit(ctx.classBody()) : "()";
        return String.format("(New %s %s %s %s)", nameS, typeArgS, argS, classBody);
    }

    @Override
    public String visitClassInstanceCreationExpression(Java8Parser.ClassInstanceCreationExpressionContext ctx) {
        String nameS = ctx.Identifier(0).getText();
        String typeArgS = (ctx.typeArgumentsOrDiamond() != null) ? visit(ctx.typeArgumentsOrDiamond()) : "()";
        String argS = (ctx.argumentList() != null) ? visit(ctx.argumentList()) : "()";
        String classBody = (ctx.classBody() != null) ? visit(ctx.classBody()) : "()";
        return String.format("(New %s %s %s %s)", nameS, typeArgS, argS, classBody);
    }

    // TODO: too casual here cause inconsistence of generic.....
    @Override
    public String visitTypeArgumentsOrDiamond(Java8Parser.TypeArgumentsOrDiamondContext ctx) {
        return ctx.getText();
    }

    /**
     * Arg?  list of arg
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitArgumentList(Java8Parser.ArgumentListContext ctx) {
        StringBuilder argsSB = new StringBuilder("(");
        for (var argCtx : ctx.expression()) {
            argsSB.append(visit(argCtx));
            argsSB.append(' ');
        }
        argsSB.append(')');
        return String.format("(Args %s)", argsSB.toString());
    }

    @Override
    public String visitFieldAccess_lfno_primary(Java8Parser.FieldAccess_lfno_primaryContext ctx) {
        return String.format("(Super %s)", ctx.Identifier().getText());
    }

    /**
     * TODO: support more dims array access
     * array-access? (ArrayAccess name (list? exprs))
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitArrayAccess_lfno_primary(Java8Parser.ArrayAccess_lfno_primaryContext ctx) {
        String exprNameS = visit(ctx.expressionName());
        StringBuilder exprSB = new StringBuilder("(");
        for (var expCtx : ctx.expression()) {
            exprSB.append(visit(expCtx));
            exprSB.append(' ');
        }
        exprSB.append(')');
        return String.format("(ArrayAccess %s %s)", exprNameS, exprSB.toString());
    }

    @Override
    public String visitArrayAccess(Java8Parser.ArrayAccessContext ctx) {
        String exprNameS = visit(ctx.expressionName());
        StringBuilder exprSB = new StringBuilder("(");
        for (var expCtx : ctx.expression()) {
            exprSB.append(visit(expCtx));
            exprSB.append(' ');
        }
        exprSB.append(')');
        return String.format("(ArrayAccess %s %s)", exprNameS, exprSB.toString());
    }

    // vague for type here
    @Override
    public String visitClassOrInterfaceType(Java8Parser.ClassOrInterfaceTypeContext ctx) {
        StringBuilder typeListS = new StringBuilder("(");
        String headS = "";
        if (ctx.classType_lfno_classOrInterfaceType() != null) {
            headS = visit(ctx.classType_lfno_classOrInterfaceType());
        } else {
            headS = visit(ctx.interfaceType_lfno_classOrInterfaceType());
        }
        for (var classTCtx : ctx.classType_lf_classOrInterfaceType()) {
            typeListS.append(classTCtx);
            typeListS.append(' ');
        }
        for (var interfaceTCtx : ctx.interfaceType_lf_classOrInterfaceType()) {
            typeListS.append(visit(interfaceTCtx));
            typeListS.append(' ');
        }
        typeListS.append(')');
        return String.format("(Types %s)", typeListS);
    }

    @Override
    public String visitClassType_lfno_classOrInterfaceType(Java8Parser.ClassType_lfno_classOrInterfaceTypeContext ctx) {
        String typeNames = ctx.Identifier().getText();
        String typeArgs = (ctx.typeArguments() != null) ? visit(ctx.typeArguments()) : "()";
        return String.format("(%s %s)", typeNames, typeArgs);
    }

    @Override
    public String visitImplicitMethodInvoc(Java8Parser.ImplicitMethodInvocContext ctx) {
        String nameS = visit(ctx.methodName());
        String argsS = (ctx.argumentList() != null) ? visit(ctx.argumentList()) : "()";
        return String.format("(MethodInvoc %s %s)", nameS, argsS);
    }

    @Override
    public String visitMethodName(Java8Parser.MethodNameContext ctx) {
        return ctx.getText();
    }

    @Override
    public String visitStaticMethodInvoc(Java8Parser.StaticMethodInvocContext ctx) {
        String typeS = visit(ctx.typeName());
        String nameS = ctx.Identifier().getText();
        String argsS = (ctx.argumentList() != null) ? visit(ctx.argumentList()) : "()";
        return String.format("(MethodInvoc %s %s %s)", typeS, nameS, argsS);
    }

    @Override
    public String visitExprMethodInvoc(Java8Parser.ExprMethodInvocContext ctx) {
        String typeS = visit(ctx.expressionName());
        String nameS = ctx.Identifier().getText();
        String argsS = (ctx.argumentList() != null) ? visit(ctx.argumentList()) : "()";
        return String.format("(MethodInvoc %s %s %s)", typeS, nameS, argsS);
    }

    @Override
    public String visitMemberMethodInvoc(Java8Parser.MemberMethodInvocContext ctx) {
        String typeS = visit(ctx.primary());
        String nameS = ctx.Identifier().getText();
        String argsS = (ctx.argumentList() != null) ? visit(ctx.argumentList()) : "()";
        return String.format("(MethodInvoc %s %s %s)", typeS, nameS, argsS);
    }

    @Override
    public String visitSuperMethodInvoc(Java8Parser.SuperMethodInvocContext ctx) {
        String nameS = ctx.Identifier().getText();
        String argsS = (ctx.argumentList() != null) ? visit(ctx.argumentList()) : "()";
        return String.format("(MethodInvoc Super %s %s)", nameS, argsS);
    }

    @Override
    public String visitAssertOneStatement(Java8Parser.AssertOneStatementContext ctx) {
        String exprS = visit(ctx.expression());
        return String.format("(Assert %s)", exprS);
    }

    @Override
    public String visitAssertTwoStatement(Java8Parser.AssertTwoStatementContext ctx) {
        String exprS1 = visit(ctx.expression(0));
        String exprS2 = visit(ctx.expression(1));
        return String.format("(Assert %s %s)", exprS1, exprS2);
    }

    /**
     * switch? := (Switch cond? (list case?))
     *
     * @param ctx
     * @return
     */
    @Override
    public String visitSwitchStatement(Java8Parser.SwitchStatementContext ctx) {
        String guardS = visit(ctx.expression());
        String blockS = visit(ctx.switchBlock());
        return String.format("(Switch %s %s)", guardS, blockS);
    }

    // only take care of useful label
    @Override
    public String visitSwitchBlock(Java8Parser.SwitchBlockContext ctx) {
        StringBuilder swSB = new StringBuilder("(");
        for (var sbCtx : ctx.switchBlockStatementGroup()) {
            swSB.append(visit(sbCtx));
            swSB.append('\n');
        }
        swSB.append(")\n");
        return swSB.toString();
    }

    @Override
    public String visitSwitchBlockStatementGroup(Java8Parser.SwitchBlockStatementGroupContext ctx) {
        String labelS = visit(ctx.switchLabels());
        String statementsS = visit(ctx.blockStatements());
        return String.format("(%s %s)", labelS, statementsS);
    }

    @Override
    public String visitSwitchLabels(Java8Parser.SwitchLabelsContext ctx) {
        StringBuilder labelSB = new StringBuilder("(");
        for (var lCtx : ctx.switchLabel()) {
            labelSB.append(visit(lCtx));
            labelSB.append(' ');
        }
        labelSB.append(')');
        return labelSB.toString();
    }

    @Override
    public String visitSwitchLabelConst(Java8Parser.SwitchLabelConstContext ctx) {
        return String.format("(Case %s)", visit(ctx.constantExpression()));
    }

    @Override
    public String visitSwitchLabelDefault(Java8Parser.SwitchLabelDefaultContext ctx) {
        return "(Default)";
    }

    @Override
    public String visitSwitchLabelEnum(Java8Parser.SwitchLabelEnumContext ctx) {
        return String.format("(Case %s)", visit(ctx.enumConstantName()));
    }

    @Override
    public String visitEnumConstantName(Java8Parser.EnumConstantNameContext ctx) {
        return ctx.Identifier().getText();
    }

    /**
     * do-while? := (Do guard? block?)
     * @param ctx
     * @return
     */
    @Override
    public String visitDoStatement(Java8Parser.DoStatementContext ctx) {
        String guardS = visit(ctx.expression());
        String blockS = visit(ctx.statement());
        return String.format("(Do %s %s)", guardS, blockS);
    }

    @Override
    public String visitBreakStatement(Java8Parser.BreakStatementContext ctx) {
        return "Break";
    }

    @Override
    public String visitContinueStatement(Java8Parser.ContinueStatementContext ctx) {
        return "Continue";
    }

    @Override
    public String visitReturnStatement(Java8Parser.ReturnStatementContext ctx) {
        return String.format("(Return %s)", visit(ctx.expression()));
    }

    // TODO: synchronizedStatement

    @Override
    public String visitThrowStatement(Java8Parser.ThrowStatementContext ctx) {
        return String.format("(Throw %s)", visit(ctx.expression()));
    }

    @Override
    public String visitTryCatchStatement(Java8Parser.TryCatchStatementContext ctx) {
        String blockS = visit(ctx.block());
        String catchS = visit(ctx.catches());
        return String.format("(Try %s %s)", blockS, catchS);
    }

    @Override
    public String visitTryCatchFinalStatement(Java8Parser.TryCatchFinalStatementContext ctx) {
        String blockS = visit(ctx.block());
        String catchS = visit(ctx.catches());
        String finalS = visit(ctx.finally_());
        return String.format("(Try %s %s %s)", blockS, catchS, finalS);
    }

    @Override
    public String visitCatches(Java8Parser.CatchesContext ctx) {
        StringBuilder catchSB = new StringBuilder("(");
        for (var catCtx : ctx.catchClause()) {
            catchSB.append(visit(catCtx));
            catchSB.append(' ');
        }
        catchSB.append(')');
        return catchSB.toString();
    }

    @Override
    public String visitCatchClause(Java8Parser.CatchClauseContext ctx) {
        String argS = visit(ctx.catchFormalParameter());
        String blockS = visit(ctx.block());
        return String.format("(Catch %s %s)", argS, blockS);
    }

    @Override
    public String visitCatchFormalParameter(Java8Parser.CatchFormalParameterContext ctx) {
        StringBuilder modSB = new StringBuilder("(");
        for (var modCtx : ctx.variableModifier()) {
            modSB.append(visit(modCtx));
            modSB.append(' ');
        }
        modSB.append(')');
        String typeS = visit(ctx.catchType());
        String name = visit(ctx.variableDeclaratorId());
        return String.format("(Arg %s %s %s)", modSB.toString(), typeS, name);
    }

    @Override
    public String visitFinally_(Java8Parser.Finally_Context ctx) {
        return String.format("(Finally %s)", ctx.block());
    }
}







