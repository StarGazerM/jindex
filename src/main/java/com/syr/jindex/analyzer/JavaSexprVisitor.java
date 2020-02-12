/***
 * a visitor generate s-expr from JAVA AST
 * for simple, won't collect  Java annotation info, cause it is has no semantic operation during runtime
 *
 * Yihao Sun <email>syun67@syr.edu</email>
 * Syracuse 2020
 */

package com.syr.jindex.analyzer;

import com.syr.jindex.parser.Java8Parser;
import com.syr.jindex.parser.Java8ParserBaseVisitor;

import java.util.List;

public class JavaSexprVisitor extends Java8ParserBaseVisitor<String> {

//    StringBuilder sexprbuilder;
//
//    public JavaSexprVisitor() {
//        this.sexprbuilder = new StringBuilder("");
//    }


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
        return String.format("((%s) %s)", ctx.Identifier().getText(), typeArgS);
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
    public String visitUnannPrimitiveType(Java8Parser.UnannPrimitiveTypeContext ctx) {
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


    //    @Override
//    public String visitClassMemberDeclaration(Java8Parser.ClassMemberDeclarationContext ctx) {
//
//    }
}
