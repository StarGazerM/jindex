// Generated from /home/stargazermiao/workspace/PL/CSE687/jindex/src/main/antlr/Java8Parser.g4 by ANTLR 4.8
package com.syr.jindex.parser;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link Java8Parser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface Java8ParserVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link Java8Parser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteral(Java8Parser.LiteralContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#primitiveType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimitiveType(Java8Parser.PrimitiveTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#numericType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumericType(Java8Parser.NumericTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#integralType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIntegralType(Java8Parser.IntegralTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#floatingPointType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFloatingPointType(Java8Parser.FloatingPointTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#referenceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReferenceType(Java8Parser.ReferenceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#classOrInterfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassOrInterfaceType(Java8Parser.ClassOrInterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ShortClassType}
	 * labeled alternative in {@link Java8Parser#classType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitShortClassType(Java8Parser.ShortClassTypeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code LongClassType}
	 * labeled alternative in {@link Java8Parser#classType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLongClassType(Java8Parser.LongClassTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#classType_lf_classOrInterfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassType_lf_classOrInterfaceType(Java8Parser.ClassType_lf_classOrInterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#classType_lfno_classOrInterfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassType_lfno_classOrInterfaceType(Java8Parser.ClassType_lfno_classOrInterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#interfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInterfaceType(Java8Parser.InterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#interfaceType_lf_classOrInterfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInterfaceType_lf_classOrInterfaceType(Java8Parser.InterfaceType_lf_classOrInterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#interfaceType_lfno_classOrInterfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInterfaceType_lfno_classOrInterfaceType(Java8Parser.InterfaceType_lfno_classOrInterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeVariable}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeVariable(Java8Parser.TypeVariableContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#arrayType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayType(Java8Parser.ArrayTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#dims}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDims(Java8Parser.DimsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeParameter(Java8Parser.TypeParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeParameterModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeParameterModifier(Java8Parser.TypeParameterModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeBound}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeBound(Java8Parser.TypeBoundContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#additionalBound}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAdditionalBound(Java8Parser.AdditionalBoundContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeArguments}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeArguments(Java8Parser.TypeArgumentsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeArgumentList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeArgumentList(Java8Parser.TypeArgumentListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeArgument}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeArgument(Java8Parser.TypeArgumentContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#wildcard}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWildcard(Java8Parser.WildcardContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#wildcardBounds}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWildcardBounds(Java8Parser.WildcardBoundsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#packageName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPackageName(Java8Parser.PackageNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeName(Java8Parser.TypeNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#packageOrTypeName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPackageOrTypeName(Java8Parser.PackageOrTypeNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#expressionName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpressionName(Java8Parser.ExpressionNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#methodName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethodName(Java8Parser.MethodNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#ambiguousName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAmbiguousName(Java8Parser.AmbiguousNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#compilationUnit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCompilationUnit(Java8Parser.CompilationUnitContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#packageDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPackageDeclaration(Java8Parser.PackageDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#packageModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPackageModifier(Java8Parser.PackageModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#importDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImportDeclaration(Java8Parser.ImportDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#singleTypeImportDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSingleTypeImportDeclaration(Java8Parser.SingleTypeImportDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeImportOnDemandDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeImportOnDemandDeclaration(Java8Parser.TypeImportOnDemandDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#singleStaticImportDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSingleStaticImportDeclaration(Java8Parser.SingleStaticImportDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#staticImportOnDemandDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStaticImportOnDemandDeclaration(Java8Parser.StaticImportOnDemandDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeDeclaration(Java8Parser.TypeDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#classDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassDeclaration(Java8Parser.ClassDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#normalClassDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNormalClassDeclaration(Java8Parser.NormalClassDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#classModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassModifier(Java8Parser.ClassModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeParameters}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeParameters(Java8Parser.TypeParametersContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeParameterList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeParameterList(Java8Parser.TypeParameterListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#superclass}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSuperclass(Java8Parser.SuperclassContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#superinterfaces}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSuperinterfaces(Java8Parser.SuperinterfacesContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#interfaceTypeList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInterfaceTypeList(Java8Parser.InterfaceTypeListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#classBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassBody(Java8Parser.ClassBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#classBodyDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassBodyDeclaration(Java8Parser.ClassBodyDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#classMemberDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassMemberDeclaration(Java8Parser.ClassMemberDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#fieldDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFieldDeclaration(Java8Parser.FieldDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#fieldModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFieldModifier(Java8Parser.FieldModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#variableDeclaratorList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariableDeclaratorList(Java8Parser.VariableDeclaratorListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#variableDeclarator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariableDeclarator(Java8Parser.VariableDeclaratorContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#variableDeclaratorId}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariableDeclaratorId(Java8Parser.VariableDeclaratorIdContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#variableInitializer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariableInitializer(Java8Parser.VariableInitializerContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannType(Java8Parser.UnannTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannPrimitiveType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannPrimitiveType(Java8Parser.UnannPrimitiveTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannReferenceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannReferenceType(Java8Parser.UnannReferenceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannClassOrInterfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannClassOrInterfaceType(Java8Parser.UnannClassOrInterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannClassType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannClassType(Java8Parser.UnannClassTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannClassType_lf_unannClassOrInterfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannClassType_lf_unannClassOrInterfaceType(Java8Parser.UnannClassType_lf_unannClassOrInterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannClassType_lfno_unannClassOrInterfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannClassType_lfno_unannClassOrInterfaceType(Java8Parser.UnannClassType_lfno_unannClassOrInterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannInterfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannInterfaceType(Java8Parser.UnannInterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannInterfaceType_lf_unannClassOrInterfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannInterfaceType_lf_unannClassOrInterfaceType(Java8Parser.UnannInterfaceType_lf_unannClassOrInterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannInterfaceType_lfno_unannClassOrInterfaceType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannInterfaceType_lfno_unannClassOrInterfaceType(Java8Parser.UnannInterfaceType_lfno_unannClassOrInterfaceTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannTypeVariable}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannTypeVariable(Java8Parser.UnannTypeVariableContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#unannArrayType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnannArrayType(Java8Parser.UnannArrayTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#methodDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethodDeclaration(Java8Parser.MethodDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#methodModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethodModifier(Java8Parser.MethodModifierContext ctx);
	/**
	 * Visit a parse tree produced by the {@code NormalMethodHeader}
	 * labeled alternative in {@link Java8Parser#methodHeader}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNormalMethodHeader(Java8Parser.NormalMethodHeaderContext ctx);
	/**
	 * Visit a parse tree produced by the {@code GenericMethodHeader}
	 * labeled alternative in {@link Java8Parser#methodHeader}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGenericMethodHeader(Java8Parser.GenericMethodHeaderContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#result}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitResult(Java8Parser.ResultContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#methodDeclarator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethodDeclarator(Java8Parser.MethodDeclaratorContext ctx);
	/**
	 * Visit a parse tree produced by the {@code FormalParamRecv}
	 * labeled alternative in {@link Java8Parser#formalParameterList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFormalParamRecv(Java8Parser.FormalParamRecvContext ctx);
	/**
	 * Visit a parse tree produced by the {@code FormalParamMulti}
	 * labeled alternative in {@link Java8Parser#formalParameterList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFormalParamMulti(Java8Parser.FormalParamMultiContext ctx);
	/**
	 * Visit a parse tree produced by the {@code FormalParamOne}
	 * labeled alternative in {@link Java8Parser#formalParameterList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFormalParamOne(Java8Parser.FormalParamOneContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#formalParameters}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFormalParameters(Java8Parser.FormalParametersContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#formalParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFormalParameter(Java8Parser.FormalParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#variableModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariableModifier(Java8Parser.VariableModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#lastFormalParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLastFormalParameter(Java8Parser.LastFormalParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#receiverParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReceiverParameter(Java8Parser.ReceiverParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#throws_}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitThrows_(Java8Parser.Throws_Context ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#exceptionTypeList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExceptionTypeList(Java8Parser.ExceptionTypeListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#exceptionType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExceptionType(Java8Parser.ExceptionTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#methodBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethodBody(Java8Parser.MethodBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#instanceInitializer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInstanceInitializer(Java8Parser.InstanceInitializerContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#staticInitializer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStaticInitializer(Java8Parser.StaticInitializerContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#constructorDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstructorDeclaration(Java8Parser.ConstructorDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#constructorModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstructorModifier(Java8Parser.ConstructorModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#constructorDeclarator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstructorDeclarator(Java8Parser.ConstructorDeclaratorContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#simpleTypeName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSimpleTypeName(Java8Parser.SimpleTypeNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#constructorBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstructorBody(Java8Parser.ConstructorBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#explicitConstructorInvocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExplicitConstructorInvocation(Java8Parser.ExplicitConstructorInvocationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#enumDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnumDeclaration(Java8Parser.EnumDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#enumBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnumBody(Java8Parser.EnumBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#enumConstantList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnumConstantList(Java8Parser.EnumConstantListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#enumConstant}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnumConstant(Java8Parser.EnumConstantContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#enumConstantModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnumConstantModifier(Java8Parser.EnumConstantModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#enumBodyDeclarations}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnumBodyDeclarations(Java8Parser.EnumBodyDeclarationsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#interfaceDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInterfaceDeclaration(Java8Parser.InterfaceDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#normalInterfaceDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNormalInterfaceDeclaration(Java8Parser.NormalInterfaceDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#interfaceModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInterfaceModifier(Java8Parser.InterfaceModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#extendsInterfaces}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExtendsInterfaces(Java8Parser.ExtendsInterfacesContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#interfaceBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInterfaceBody(Java8Parser.InterfaceBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#interfaceMemberDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInterfaceMemberDeclaration(Java8Parser.InterfaceMemberDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#constantDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstantDeclaration(Java8Parser.ConstantDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#constantModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstantModifier(Java8Parser.ConstantModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#interfaceMethodDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInterfaceMethodDeclaration(Java8Parser.InterfaceMethodDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#interfaceMethodModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInterfaceMethodModifier(Java8Parser.InterfaceMethodModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#annotationTypeDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotationTypeDeclaration(Java8Parser.AnnotationTypeDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#annotationTypeBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotationTypeBody(Java8Parser.AnnotationTypeBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#annotationTypeMemberDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotationTypeMemberDeclaration(Java8Parser.AnnotationTypeMemberDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#annotationTypeElementDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotationTypeElementDeclaration(Java8Parser.AnnotationTypeElementDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#annotationTypeElementModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotationTypeElementModifier(Java8Parser.AnnotationTypeElementModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#defaultValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDefaultValue(Java8Parser.DefaultValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#annotation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotation(Java8Parser.AnnotationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#normalAnnotation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNormalAnnotation(Java8Parser.NormalAnnotationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#elementValuePairList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElementValuePairList(Java8Parser.ElementValuePairListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#elementValuePair}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElementValuePair(Java8Parser.ElementValuePairContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#elementValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElementValue(Java8Parser.ElementValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#elementValueArrayInitializer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElementValueArrayInitializer(Java8Parser.ElementValueArrayInitializerContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#elementValueList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitElementValueList(Java8Parser.ElementValueListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#markerAnnotation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMarkerAnnotation(Java8Parser.MarkerAnnotationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#singleElementAnnotation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSingleElementAnnotation(Java8Parser.SingleElementAnnotationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#arrayInitializer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayInitializer(Java8Parser.ArrayInitializerContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#variableInitializerList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariableInitializerList(Java8Parser.VariableInitializerListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#block}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlock(Java8Parser.BlockContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#blockStatements}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlockStatements(Java8Parser.BlockStatementsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#blockStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlockStatement(Java8Parser.BlockStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#localVariableDeclarationStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLocalVariableDeclarationStatement(Java8Parser.LocalVariableDeclarationStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#localVariableDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLocalVariableDeclaration(Java8Parser.LocalVariableDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatement(Java8Parser.StatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#statementNoShortIf}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatementNoShortIf(Java8Parser.StatementNoShortIfContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#statementWithoutTrailingSubstatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatementWithoutTrailingSubstatement(Java8Parser.StatementWithoutTrailingSubstatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#emptyStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEmptyStatement(Java8Parser.EmptyStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#labeledStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLabeledStatement(Java8Parser.LabeledStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#labeledStatementNoShortIf}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLabeledStatementNoShortIf(Java8Parser.LabeledStatementNoShortIfContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#expressionStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpressionStatement(Java8Parser.ExpressionStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#statementExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatementExpression(Java8Parser.StatementExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#ifThenStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfThenStatement(Java8Parser.IfThenStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#ifThenElseStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfThenElseStatement(Java8Parser.IfThenElseStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#ifThenElseStatementNoShortIf}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfThenElseStatementNoShortIf(Java8Parser.IfThenElseStatementNoShortIfContext ctx);
	/**
	 * Visit a parse tree produced by the {@code AssertOneStatement}
	 * labeled alternative in {@link Java8Parser#assertStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssertOneStatement(Java8Parser.AssertOneStatementContext ctx);
	/**
	 * Visit a parse tree produced by the {@code AssertTwoStatement}
	 * labeled alternative in {@link Java8Parser#assertStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssertTwoStatement(Java8Parser.AssertTwoStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#switchStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSwitchStatement(Java8Parser.SwitchStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#switchBlock}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSwitchBlock(Java8Parser.SwitchBlockContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#switchBlockStatementGroup}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSwitchBlockStatementGroup(Java8Parser.SwitchBlockStatementGroupContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#switchLabels}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSwitchLabels(Java8Parser.SwitchLabelsContext ctx);
	/**
	 * Visit a parse tree produced by the {@code SwitchLabelConst}
	 * labeled alternative in {@link Java8Parser#switchLabel}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSwitchLabelConst(Java8Parser.SwitchLabelConstContext ctx);
	/**
	 * Visit a parse tree produced by the {@code SwitchLabelEnum}
	 * labeled alternative in {@link Java8Parser#switchLabel}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSwitchLabelEnum(Java8Parser.SwitchLabelEnumContext ctx);
	/**
	 * Visit a parse tree produced by the {@code SwitchLabelDefault}
	 * labeled alternative in {@link Java8Parser#switchLabel}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSwitchLabelDefault(Java8Parser.SwitchLabelDefaultContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#enumConstantName}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnumConstantName(Java8Parser.EnumConstantNameContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#whileStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWhileStatement(Java8Parser.WhileStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#whileStatementNoShortIf}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWhileStatementNoShortIf(Java8Parser.WhileStatementNoShortIfContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#doStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDoStatement(Java8Parser.DoStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#forStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForStatement(Java8Parser.ForStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#forStatementNoShortIf}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForStatementNoShortIf(Java8Parser.ForStatementNoShortIfContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#basicForStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBasicForStatement(Java8Parser.BasicForStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#basicForStatementNoShortIf}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBasicForStatementNoShortIf(Java8Parser.BasicForStatementNoShortIfContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#forInit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForInit(Java8Parser.ForInitContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#forUpdate}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForUpdate(Java8Parser.ForUpdateContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#statementExpressionList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatementExpressionList(Java8Parser.StatementExpressionListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#enhancedForStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnhancedForStatement(Java8Parser.EnhancedForStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#enhancedForStatementNoShortIf}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEnhancedForStatementNoShortIf(Java8Parser.EnhancedForStatementNoShortIfContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#breakStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBreakStatement(Java8Parser.BreakStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#continueStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitContinueStatement(Java8Parser.ContinueStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#returnStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReturnStatement(Java8Parser.ReturnStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#throwStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitThrowStatement(Java8Parser.ThrowStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#synchronizedStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSynchronizedStatement(Java8Parser.SynchronizedStatementContext ctx);
	/**
	 * Visit a parse tree produced by the {@code TryCatchStatement}
	 * labeled alternative in {@link Java8Parser#tryStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTryCatchStatement(Java8Parser.TryCatchStatementContext ctx);
	/**
	 * Visit a parse tree produced by the {@code TryCatchFinalStatement}
	 * labeled alternative in {@link Java8Parser#tryStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTryCatchFinalStatement(Java8Parser.TryCatchFinalStatementContext ctx);
	/**
	 * Visit a parse tree produced by the {@code TryResourceStatement}
	 * labeled alternative in {@link Java8Parser#tryStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTryResourceStatement(Java8Parser.TryResourceStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#catches}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCatches(Java8Parser.CatchesContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#catchClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCatchClause(Java8Parser.CatchClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#catchFormalParameter}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCatchFormalParameter(Java8Parser.CatchFormalParameterContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#catchType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCatchType(Java8Parser.CatchTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#finally_}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFinally_(Java8Parser.Finally_Context ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#tryWithResourcesStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTryWithResourcesStatement(Java8Parser.TryWithResourcesStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#resourceSpecification}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitResourceSpecification(Java8Parser.ResourceSpecificationContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#resourceList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitResourceList(Java8Parser.ResourceListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#resource}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitResource(Java8Parser.ResourceContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimary(Java8Parser.PrimaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#primaryNoNewArray}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray(Java8Parser.PrimaryNoNewArrayContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#primaryNoNewArray_lf_arrayAccess}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lf_arrayAccess(Java8Parser.PrimaryNoNewArray_lf_arrayAccessContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#primaryNoNewArray_lfno_arrayAccess}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_arrayAccess(Java8Parser.PrimaryNoNewArray_lfno_arrayAccessContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#primaryNoNewArray_lf_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lf_primary(Java8Parser.PrimaryNoNewArray_lf_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#primaryNoNewArray_lf_primary_lf_arrayAccess_lf_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lf_primary_lf_arrayAccess_lf_primary(Java8Parser.PrimaryNoNewArray_lf_primary_lf_arrayAccess_lf_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#primaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary(Java8Parser.PrimaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primaryContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primaryLit}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primaryLit(Java8Parser.PrimaryNoNewArray_lfno_primaryLitContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primaryRefl}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primaryRefl(Java8Parser.PrimaryNoNewArray_lfno_primaryReflContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primaryArrayRefl}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primaryArrayRefl(Java8Parser.PrimaryNoNewArray_lfno_primaryArrayReflContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primaryVRefl}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primaryVRefl(Java8Parser.PrimaryNoNewArray_lfno_primaryVReflContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primarySelf}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primarySelf(Java8Parser.PrimaryNoNewArray_lfno_primarySelfContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primaryClassSelf}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primaryClassSelf(Java8Parser.PrimaryNoNewArray_lfno_primaryClassSelfContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primaryParen}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primaryParen(Java8Parser.PrimaryNoNewArray_lfno_primaryParenContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primaryClass}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primaryClass(Java8Parser.PrimaryNoNewArray_lfno_primaryClassContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primaryField}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primaryField(Java8Parser.PrimaryNoNewArray_lfno_primaryFieldContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primaryArray}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primaryArray(Java8Parser.PrimaryNoNewArray_lfno_primaryArrayContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primaryMethCall}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primaryMethCall(Java8Parser.PrimaryNoNewArray_lfno_primaryMethCallContext ctx);
	/**
	 * Visit a parse tree produced by the {@code primaryNoNewArray_lfno_primaryMethRef}
	 * labeled alternative in {@link Java8Parser#primaryNoNewArray_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primaryMethRef(Java8Parser.PrimaryNoNewArray_lfno_primaryMethRefContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#primaryNoNewArray_lfno_primary_lf_arrayAccess_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primary_lf_arrayAccess_lfno_primary(Java8Parser.PrimaryNoNewArray_lfno_primary_lf_arrayAccess_lfno_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#primaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary(Java8Parser.PrimaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#classInstanceCreationExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassInstanceCreationExpression(Java8Parser.ClassInstanceCreationExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#classInstanceCreationExpression_lf_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassInstanceCreationExpression_lf_primary(Java8Parser.ClassInstanceCreationExpression_lf_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#classInstanceCreationExpression_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitClassInstanceCreationExpression_lfno_primary(Java8Parser.ClassInstanceCreationExpression_lfno_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#typeArgumentsOrDiamond}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeArgumentsOrDiamond(Java8Parser.TypeArgumentsOrDiamondContext ctx);
	/**
	 * Visit a parse tree produced by the {@code PrimFieldAccess}
	 * labeled alternative in {@link Java8Parser#fieldAccess}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimFieldAccess(Java8Parser.PrimFieldAccessContext ctx);
	/**
	 * Visit a parse tree produced by the {@code SuperFieldAccess}
	 * labeled alternative in {@link Java8Parser#fieldAccess}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSuperFieldAccess(Java8Parser.SuperFieldAccessContext ctx);
	/**
	 * Visit a parse tree produced by the {@code TypeSuperFieldAccess}
	 * labeled alternative in {@link Java8Parser#fieldAccess}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeSuperFieldAccess(Java8Parser.TypeSuperFieldAccessContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#fieldAccess_lf_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFieldAccess_lf_primary(Java8Parser.FieldAccess_lf_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#fieldAccess_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFieldAccess_lfno_primary(Java8Parser.FieldAccess_lfno_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#arrayAccess}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayAccess(Java8Parser.ArrayAccessContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#arrayAccess_lf_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayAccess_lf_primary(Java8Parser.ArrayAccess_lf_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#arrayAccess_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayAccess_lfno_primary(Java8Parser.ArrayAccess_lfno_primaryContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ImplicitMethodInvoc}
	 * labeled alternative in {@link Java8Parser#methodInvocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImplicitMethodInvoc(Java8Parser.ImplicitMethodInvocContext ctx);
	/**
	 * Visit a parse tree produced by the {@code StaticMethodInvoc}
	 * labeled alternative in {@link Java8Parser#methodInvocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStaticMethodInvoc(Java8Parser.StaticMethodInvocContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ExprMethodInvoc}
	 * labeled alternative in {@link Java8Parser#methodInvocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExprMethodInvoc(Java8Parser.ExprMethodInvocContext ctx);
	/**
	 * Visit a parse tree produced by the {@code MemberMethodInvoc}
	 * labeled alternative in {@link Java8Parser#methodInvocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMemberMethodInvoc(Java8Parser.MemberMethodInvocContext ctx);
	/**
	 * Visit a parse tree produced by the {@code SuperMethodInvoc}
	 * labeled alternative in {@link Java8Parser#methodInvocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSuperMethodInvoc(Java8Parser.SuperMethodInvocContext ctx);
	/**
	 * Visit a parse tree produced by the {@code TypeSuperMethodInvoc}
	 * labeled alternative in {@link Java8Parser#methodInvocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeSuperMethodInvoc(Java8Parser.TypeSuperMethodInvocContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#methodInvocation_lf_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethodInvocation_lf_primary(Java8Parser.MethodInvocation_lf_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#methodInvocation_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethodInvocation_lfno_primary(Java8Parser.MethodInvocation_lfno_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#argumentList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArgumentList(Java8Parser.ArgumentListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#methodReference}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethodReference(Java8Parser.MethodReferenceContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#methodReference_lf_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethodReference_lf_primary(Java8Parser.MethodReference_lf_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#methodReference_lfno_primary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethodReference_lfno_primary(Java8Parser.MethodReference_lfno_primaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#arrayCreationExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayCreationExpression(Java8Parser.ArrayCreationExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#dimExprs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDimExprs(Java8Parser.DimExprsContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#dimExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDimExpr(Java8Parser.DimExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#constantExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstantExpression(Java8Parser.ConstantExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpression(Java8Parser.ExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#lambdaExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLambdaExpression(Java8Parser.LambdaExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#lambdaParameters}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLambdaParameters(Java8Parser.LambdaParametersContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#inferredFormalParameterList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInferredFormalParameterList(Java8Parser.InferredFormalParameterListContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#lambdaBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLambdaBody(Java8Parser.LambdaBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#assignmentExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssignmentExpression(Java8Parser.AssignmentExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#assignment}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssignment(Java8Parser.AssignmentContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#leftHandSide}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLeftHandSide(Java8Parser.LeftHandSideContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#assignmentOperator}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssignmentOperator(Java8Parser.AssignmentOperatorContext ctx);
	/**
	 * Visit a parse tree produced by the {@code CondOrExpr}
	 * labeled alternative in {@link Java8Parser#conditionalExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCondOrExpr(Java8Parser.CondOrExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code CondOrExprHummOp}
	 * labeled alternative in {@link Java8Parser#conditionalExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCondOrExprHummOp(Java8Parser.CondOrExprHummOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#conditionalOrExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConditionalOrExpression(Java8Parser.ConditionalOrExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#conditionalAndExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConditionalAndExpression(Java8Parser.ConditionalAndExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#inclusiveOrExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInclusiveOrExpression(Java8Parser.InclusiveOrExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#exclusiveOrExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExclusiveOrExpression(Java8Parser.ExclusiveOrExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#andExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAndExpression(Java8Parser.AndExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code EqRelExpr}
	 * labeled alternative in {@link Java8Parser#equalityExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEqRelExpr(Java8Parser.EqRelExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code EqEqExpr}
	 * labeled alternative in {@link Java8Parser#equalityExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEqEqExpr(Java8Parser.EqEqExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code EqNotEqExpr}
	 * labeled alternative in {@link Java8Parser#equalityExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEqNotEqExpr(Java8Parser.EqNotEqExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code RelLtExpr}
	 * labeled alternative in {@link Java8Parser#relationalExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRelLtExpr(Java8Parser.RelLtExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code RelLeExpr}
	 * labeled alternative in {@link Java8Parser#relationalExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRelLeExpr(Java8Parser.RelLeExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code RelGtExpr}
	 * labeled alternative in {@link Java8Parser#relationalExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRelGtExpr(Java8Parser.RelGtExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code RelGeExpr}
	 * labeled alternative in {@link Java8Parser#relationalExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRelGeExpr(Java8Parser.RelGeExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code RelShiftExpr}
	 * labeled alternative in {@link Java8Parser#relationalExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRelShiftExpr(Java8Parser.RelShiftExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code RelInstExpr}
	 * labeled alternative in {@link Java8Parser#relationalExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRelInstExpr(Java8Parser.RelInstExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ShiftRightExpr}
	 * labeled alternative in {@link Java8Parser#shiftExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitShiftRightExpr(Java8Parser.ShiftRightExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ShiftLeftExpr}
	 * labeled alternative in {@link Java8Parser#shiftExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitShiftLeftExpr(Java8Parser.ShiftLeftExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ShiftUnsignLeftExpr}
	 * labeled alternative in {@link Java8Parser#shiftExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitShiftUnsignLeftExpr(Java8Parser.ShiftUnsignLeftExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ShiftAddExpr}
	 * labeled alternative in {@link Java8Parser#shiftExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitShiftAddExpr(Java8Parser.ShiftAddExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code AddAddExpr}
	 * labeled alternative in {@link Java8Parser#additiveExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAddAddExpr(Java8Parser.AddAddExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code AddMultExpr}
	 * labeled alternative in {@link Java8Parser#additiveExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAddMultExpr(Java8Parser.AddMultExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code AddSubExpr}
	 * labeled alternative in {@link Java8Parser#additiveExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAddSubExpr(Java8Parser.AddSubExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code MultMultExpr}
	 * labeled alternative in {@link Java8Parser#multiplicativeExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultMultExpr(Java8Parser.MultMultExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code MultDivExpr}
	 * labeled alternative in {@link Java8Parser#multiplicativeExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultDivExpr(Java8Parser.MultDivExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code MultUnaryExpr}
	 * labeled alternative in {@link Java8Parser#multiplicativeExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultUnaryExpr(Java8Parser.MultUnaryExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code MultModExpr}
	 * labeled alternative in {@link Java8Parser#multiplicativeExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultModExpr(Java8Parser.MultModExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code UnaryIncExpr}
	 * labeled alternative in {@link Java8Parser#unaryExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryIncExpr(Java8Parser.UnaryIncExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code UnaryDecExpr}
	 * labeled alternative in {@link Java8Parser#unaryExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryDecExpr(Java8Parser.UnaryDecExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code UnaryPosExpr}
	 * labeled alternative in {@link Java8Parser#unaryExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryPosExpr(Java8Parser.UnaryPosExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code UnaryNegExpr}
	 * labeled alternative in {@link Java8Parser#unaryExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryNegExpr(Java8Parser.UnaryNegExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code UnaryExpr}
	 * labeled alternative in {@link Java8Parser#unaryExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryExpr(Java8Parser.UnaryExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#preIncrementExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPreIncrementExpression(Java8Parser.PreIncrementExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#preDecrementExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPreDecrementExpression(Java8Parser.PreDecrementExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code UnaryPostExpr}
	 * labeled alternative in {@link Java8Parser#unaryExpressionNotPlusMinus}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryPostExpr(Java8Parser.UnaryPostExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code UnaryBitNotExpr}
	 * labeled alternative in {@link Java8Parser#unaryExpressionNotPlusMinus}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryBitNotExpr(Java8Parser.UnaryBitNotExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code UnaryNotExpr}
	 * labeled alternative in {@link Java8Parser#unaryExpressionNotPlusMinus}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryNotExpr(Java8Parser.UnaryNotExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code UnaryCastExpr}
	 * labeled alternative in {@link Java8Parser#unaryExpressionNotPlusMinus}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryCastExpr(Java8Parser.UnaryCastExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#postfixExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPostfixExpression(Java8Parser.PostfixExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#postIncrementExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPostIncrementExpression(Java8Parser.PostIncrementExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#postIncrementExpression_lf_postfixExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPostIncrementExpression_lf_postfixExpression(Java8Parser.PostIncrementExpression_lf_postfixExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#postDecrementExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPostDecrementExpression(Java8Parser.PostDecrementExpressionContext ctx);
	/**
	 * Visit a parse tree produced by {@link Java8Parser#postDecrementExpression_lf_postfixExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPostDecrementExpression_lf_postfixExpression(Java8Parser.PostDecrementExpression_lf_postfixExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code CastPrimExpr}
	 * labeled alternative in {@link Java8Parser#castExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCastPrimExpr(Java8Parser.CastPrimExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code CastTypeExpr}
	 * labeled alternative in {@link Java8Parser#castExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCastTypeExpr(Java8Parser.CastTypeExprContext ctx);
	/**
	 * Visit a parse tree produced by the {@code CastTypeLambdaExpr}
	 * labeled alternative in {@link Java8Parser#castExpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCastTypeLambdaExpr(Java8Parser.CastTypeLambdaExprContext ctx);
}