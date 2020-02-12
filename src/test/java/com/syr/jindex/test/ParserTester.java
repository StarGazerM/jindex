package com.syr.jindex.test;

import com.syr.jindex.analyzer.JavaSexprVisitor;
import com.syr.jindex.parser.Java8Lexer;
import com.syr.jindex.parser.Java8Parser;
import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.*;

public class ParserTester {

    public static void main(String[] args) {
        CharStream in = null;
        try {
            in = CharStreams.fromFileName("src/main/resources/testprog.java");
        } catch (IOException e) {
            e.printStackTrace();
        }
        Java8Lexer lexer = new Java8Lexer(in);
        Java8Parser parser = new Java8Parser(new CommonTokenStream(lexer));

        Java8Parser.CompilationUnitContext tree = parser.compilationUnit();

        JavaSexprVisitor sexprVisitor = new JavaSexprVisitor();
        System.out.println(sexprVisitor.visit(tree));
    }
}
