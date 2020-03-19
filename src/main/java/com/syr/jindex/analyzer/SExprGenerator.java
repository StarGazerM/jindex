package com.syr.jindex.analyzer;
/***
 * A sexpr generator will generate scheme style sexpr from java code
 * using visitor pattern provided by antlr to traversal AST
 */

public class SExprGenerator implements IRGenerator {

    // result produced by
    StringBuilder result;

    @Override
    public void flush() {

    }

}
