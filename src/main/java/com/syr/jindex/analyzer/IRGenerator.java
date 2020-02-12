/**
 *  Worker thread base class for IR generating IR form java AST
 *  It will use visitor pattern to traversal AST and produce different
 *  kind of IR for later pass
 *
 *  Yihao Sun <email>ysun67@syr.edu</email>
 *  Syracuse 2020
 *
 * */
package com.syr.jindex.analyzer;

public interface IRGenerator extends Runnable {

    /***
     *  flush generated resource to somewhere...
     *  some quick but dirty way is just return text as out put
     *  should add more port, probably redis integrated
     */
    public void flush();
}
