/***
 *
 * A IR Proxy for souffle/Racket IR
 *
 * Yihao Sun <email>ysun67@str.edu<email/>
 */

package com.syr.jindex.analyzer;

        import java.lang.reflect.InvocationHandler;
        import java.lang.reflect.Method;

public class IRGeneratorProxy implements InvocationHandler {

    private  IRGenerator generator;

    public IRGenerator getInstance() {
        return null;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        return null;
    }
}
