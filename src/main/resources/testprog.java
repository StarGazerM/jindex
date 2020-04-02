/***
 *  This is a silly test java program for analysis
 * */
package com.foo.bar;

import java.util.List;

class Foo {
    int data;

    public Foo(){
        this.data = 0;
        System.out.println("this is foo!");
        this.bar(1);
        return;
    }

    public void bar(int a) {
        int c;
        int b = 0;
        this.data = a;
        return;
    }
}