/***
 *  This is a silly test java program for analysis
 * */
package com.foo.bar;

public class Bar {
    int www;
    public Bar() {
        this.www = 1;
    }
}

class Foo {
    int data;
    StringBuilder sb;

    public Foo(){
        this.data = 0;
        this.sb = new StringBuilder("");
        System.out.println("this is foo!");
        this.bar(1);
        return;
    }

    public void bar(int a) {
        int c;
        int b = 0;
        Bar barclass = new Bar();
        barclass.www = b;
        this.data = a;
        return;
    }
}

class Main {
    public static void main(String[] args) {
        Foo f = new Foo();
        return;
    }
}
