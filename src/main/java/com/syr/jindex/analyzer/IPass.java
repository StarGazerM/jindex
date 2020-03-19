package com.syr.jindex.analyzer;

import java.util.concurrent.Callable;

public interface IPass extends Callable {

    /***
     * feed IR in and get IR out
     * @return next stage IR
     */
    public String run(IR irType , String in);

}
