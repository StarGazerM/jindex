package com.syr.jindex.analyzer;

import com.fasterxml.jackson.annotation.JsonValue;

public enum IR {
    SEXPR("SEXPR"),
    SOUFFLE("SOUFFLE");

    private String ir;

    IR(String s) {
        this.ir = s;
    }

    @JsonValue
    public String getIr() {
        return ir;
    }

    public void setIr(String ir) {
        this.ir = ir;
    }
}
