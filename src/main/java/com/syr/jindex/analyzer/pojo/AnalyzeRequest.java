package com.syr.jindex.analyzer.pojo;

public class AnalyzeRequest {
    String ir;
    String startx;
    String starty;

    public String getStarty() {
        return starty;
    }

    public String getStartx() {
        return startx;
    }

    public String getIr() {
        return ir;
    }

    public void setStarty(String starty) {
        this.starty = starty;
    }

    public void setStartx(String startx) {
        this.startx = startx;
    }

    public void setIr(String ir) {
        this.ir = ir;
    }
}
