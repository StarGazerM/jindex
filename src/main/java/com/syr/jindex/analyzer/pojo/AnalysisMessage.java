/***
 *  the message of format should in JSON,
 */

package com.syr.jindex.analyzer.pojo;

import com.syr.jindex.analyzer.IR;

public class AnalysisMessage {
    private String name;
    private IR ir;
    private int id;
    private String analysis;
    private String code;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public IR getIr() {
        return ir;
    }

    public void setIr(IR ir) {
        this.ir = ir;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getAnalysis() {
        return analysis;
    }

    public void setAnalysis(String analysis) {
        this.analysis = analysis;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }
}
