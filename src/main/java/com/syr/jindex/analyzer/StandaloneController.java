// this just for easy to monintor analyzer via http browser most for debug
// also some standalone feature will be put here
// there is actually no reason to make whole system so complicated
// emmm, I do have.... for course purpose
// here is how real program should work, maybe pack to a vscode LSP later!

package com.syr.jindex.analyzer;

import com.syr.jindex.analyzer.pojo.AnalysisMessage;
import com.syr.jindex.analyzer.pojo.AnalysisResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class StandaloneController {

    @RequestMapping("/")
    public String index() {
        return "analyzer is alive!";
    }

    @GetMapping("/analyze")
    public AnalysisResult analyze(@RequestBody AnalysisMessage msg) {

        return null;
    }
}
