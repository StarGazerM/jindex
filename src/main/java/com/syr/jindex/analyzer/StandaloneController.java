// this just for easy to monintor analyzer via http browser most for debug
// also some standalone feature will be put here
// there is actually no reason to make whole system so complicated
// emmm, I do have.... for course purpose
// here is how real program should work, maybe pack to a vscode LSP later!

package com.syr.jindex.analyzer;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.syr.jindex.analyzer.pojo.CodeRequest;
import com.syr.jindex.parser.Java8Lexer;
import com.syr.jindex.parser.Java8Parser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

@RestController
@CrossOrigin(origins = "http://localhost:8000")
public class StandaloneController {
    private static final Logger LOGGER = LoggerFactory.getLogger(MessageReceiver.class);

    // used for check alive
    @RequestMapping("/")
    public String index() {
        return "analyzer is alive!";
    }

    // this is used for test and demo enter point
    // for test only
    @PostMapping("/analyze")
    public String analyze(@RequestBody CodeRequest msg) throws IOException {
        LOGGER.info("start to analyze....");
        CharStream in = CharStreams.fromString(msg.getIr());
        Java8Lexer lexer = new Java8Lexer(in);
        Java8Parser parser = new Java8Parser(new CommonTokenStream(lexer));

        Java8Parser.CompilationUnitContext tree = parser.compilationUnit();

        JavaSexprVisitor sexprVisitor = new JavaSexprVisitor();
        String ir = sexprVisitor.visit(tree);
        // LOGGER.info(ir);

        RestTemplate restTemplate = new RestTemplate();
        String racketURL = "http://localhost:8081/analyze";
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setAcceptCharset(Arrays.asList(Charset.forName("UTF-8")));
        Map<String, Object> reqbody = new HashMap<>();
        String[] startp = msg.getStart().split(" ");
        reqbody.put("ir", ir);
        reqbody.put("startx", startp[0]);
        reqbody.put("starty", startp[1]);
        ObjectMapper mapper = new ObjectMapper();
        String json = mapper.writeValueAsString(reqbody);
        HttpEntity<String> req = new HttpEntity<>(json, headers);
        ResponseEntity<String> response = restTemplate.postForEntity(racketURL, req, String.class);
        String result = response.getBody();
        LOGGER.info(result);
        return result;
    }
}
