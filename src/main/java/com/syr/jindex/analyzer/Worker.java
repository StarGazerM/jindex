package com.syr.jindex.analyzer;

import com.syr.jindex.analyzer.pojo.AnalysisMessage;
import com.syr.jindex.analyzer.pojo.AnalysisResult;
import com.syr.jindex.parser.Java8Lexer;
import com.syr.jindex.parser.Java8Parser;
import io.lettuce.core.RedisClient;
import io.lettuce.core.api.StatefulRedisConnection;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.concurrent.Callable;

public class Worker implements Callable<AnalysisResult> {
    private String dataTmpDir = "/home/stargazermiao/workspace/PL/CSE687/jindex/tmp";
    private static final Logger LOGGER = LoggerFactory.getLogger(MessageReceiver.class);
    RedisClient client;
    StatefulRedisConnection<String, String> cacheConn;
    AnalysisMessage msg;

    public Worker(AnalysisMessage msg) {
        this.client = RedisClient.create("redis://127.0.0.1/0");
        this.cacheConn = client.connect();
        this.msg = msg;
    }

    // TODO: change it into IR proxy/pass style
    @Override
    public AnalysisResult call() throws IOException {
        CharStream in = CharStreams.fromStream(
                new ByteArrayInputStream(msg.getCode().getBytes()));
        Java8Lexer lexer = new Java8Lexer(in);
        Java8Parser parser = new Java8Parser(new CommonTokenStream(lexer));
        Java8Parser.CompilationUnitContext tree = parser.compilationUnit();

        JavaSexprVisitor sexprVisitor = new JavaSexprVisitor();
        String sexpr = sexprVisitor.visit(tree);
        FileOutputStream out = new FileOutputStream(dataTmpDir + msg.getName() + ".sexpr");
        out.write(sexpr.getBytes());

        // TODO: run analysis code here

        LOGGER.debug(sexpr);
        AnalysisResult res = new AnalysisResult();
        res.setId(msg.getId());
        return res;
    }

    void stop() {
        this.cacheConn.close();
        this.client.shutdown();
    }
}
