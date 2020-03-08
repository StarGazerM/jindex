package com.syr.jindex.analyzer;

import io.lettuce.core.RedisClient;
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.pubsub.StatefulRedisPubSubConnection;
import io.lettuce.core.pubsub.api.reactive.RedisPubSubReactiveCommands;

public class Worker implements Runnable{

    RedisClient client;
    StatefulRedisConnection<String, String> cacheConn;
    StatefulRedisPubSubConnection<String, String> msgConn;

    public Worker() {
        this.client = RedisClient.create("redis://127.0.0.1/jindex");
        this.cacheConn = client.connect();
        this.msgConn = client.connectPubSub();
    }

    @Override
    public void run() {
        RedisPubSubReactiveCommands<String, String> msgOpCmd = msgConn.reactive();
        msgOpCmd.subscribe("analyzeq").subscribe();

    }

    void stop() {
        this.cacheConn.close();
        this.client.shutdown();
    }
}
