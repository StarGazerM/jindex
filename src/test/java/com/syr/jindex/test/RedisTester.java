package com.syr.jindex.test;

import io.lettuce.core.RedisClient;
import io.lettuce.core.pubsub.StatefulRedisPubSubConnection;
import io.lettuce.core.pubsub.api.reactive.RedisPubSubReactiveCommands;

public class RedisTester {

    public static void main(String[] args) {
        RedisClient client = RedisClient.create("redis://127.0.0.1/0");
        StatefulRedisPubSubConnection<String, String> connection = client.connectPubSub();
        RedisPubSubReactiveCommands<String, String> reactive = connection.reactive();
        reactive.observeChannels().doOnNext(msg -> {
            System.out.println(msg.getMessage());
        }).subscribe();
    }
}
