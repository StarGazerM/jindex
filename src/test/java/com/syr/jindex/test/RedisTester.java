package com.syr.jindex.test;

import com.syr.jindex.analyzer.Main;
import com.syr.jindex.analyzer.MessageReceiver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.adapter.MessageListenerAdapter;

@SpringBootApplication
public class RedisTester {

    private static final Logger LOGGER = LoggerFactory.getLogger(RedisTester.class);

    @Bean
    RedisMessageListenerContainer container(RedisConnectionFactory connectionFactory,
                                            MessageListenerAdapter listenerAdapter) {

        RedisMessageListenerContainer container = new RedisMessageListenerContainer();
        container.setConnectionFactory(connectionFactory);
        container.addMessageListener(listenerAdapter, new PatternTopic("chat"));

        return container;
    }

    @Bean
    MessageListenerAdapter listenerAdapter(MessageReceiver receiver) {
        return new MessageListenerAdapter(receiver, "receiveMessage");
    }

    @Bean
    MessageReceiver receiver() {
        return new MessageReceiver();
    }

    @Bean
    StringRedisTemplate template(RedisConnectionFactory connectionFactory) {
        return new StringRedisTemplate(connectionFactory);
    }

    public static void main(String[] args) throws InterruptedException {

        ApplicationContext ctx = SpringApplication.run(RedisTester.class, args);

        StringRedisTemplate template = ctx.getBean(StringRedisTemplate.class);
        MessageReceiver receiver = ctx.getBean(MessageReceiver.class);

        while (true) {

            LOGGER.info("Sending message...");
            template.convertAndSend("chat", "Hello from Redis!");
            Thread.sleep(500L);
        }

        // System.exit(0);
    }
}
