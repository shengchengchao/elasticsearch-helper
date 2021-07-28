package com.xixi.search.elasticsearchhelper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class ElasticsearchHelperApplication {
    private static final Logger logger = LoggerFactory.getLogger(ElasticsearchHelperApplication.class);

    public static void main(String[] args) {
         setGlobalUncaughtExceptionHandler();

        SpringApplication.run(ElasticsearchHelperApplication.class, args);
    }


    private static void setGlobalUncaughtExceptionHandler() {
        Thread.setDefaultUncaughtExceptionHandler((t, e) -> logger.error("UnCaughtException", e));
    }
}
