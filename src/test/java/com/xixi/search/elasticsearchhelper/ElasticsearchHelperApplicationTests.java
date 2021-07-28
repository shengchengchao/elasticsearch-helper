package com.xixi.search.elasticsearchhelper;

import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.action.index.IndexResponse;
import org.elasticsearch.action.search.SearchRequest;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.common.unit.TimeValue;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.SearchHit;
import org.elasticsearch.search.builder.SearchSourceBuilder;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.TimeUnit;

@SpringBootTest
@Slf4j
class ElasticsearchHelperApplicationTests {

   
    @Autowired 
    private RestHighLevelClient elasticsearchClient;
    
    @Test
    void contextLoads() throws IOException {
        SearchSourceBuilder sourceBuilder = new SearchSourceBuilder();
        sourceBuilder.query(QueryBuilders.matchQuery("body", "elk"));
        sourceBuilder.from(0);
        sourceBuilder.size(5);
        sourceBuilder.timeout(new TimeValue(60, TimeUnit.SECONDS));
        SearchRequest searchRequest = new SearchRequest();
        searchRequest.source(sourceBuilder);
        searchRequest.indices("blogs");
        SearchResponse response = elasticsearchClient.search(searchRequest, RequestOptions.DEFAULT);
        long totalHits = response.getHits().getTotalHits();
        log.info("result.hit={}", totalHits);

        for (SearchHit hit : response.getHits()) {
            Map<String, Object> map = hit.getSourceAsMap();
            log.info("map={}", map.toString());
            log.info("docId={}", hit.getId());
        }

    }

}
