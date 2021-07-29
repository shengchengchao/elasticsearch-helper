package com.xixi.search.elasticsearchhelper.util;

import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.search.builder.SearchSourceBuilder;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/29
 */
public class EsUtil {


    /**
     * 打印语句
     * @param queryBuilder
     * @return
     */
    public static String getSearchQuerySource(QueryBuilder queryBuilder) {
        return new SearchSourceBuilder().query(
                queryBuilder
        ).toString();
    }
}
