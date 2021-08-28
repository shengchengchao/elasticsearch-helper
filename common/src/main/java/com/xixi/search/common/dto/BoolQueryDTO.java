package com.xixi.search.common.dto;

import lombok.Data;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilder;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;

/**
 * 处理组合dsl语句
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
@Data
public class BoolQueryDTO {


    /**
     * 已经处理好的boolQuery
     */
    private BoolQueryBuilder boolQueryBuilder;

    /**
     * 没有被确认关系的 query
     */
    private List<QueryBuilder> queryBuilder;



    private Map<String, String> fieldMap;

    private Field[] declaredFields;
}
