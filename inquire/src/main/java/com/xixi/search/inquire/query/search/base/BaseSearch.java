package com.xixi.search.inquire.query.search.base;

import com.xixi.search.common.param.BaseQueryParam;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
public interface BaseSearch {


    /**
     * 得到最终的dsl语句
     * @param baseQueryParam
     * @return
     */
    NativeSearchQueryBuilder getSearchQueryBuilder(BaseQueryParam baseQueryParam);


    String getIndexName();
}
