package com.xixi.search.common.param;

import com.xixi.search.common.constant.EsConstants;
import lombok.Builder;
import lombok.Data;
import lombok.experimental.Tolerate;
import org.apache.commons.lang3.StringUtils;
import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.search.aggregations.AbstractAggregationBuilder;
import org.elasticsearch.search.fetch.subphase.highlight.HighlightBuilder;
import org.elasticsearch.search.sort.FieldSortBuilder;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;

import java.util.List;

/**
 * 基础es参数处理类
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
@Data
@Builder
public class BaseQueryParam {

    /**
     * 索引的名称
     */
    private String indexName;

    /**
     * 查询条件，自由组合的条件
     */
    private String baseEsQueryParams;

    /**
     * 其他附加的条件 获取id字段 默认是and
     */
    private List<QueryBuilder> otherQuery;

    /**
     * 需要排除的条件
     */
    private List<QueryBuilder> excludeQuery;

    /**
     * 分页参数
     */
    private Pageable pageable;

    /**
     * 需要从es中取出的字段 类似sql的select xx from table中的xx：name age..
     */
    private String[] fields;

    /**
     * 聚合数据
     */
    private AbstractAggregationBuilder aggregationBuilder;

    /**
     * 排序字段
     */
    private String sort;

    /**
     * 排序的方式
     */
    private String sortOrder;




    /**
     * 需要高亮的字段
     */
    List<HighlightBuilder.Field> highLightFields;

    /**
     * 嵌套表命中的字段数据
     */
    List<String> innerList;

    /**
     * 追加的id 主要是or的语法，不关心其他条件
     */
    List<String> appendIdList;

    /**
     * 增加员工维度的数量聚合
     */
    public static AbstractAggregationBuilder getEmpAggregationBuilder() {
        return null;
    }


    public void  buildSearchQuery(NativeSearchQueryBuilder nativeSearchQueryBuilder) {
        //增加聚合函数
        if (getAggregationBuilder() != null) {
            nativeSearchQueryBuilder.addAggregation(getAggregationBuilder());
        }
        //增加排序字段,不为相关性排序
        if (StringUtils.isNotEmpty(getSort()) && EsConstants.SORT_ORDER_DESC.equals(getSortOrder()) && !StringUtils.equals(EsConstants.SORT_BY_SCORE, getSort())) {
            nativeSearchQueryBuilder.withSort(new FieldSortBuilder(getSort()).order(SortOrder.DESC));
        } else if (StringUtils.isNotEmpty(getSort()) && EsConstants.SORT_ORDER_ASC.equals(getSortOrder()) && !StringUtils.equals(EsConstants.SORT_BY_SCORE, getSort())) {
            nativeSearchQueryBuilder.withSort(new FieldSortBuilder(getSort()).order(SortOrder.ASC));
        }

    }
    @Tolerate
    public BaseQueryParam() {
    }
}
