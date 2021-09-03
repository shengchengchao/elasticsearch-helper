package com.xixi.search.common.constant;

import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
public class EsConstants {


    /**
     * 横线连接符
     */
    public static final String REGEX_LINE_SYMBOL = "-";
    /** 默认的没有加条件的BOL */
    public static BoolQueryBuilder DEFAULT_QUERY_BUILDER = QueryBuilders.boolQuery();


    /**
     * 高亮的字段
     */
    public static final String DATA_HIGH_LIGHT_FIELD = "highLightContent";

    /** 精确匹配 */
    public static final String SEARCH_TYPE_TERM = "TERM";
    /** 模糊匹配 */
    public static final String SEARCH_TYPE_WILDCARD = "WILDCARD";
    /** 范围匹配 */
    public static final String SEARCH_TYPE_RANGE = "RANGE";
    /** 是否标志查询 */
    public static final String SEARCH_TYPE_FLAG = "FLAG";
    /** 表达式查询 */
    public static final String SEARCH_TYPE_REGEX = "REGEX";
    /** 非查询 */
    public static final String SEARCH_TYPE_NOT = "NOT";


    /** 比较类型 小于 */
    public static final String COMPARE_TYPE_LT = "LT";
    /** 比较类型 小于等于 */
    public static final String COMPARE_TYPE_LTE = "LTE";
    /** 比较类型 等于 */
    public static final String COMPARE_TYPE_EQ = "EQ";
    /** 比较类型 大于等于 */
    public static final String COMPARE_TYPE_GTE = "GTE";
    /** 比较类型 大于 */
    public static final String COMPARE_TYPE_GT = "GT";

    /** 查询的类型 且 MUST */
    public static final String QUERY_AND_OR_TYPE_MUST = "AND";
    /** 查询的类型 或 SHOULD*/
    public static final String QUERY_AND_OR_TYPE_OR = "OR";
    /** 查询的类型 非 MUST_NOT*/
    public static final String QUERY_AND_OR_TYPE_MUST_NOT = "OUT";

    /** 住院类型 */
    public static final String PATIENT_TYPE = "patientType";


    /** 正序排 */
    public static final String SORT_ORDER_ASC = "asc";
    /** 倒叙排*/
    public static final String SORT_ORDER_DESC = "desc";

    /** 条件组件 逻辑关系 */
    public static final String COMPONENT_TYPE_LOGIC = "LOGIC";
    /** 条件组件 查询条件*/
    public static final String COMPONENT_TYPE_CONDITION = "CONDITION";


    /** 相关性排序*/
    public static final String SORT_BY_SCORE = "sortScore";


    /** 聚合方式 */
    public static final String AGG_TERMS = "TERMS";

    public static final String INTERVAL_YEAR = "YEAR";
    public static final String INTERVAL_MONTH = "MONTH";

    /** 图表的过滤的字段 */
    public static final String CHART_FILTER_FIELD = "FIELD_FILTER";


    /** 分析词 */
    public static final String FIELD_ANALYZER = "ik_syno";
    /** 检索的分析词 */
    public static final String SEARCH_ANALYZER = "ik_syno";

    /**
     * 用于聚合的字段
     */
    public static final String CODE ="CODE";

    public static final String FILTER="FILTER";


    public static final String CHILDREN_CODE = "CHILDREN";

    /**
     * 逻辑节点
     */
    public static final String LOGIC_TYPE = "LOGIC";

    /**
     * 条件节点
     */
    public static final String CONDITION_TYPE = "CONDITION";
}
