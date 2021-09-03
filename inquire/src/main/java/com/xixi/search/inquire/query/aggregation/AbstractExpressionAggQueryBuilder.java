package com.xixi.search.inquire.query.aggregation;

import com.google.common.primitives.Longs;
import com.xixi.search.common.advice.ElasticSearchAssert;
import com.xixi.search.common.constant.EsConstants;
import com.xixi.search.common.constant.RegexConstants;
import com.xixi.search.common.dto.AggDataDTO;
import com.xixi.search.common.dto.AggParamDTO;
import com.xixi.search.common.param.BaseQueryParam;
import com.xixi.search.common.vo.ChartDataVO;
import com.xixi.search.inquire.query.resultMap.EsDateHistogramResultMapper;
import com.xixi.search.inquire.query.resultMap.EsRangeHistogramResultMapper;
import com.xixi.search.inquire.query.resultMap.EsTermsHistogramResultMapper;
import com.xixi.search.inquire.query.search.AbstractExpressionSearchQueryBuilder;
import org.apache.commons.lang3.StringUtils;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.aggregations.AbstractAggregationBuilder;
import org.elasticsearch.search.aggregations.AggregationBuilders;
import org.elasticsearch.search.aggregations.bucket.filter.FilterAggregationBuilder;
import org.elasticsearch.search.aggregations.bucket.histogram.DateHistogramAggregationBuilder;
import org.elasticsearch.search.aggregations.bucket.histogram.DateHistogramInterval;
import org.elasticsearch.search.aggregations.bucket.range.RangeAggregationBuilder;
import org.elasticsearch.search.aggregations.bucket.terms.TermsAggregationBuilder;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.data.elasticsearch.core.DefaultResultMapper;
import org.springframework.data.elasticsearch.core.aggregation.AggregatedPage;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/29
 */
public abstract class AbstractExpressionAggQueryBuilder<T> extends AbstractExpressionSearchQueryBuilder<T> {


    /**
     * 根据表达式 构建出查询语句
     * @param expression
     * @return
     */
    NativeSearchQueryBuilder getAggreSearchBuilder(String expression){
        String[] fields = new String[1];
        fields[0] = getIdField();
        BaseQueryParam baseQueryParam = super.buildQueryParam(fields, getIndexName(), expression);
        baseQueryParam.setPageable(PageRequest.of(0,1));

        NativeSearchQueryBuilder searchQueryBuilder = getSearchQueryBuilder(baseQueryParam);
        return searchQueryBuilder;
    }
    
    /**
     * 简单数据聚合
     * @param expression
     * @param aggParamDTO
     * @return
     */
    public ChartDataVO termHistogram(String expression, AggParamDTO aggParamDTO){
        NativeSearchQueryBuilder searchQueryBuilder = getAggreSearchBuilder(expression);
        EsTermsHistogramResultMapper esTermsHistogramResultMapper = new EsTermsHistogramResultMapper(aggParamDTO);
        buildTermAggregation(aggParamDTO,searchQueryBuilder,esTermsHistogramResultMapper);
        List<AggDataDTO> resultList = esTermsHistogramResultMapper.getResultList();
        ChartDataVO chartDataVO = new ChartDataVO();
        chartDataVO.setAggData(resultList);
        return chartDataVO;
    }

    /**
     * 
     * @param aggParamDTO  聚合参数
     * @param searchQueryBuilder  请求参数 
     * @param defaultResultMapper  默认的返回结果
     * @return
     */
    AggregatedPage<T> buildTermAggregation(AggParamDTO aggParamDTO, NativeSearchQueryBuilder searchQueryBuilder, DefaultResultMapper defaultResultMapper){
        String searchField = aggParamDTO.getSearchField();
        AbstractAggregationBuilder abstractAggregationBuilder;
        TermsAggregationBuilder termsAggregationBuilder = AggregationBuilders.terms(EsConstants.CODE).field(searchField);
        if (aggParamDTO.getSize() != null) {
            termsAggregationBuilder.size(aggParamDTO.getSize());
        }
        //如果子类不为null，则需要进行二级计算
        if (aggParamDTO.getChild()!=null) {
            if (EsConstants.AGG_TERMS.equals(aggParamDTO.getChild().getSearchTyped())) {
                AggParamDTO child = aggParamDTO.getChild();
                String childField = child.getSearchField();
                TermsAggregationBuilder childAgg = AggregationBuilders.terms(EsConstants.CHILDREN_CODE).field(childField);
                if (child.getSize() != null) {
                    childAgg.size(child.getSize());
                }
                termsAggregationBuilder.subAggregation(childAgg);
            }
        }

        FilterAggregationBuilder aggregationBuilder = AggregationBuilders.filter(EsConstants.CHART_FILTER_FIELD, QueryBuilders.boolQuery()).subAggregation(termsAggregationBuilder);
        
        //如果为nest则需要增加一层结构
        if (StringUtils.equals(FieldType.Nested.name(), aggParamDTO.getSearchTyped())) {
            abstractAggregationBuilder = AggregationBuilders.nested(EsConstants.CODE + FieldType.Nested.name(), getPathFromParams(aggParamDTO.getSearchField()));
            abstractAggregationBuilder.subAggregation(aggregationBuilder);
        } else {
            abstractAggregationBuilder = aggregationBuilder;
        }
        searchQueryBuilder.addAggregation(abstractAggregationBuilder);
        NativeSearchQuery query = searchQueryBuilder.build();
        return elasticsearchRestTemplate.queryForPage(query,gettClass(),defaultResultMapper);
    }

    /**
     * 获取嵌套表的路径 aa.bb.cc 截取获取的aa.bb
     * @param param 表达式字符串
     * @return 截取的字符串
     */
    private String getPathFromParams(String param) {
        if (param.lastIndexOf(RegexConstants.REGEX_POINT) > -1) {
            return StringUtils.substringBefore(param, RegexConstants.REGEX_POINT);
        } else {
            return param;
        }
    }

    /**
     * 范围数据聚合
     * @param expression
     * @param aggParamDTO
     * @return
     */
    public ChartDataVO rangeHistogram(String expression, AggParamDTO aggParamDTO){
        NativeSearchQueryBuilder searchQueryBuilder = getAggreSearchBuilder(expression);
        EsRangeHistogramResultMapper esRangeHistogramResultMapper = new EsRangeHistogramResultMapper(aggParamDTO);
        buildRangeHistogram(aggParamDTO,esRangeHistogramResultMapper,searchQueryBuilder);
        List<AggDataDTO> resultList = esRangeHistogramResultMapper.getResultList();
        ChartDataVO chartDataVO = new ChartDataVO();
        chartDataVO.setAggData(resultList);
        return chartDataVO;
    }


    AggregatedPage<T> buildRangeHistogram( AggParamDTO aggParamDTO, DefaultResultMapper defaultResultMapper,NativeSearchQueryBuilder searchQueryBuilder){
        AbstractAggregationBuilder abstractAggregationBuilder;
        
        RangeAggregationBuilder rangeAggregationBuilder = AggregationBuilders.range(EsConstants.CODE).field(aggParamDTO.getSearchField());
        ElasticSearchAssert.meetCondition(CollectionUtils.isEmpty(aggParamDTO.getRangeList()),"范围为空");
        List<String> rangeParam = aggParamDTO.getRangeList();
        rangeParam.forEach(each->{
            String[] split = StringUtils.split(each, EsConstants.REGEX_LINE_SYMBOL);
            rangeAggregationBuilder.addRange(Longs.tryParse(split[0]), Longs.tryParse(split[1]));
        });
        if (aggParamDTO.getChild() != null) {
            AggParamDTO child = aggParamDTO.getChild();
            if (EsConstants.AGG_TERMS.equals(child.getSearchTyped())) {
                String childField = child.getSearchField();
                TermsAggregationBuilder childAgg = AggregationBuilders.terms(EsConstants.CHILDREN_CODE).field(childField);
                if (child.getSize() != null) {
                    childAgg.size(child.getSize());
                }
                rangeAggregationBuilder.subAggregation(childAgg);
            }
        }
        //如果为nest则需要增加一层结构
        if (StringUtils.equals(FieldType.Nested.name(), aggParamDTO.getSearchTyped())) {
            abstractAggregationBuilder = AggregationBuilders.nested(EsConstants.CODE + FieldType.Nested.name(), getPathFromParams(aggParamDTO.getSearchField()));
            abstractAggregationBuilder.subAggregation(rangeAggregationBuilder);
        } else {
            abstractAggregationBuilder = rangeAggregationBuilder;
        }

        searchQueryBuilder.addAggregation(abstractAggregationBuilder);
        NativeSearchQuery query = searchQueryBuilder.build();
        return elasticsearchRestTemplate.queryForPage(query,gettClass(),defaultResultMapper);
    }


    /**
     * 日期范围数据
     * @param expression
     * @param aggParamDTO
     * @return
     */
    public ChartDataVO dateHistogram(String expression, AggParamDTO aggParamDTO){
        NativeSearchQueryBuilder searchQueryBuilder = getAggreSearchBuilder(expression);
        EsDateHistogramResultMapper esDateHistogramResultMapper = new EsDateHistogramResultMapper(aggParamDTO);
        buildDateAggregation(aggParamDTO,searchQueryBuilder,esDateHistogramResultMapper);
        List<AggDataDTO> resultList = esDateHistogramResultMapper.getResultList();
        ChartDataVO chartDataVO = new ChartDataVO();
        chartDataVO.setAggData(resultList);
        return chartDataVO;
    }

    AggregatedPage<T> buildDateAggregation(AggParamDTO aggParamDTO, NativeSearchQueryBuilder searchQueryBuilder, DefaultResultMapper defaultResultMapper){
        AbstractAggregationBuilder abstractAggregationBuilder;
        //级别一
        String field = aggParamDTO.getSearchField();
        //解析为数据范围
        String unit = aggParamDTO.getUnit();
        DateHistogramAggregationBuilder dateHistogramAggregationBuilder = AggregationBuilders.dateHistogram(EsConstants.CODE).field(field);
        if (StringUtils.equals(unit, EsConstants.INTERVAL_YEAR)) {
            dateHistogramAggregationBuilder.dateHistogramInterval(DateHistogramInterval.YEAR);
        } else if(StringUtils.equals(unit, EsConstants.INTERVAL_MONTH)){
            dateHistogramAggregationBuilder.dateHistogramInterval(DateHistogramInterval.MONTH);
        }else {
            dateHistogramAggregationBuilder.dateHistogramInterval(DateHistogramInterval.YEAR);
        }
        AggParamDTO child = aggParamDTO.getChild();
        //如果子类不为null，则需要进行二级计算
        if (child != null) {
            AbstractAggregationBuilder childAggregationBuilder;
            if (EsConstants.AGG_TERMS.equals(child.getSearchTyped())) {
                String searchField = child.getSearchField();
                TermsAggregationBuilder childAgg = AggregationBuilders.terms(EsConstants.CHILDREN_CODE).field(searchField);
                if (child.getSize() != null) {
                    childAgg.size(child.getSize());
                }
                childAggregationBuilder = childAgg;
                dateHistogramAggregationBuilder.subAggregation(childAggregationBuilder);
            }
        }

        //如果为nest则需要增加一层结构
        if (StringUtils.equals(FieldType.Nested.name(), aggParamDTO.getSearchTyped())) {
            abstractAggregationBuilder = AggregationBuilders.nested(EsConstants.CODE + FieldType.Nested.name(), getPathFromParams(aggParamDTO.getSearchField()));
            abstractAggregationBuilder.subAggregation(dateHistogramAggregationBuilder);
        } else {
            abstractAggregationBuilder = dateHistogramAggregationBuilder;
        }
        searchQueryBuilder.addAggregation(abstractAggregationBuilder);
        NativeSearchQuery query = searchQueryBuilder.build();
        return elasticsearchRestTemplate.queryForPage(query,gettClass(),defaultResultMapper);
    }


}
