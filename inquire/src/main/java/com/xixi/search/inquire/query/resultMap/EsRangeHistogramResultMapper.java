package com.xixi.search.inquire.query.resultMap;

import com.xixi.search.common.constant.EsConstants;
import com.xixi.search.common.dto.AggDataDTO;
import com.xixi.search.common.dto.AggParamDTO;
import org.apache.commons.lang3.StringUtils;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.search.aggregations.Aggregations;
import org.elasticsearch.search.aggregations.bucket.nested.Nested;
import org.elasticsearch.search.aggregations.bucket.range.ParsedRange;
import org.elasticsearch.search.aggregations.bucket.range.Range;
import org.elasticsearch.search.aggregations.bucket.terms.Terms;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.data.elasticsearch.core.DefaultResultMapper;
import org.springframework.data.elasticsearch.core.aggregation.AggregatedPage;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/9/3
 */
public class EsRangeHistogramResultMapper extends DefaultResultMapper {

    private List<AggDataDTO> resultList;

    private AggParamDTO aggParamDTO;

    public EsRangeHistogramResultMapper(AggParamDTO aggParamDTO) {
        this.aggParamDTO = aggParamDTO;
    }

    @Override
    public <T> AggregatedPage<T> mapResults(SearchResponse response, Class<T> clazz, Pageable pageable) {


        //获取总数
        Aggregations aggregations = response.getAggregations();
        //获取总数
        long totalHits = response.getHits().getTotalHits();
        //判断是否是nest
        if (StringUtils.equals(FieldType.Nested.name(),aggParamDTO.getSearchTyped())) {
            Nested aggregation = aggregations.get(EsConstants.CODE+ FieldType.Nested.name());
            aggregations = aggregation.getAggregations();
        }
        AggParamDTO child = Optional.ofNullable(aggParamDTO.getChild()).orElse(new AggParamDTO()) ;
        String childAggCode = Optional.ofNullable(child.getSearchField()).orElse("");
        ParsedRange parsedRange = aggregations.get(EsConstants.CODE);
        List<? extends Range.Bucket> aggregationBuckets = parsedRange.getBuckets();
        
        List<AggDataDTO> chartList = new ArrayList<>();
        aggregationBuckets.forEach(each->{
            Long docCount = each.getDocCount();
            BigDecimal percent = BigDecimal.valueOf(docCount).divide(BigDecimal.valueOf(totalHits), 4, RoundingMode.HALF_UP);
            Aggregations childAggregations = each.getAggregations();
            AggDataDTO chartDTO = new AggDataDTO(each.getKeyAsString(),  percent,String.valueOf(docCount));
            if (childAggregations != null && StringUtils.isNotBlank(childAggCode)) {

                Terms childAggregationBuckets = childAggregations.get(childAggCode);
                List<? extends Terms.Bucket> buckets = childAggregationBuckets.getBuckets();
                List<AggDataDTO> children = new ArrayList<>();
                buckets.forEach(eachChild -> {
                    long childCount = eachChild.getDocCount();
                    BigDecimal childPercent = BigDecimal.valueOf(childCount).divide(BigDecimal.valueOf(docCount), 4, RoundingMode.HALF_UP);
                    children.add(new AggDataDTO(eachChild.getKeyAsString(), childPercent,String.valueOf(docCount)));
                });

                if (!CollectionUtils.isEmpty(children)) {
                    chartDTO.setChildren(children);
                }
            }
            chartList.add(chartDTO);

        });

        resultList=chartList;

        return null;
    }

    public List<AggDataDTO> getResultList() {
        return resultList;
    }
}
