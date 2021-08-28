package com.xixi.search.inquire.query.resultMap;

import com.xixi.search.common.annotation.MyHighLightField;
import com.xixi.search.common.constant.EsConstants;
import com.xixi.search.common.util.BeanMapUtils;
import org.apache.commons.lang3.StringUtils;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.common.text.Text;
import org.elasticsearch.search.SearchHit;
import org.elasticsearch.search.fetch.subphase.highlight.HighlightField;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.core.DefaultResultMapper;
import org.springframework.data.elasticsearch.core.aggregation.AggregatedPage;
import org.springframework.data.elasticsearch.core.aggregation.impl.AggregatedPageImpl;

import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
public class EsDataResultMapper extends DefaultResultMapper {



    @Override
    public <T> AggregatedPage<T> mapResults(SearchResponse response, Class<T> clazz, Pageable pageable) {

        List<T> chunk = new ArrayList();

        for (SearchHit hit : response.getHits()) {
            if (hit != null) {
                T result = null;
                if (!org.springframework.util.StringUtils.isEmpty(hit.getSourceAsString())) {
                    result = mapEntity(hit.getSourceAsString(), clazz);
                }
                //特殊处理高亮的数据
                Map<String, HighlightField> hMap = hit.getHighlightFields();
                Map<String, Object> map = new HashMap<>(10);
                Map<String, List<String>> allList = new HashMap<>(10);
                for (Map.Entry<String, HighlightField> entry : hMap.entrySet()) {
                    String key = entry.getKey();
                    HighlightField value = entry.getValue();
                    List<String> collect = Arrays.stream(value.getFragments()).map(Text::toString).collect(Collectors.toList());
                    Field[] declaredFields = clazz.getDeclaredFields();
                    Map<String, String> highNameMap =new HashMap<>();
                    Arrays.stream(declaredFields).forEach(field -> {
                        MyHighLightField annotation = AnnotationUtils.findAnnotation(field, MyHighLightField.class);
                        if(annotation!=null){
                            String annotationValue = annotation.value();
                            highNameMap.put(field.getName(),annotationValue);
                        }

                    });
                    String nameByCode = highNameMap.getOrDefault(key, "");
                    if (StringUtils.isNotBlank(nameByCode)) {
                        allList.put(nameByCode, collect);
                    }

                }

                map.put(EsConstants.DATA_HIGH_LIGHT_FIELD,allList);
                T t = BeanMapUtils.mapToBean(map, result);
                chunk.add(t);
            }
        }
        AggregatedPage<T> result=new AggregatedPageImpl(chunk,pageable,response.getHits().getTotalHits(),response.getAggregations());

        return result;


    }




}
