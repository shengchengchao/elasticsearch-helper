package com.xixi.search.inquire.query.search.execute;

import com.xixi.search.common.advice.ElasticSearchAssert;
import com.xixi.search.common.dto.BoolQueryDTO;
import com.xixi.search.common.enums.RelateEnum;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * 关联关系的处理类
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/23
 */
@Slf4j
public class RelatePackageBoolQueryExecute extends AbstractPackageBoolQueryExecute {



    @Override
    public void packageQuery(String expression, BoolQueryDTO boolQueryDTO) {
        BoolQueryBuilder defaultQueryBuilder = QueryBuilders.boolQuery();
        ElasticSearchAssert.meetCondition(boolQueryDTO==null,"解析出错");
        List<QueryBuilder> queryBuilder = boolQueryDTO.getQueryBuilder();
        if(CollectionUtils.isEmpty(queryBuilder)){
            boolQueryDTO.setBoolQueryBuilder(boolQueryDTO.getBoolQueryBuilder()==null ? defaultQueryBuilder:boolQueryDTO.getBoolQueryBuilder()  );
            return;
        }
        if(RelateEnum.OR.getCode().equals(expression)){
            if(boolQueryDTO.getBoolQueryBuilder() !=null){
                defaultQueryBuilder.should(boolQueryDTO.getBoolQueryBuilder());
            }
            for (QueryBuilder builder : queryBuilder) {
                defaultQueryBuilder.should(builder);
            }
        }else if(RelateEnum.AND.getCode().equals(expression)) {
            if(boolQueryDTO.getBoolQueryBuilder() !=null){
                defaultQueryBuilder.must(boolQueryDTO.getBoolQueryBuilder());
            }
            for (QueryBuilder builder : queryBuilder) {
                defaultQueryBuilder.must(builder);
            }
        }
        boolQueryDTO.setQueryBuilder(new ArrayList<>());
        boolQueryDTO.setBoolQueryBuilder(defaultQueryBuilder);
    }
}
