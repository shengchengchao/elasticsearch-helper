package com.xixi.search.example.search;

import com.xixi.search.common.dto.SearchTreeDTO;
import com.xixi.search.common.util.PagingHelper;
import com.xixi.search.example.transport.index.EmployeeEsEntity;
import com.xixi.search.inquire.query.search.AbstractExpressionSearchQueryBuilder;
import com.xixi.search.inquire.query.search.handle.HandleRegistry;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.springframework.data.elasticsearch.core.aggregation.AggregatedPage;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/25
 */
@Component
public class EmployeeExpressionSearchQuery extends AbstractExpressionSearchQueryBuilder<EmployeeEsEntity> {
    /**
     * 添加额外的结果
     *
     * @param pageResult es结果
     * @param myPage     返回结果
     */
    @Override
    protected void buildOtherResult(AggregatedPage<EmployeeEsEntity> pageResult, PagingHelper myPage) {
        return;
    }

    /**
     * 添加检索拦截处理类 最基本的就是高亮
     *
     * @param extendMap
     * @return
     */
    @Override
    protected HandleRegistry addSearchHandler(Map extendMap) {
        return new HandleRegistry();
    }

    @Override
    protected BoolQueryBuilder buildTreeQuery(SearchTreeDTO searchTreeDTO) {
        return null;
    }
}
