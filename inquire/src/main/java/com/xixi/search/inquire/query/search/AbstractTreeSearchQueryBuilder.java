package com.xixi.search.inquire.query.search;

import com.google.common.collect.Lists;
import com.xixi.search.common.constant.EsConstants;
import com.xixi.search.common.dto.SearchTreeDTO;
import com.xixi.search.common.param.BaseQueryParam;
import com.xixi.search.common.param.ExtentSearchPageParam;
import com.xixi.search.inquire.query.search.execute.ExpressionPackageBoolQueryExecute;
import com.xixi.search.inquire.query.search.execute.ParenthesesPackageBoolQueryExecute;
import com.xixi.search.inquire.transform.analyze.TreeAnalyze;
import com.xixi.search.inquire.transform.dto.FieldTreeRelateDTO;
import org.apache.commons.lang3.StringUtils;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilder;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/31
 */
public abstract class AbstractTreeSearchQueryBuilder <T> extends AbstractExpressionSearchQueryBuilder<T> {




    @Override
    protected BaseQueryParam buildQueryParam(ExtentSearchPageParam params) {
        String[] fields =  params.getFields()!=null && params.getFields().length>0 ?params.getFields() : getFields();
        BaseQueryParam param = BaseQueryParam.builder()
                .indexName(getIndexName())
                .fields(fields)
                .searchTreeDTO(params.getSearchTreeDTO())
                .build();
        return param;
    }

    @Override
    protected BoolQueryBuilder buildTreeQuery(SearchTreeDTO searchTreeDTO) {
        //递归构建出树
        FieldTreeRelateDTO fieldTreeRelateDTO = TreeAnalyze.getInstance().getFieldTreeRelateDTO(searchTreeDTO);
        BoolQueryBuilder boolQueryBuilder = new BoolQueryBuilder();
        getBoolQueryTree(fieldTreeRelateDTO,boolQueryBuilder);
        return boolQueryBuilder;
    }


    public void getBoolQueryTree(FieldTreeRelateDTO fieldTreeRelateDTO,BoolQueryBuilder queryBuilders ){
        if(EsConstants.LOGIC_TYPE.equals(fieldTreeRelateDTO.getTreeType())){
            List<FieldTreeRelateDTO> childrenList = fieldTreeRelateDTO.getChildrenList();
            List<QueryBuilder> boolList = new ArrayList<>();
            if(!CollectionUtils.isEmpty(childrenList)){
                childrenList.forEach(each->{
                    BoolQueryBuilder childBoolQueryBuilder = new BoolQueryBuilder();
                    getBoolQueryTree(each,childBoolQueryBuilder);
                    boolList.add(childBoolQueryBuilder);
                });
            }
            buildBool(boolList,queryBuilders,fieldTreeRelateDTO.getExpressionRelate());
        }else{
            // 不是逻辑节点的 那就是条件节点 条件节点的 childrenList为空
            List<QueryBuilder> list = packageEsBool(fieldTreeRelateDTO);
            if(!CollectionUtils.isEmpty(list)){
                buildBool(list,queryBuilders,fieldTreeRelateDTO.getExpressionRelate());
            }
        }
    }

    private List<QueryBuilder> packageEsBool(FieldTreeRelateDTO fieldTreeRelateDTO) {
        String expression = fieldTreeRelateDTO.getExpression();
        Map<String, String> fieldMap = getFieldMap();
        //代表是正常的语句 直接使用相关的处理
        ParenthesesPackageBoolQueryExecute executeInstance = super.getExecuteInstance();
        if(StringUtils.isBlank(expression)){
            ExpressionPackageBoolQueryExecute nextExecute = (ExpressionPackageBoolQueryExecute) executeInstance.getNextExecute();
            QueryBuilder queryBuilder = nextExecute.packageDefaultExpression(fieldTreeRelateDTO,fieldMap,getDeclaredFields());
            return Lists.newArrayList(queryBuilder);
        }else{
            // 存在 与 或操作 需要先进行拆分
            QueryBuilder queryBuilder = executeInstance.packageTreeQuery(fieldTreeRelateDTO.getExpression(),fieldMap,getDeclaredFields());
            return Lists.newArrayList(queryBuilder);
        }

    }


    public void buildBool(List<QueryBuilder> list,BoolQueryBuilder queryBuilders,String relationType){
        if (EsConstants.QUERY_AND_OR_TYPE_MUST.equals(relationType) || EsConstants.QUERY_AND_OR_TYPE_MUST_NOT.equals(relationType)) {
            list.forEach(each->queryBuilders.must(each));
        } else if (EsConstants.QUERY_AND_OR_TYPE_OR.equals(relationType)) {
            list.forEach(each->queryBuilders.should(each));
        } else {
            list.forEach(each->queryBuilders.must(each));
        }
    }
}
