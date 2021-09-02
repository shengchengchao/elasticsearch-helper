package com.xixi.search.inquire.query.search.execute;

import com.xixi.search.common.advice.ElasticSearchAssert;
import com.xixi.search.common.dto.BoolQueryDTO;
import com.xixi.search.common.enums.RelateEnum;
import com.xixi.search.common.util.BeanUtils;
import com.xixi.search.inquire.transform.analyze.TreeAnalyze;
import com.xixi.search.inquire.transform.dto.CompleteFieldDTO;
import com.xixi.search.inquire.transform.dto.FieldRelateDTO;
import com.xixi.search.inquire.transform.parse.NestExpressionParse;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.search.join.ScoreMode;
import org.elasticsearch.index.query.NestedQueryBuilder;
import org.elasticsearch.index.query.QueryBuilder;
import org.springframework.util.CollectionUtils;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/23
 */
public class ParenthesesPackageBoolQueryExecute extends AbstractPackageBoolQueryExecute {

    private static ExpressionPackageBoolQueryExecute instance;



    public static synchronized ExpressionPackageBoolQueryExecute getInstance() {
        if (instance == null) {
            instance = new ExpressionPackageBoolQueryExecute();
        }
        return instance;
    }


    public void setNextExecute() {
        super.setNextExecute(ParenthesesPackageBoolQueryExecute.getInstance());
    }

    @Override
    public void packageQuery(String expression, BoolQueryDTO boolQueryDTO) {
        // 先去判断你的 expression 是否包含 || 或者 &&
        if(StringUtils.isBlank(expression)){
            return;
        }
        setNextExecute();
        if(StringUtils.isNotBlank(expression) && expression.length()==2){
            this.getNextExecute().packageQuery(expression,boolQueryDTO);
            return;
        }
        if(StringUtils.contains(expression, RelateEnum.OR.getCode()) || StringUtils.contains(expression, RelateEnum.AND.getCode())){
            NestExpressionParse parseInstance = getParseInstance();
            List<String> list = parseInstance.nestParseTree(expression);
            BoolQueryDTO helper = BeanUtils.copyProperties(boolQueryDTO, BoolQueryDTO.class);
            helper.setQueryBuilder(null);
            helper.setBoolQueryBuilder(null);
            for (String s : list) {
                this.packageQuery(s,helper);
            }
            boolQueryDTO.setBoolQueryBuilder(helper.getBoolQueryBuilder());
        }else{
            this.getNextExecute().packageQuery(expression,boolQueryDTO);
        }

    }

    public QueryBuilder packageTreeQuery(String expression, Map<String, String> fieldMap, Field[] declaredFields) {
        //先进行分割
        List<FieldRelateDTO> fieldRelateList = TreeAnalyze.getInstance().fieldTreeDTO(expression);
        return getTreeQuery(expression,fieldRelateList,fieldMap, declaredFields);
    }


    public QueryBuilder getTreeQuery(String expression, List<FieldRelateDTO> fieldRelateList, Map<String, String> fieldMap, Field[] declaredFields) {
        List<CompleteFieldDTO> list =  getInstance().getFieldTreeRelateDTO(fieldRelateList,fieldMap,declaredFields);
        ElasticSearchAssert.meetCondition(CollectionUtils.isEmpty(list),"集合为空");
        List<CompleteFieldDTO> parentCollect = list.stream().filter(x -> StringUtils.isNotBlank(x.getParentFieldName())).collect(Collectors.toList());
        List<String> collect = parentCollect.stream().map(CompleteFieldDTO::getParentFieldName).distinct().collect(Collectors.toList());
        if(collect.size()==1 && parentCollect.size()>1){
            //表明只有一个前缀 考虑使用行行匹配的问题
            QueryBuilder  boolQuery =getInstance().getBoolQueryBuilder(parentCollect,fieldMap,declaredFields);
            String parent = collect.get(0);
            NestedQueryBuilder nestedQueryBuilder = new NestedQueryBuilder(parent, boolQuery, ScoreMode.None);
            return nestedQueryBuilder;
        }else{
            //正常处理
            BoolQueryDTO boolQueryDTO = new BoolQueryDTO();
            boolQueryDTO.setFieldMap(fieldMap);
            boolQueryDTO.setDeclaredFields(declaredFields);
            packageQuery(expression,boolQueryDTO);
            return boolQueryDTO.getBoolQueryBuilder();
        }
    }
}
