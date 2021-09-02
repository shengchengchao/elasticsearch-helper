package com.xixi.search.inquire.transform.analyze;

import com.google.common.base.Splitter;
import com.xixi.search.common.enums.RelateEnum;
import com.xixi.search.inquire.transform.dto.FieldDTO;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * 单一语句解析类
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/8
 */
@Slf4j
public class SimpleExpressionAnalyze implements ExpressionAnalyze {

    private static SimpleExpressionAnalyze instance;



    public static synchronized SimpleExpressionAnalyze getInstance() {
        if (instance == null) {
            instance = new SimpleExpressionAnalyze();
        }
        return instance;
    }

    public SimpleExpressionAnalyze() {
    }

    /**
     * 解析表达式中中的字段名
     *
     * @param fieldExpression 表达式  a=2 a!=2
     * @return
     */
    @Override
    public String expressionFieldAnalyze(String fieldExpression) {
        if(StringUtils.isNotBlank(fieldExpression)){
            String relate = expressionRelationAnalyze(fieldExpression);
            if(StringUtils.isNotBlank(relate)){
                List<String> list = Splitter.on(relate).splitToList(fieldExpression);
                if(!CollectionUtils.isEmpty(list) && list.size()==2){
                    return list.get(0);
                }
            }
        }
        return StringUtils.EMPTY;
    }

    /**
     * 解析表达式中的结果
     *
     * @param fieldExpression 表达式
     * @return
     */
    @Override
    public Object expressionValueAnalyze(String fieldExpression) {
        if(StringUtils.isNotBlank(fieldExpression)){
            String relate = expressionRelationAnalyze(fieldExpression);
            if(StringUtils.isNotBlank(relate)){
                List<String> list = Splitter.on(relate).splitToList(fieldExpression);
                if(!CollectionUtils.isEmpty(list) && list.size()==2){
                    return list.get(1);
                }
            }
        }
        return StringUtils.EMPTY;
    }

    /**
     * 解析表达式中的关系
     *
     * @param fieldExpression 表达式
     * @return
     */
    @Override
    public String expressionRelationAnalyze(String fieldExpression) {
        if(StringUtils.isNotBlank(fieldExpression)){
            if(fieldExpression.contains(RelateEnum.GE.getCode())){
                return RelateEnum.GE.getCode();
            }else if(fieldExpression.contains(RelateEnum.GT.getCode())){
                return RelateEnum.GT.getCode();
            }else if(fieldExpression.contains(RelateEnum.LE.getCode())){
                return  RelateEnum.LE.getCode();
            }else if(fieldExpression.contains(RelateEnum.LT.getCode())){
                return RelateEnum.LT.getCode();
            }else if(fieldExpression.contains(RelateEnum.NE.getCode())){
                return RelateEnum.NE.getCode();
            }else if(fieldExpression.contains(RelateEnum.EQ.getCode())){
                return RelateEnum.EQ.getCode();
            }
            return StringUtils.EMPTY;
        }
        return StringUtils.EMPTY;
    }

    /**
     * 装配数据
     * @param expression  表达式
     * @return
     */
    public FieldDTO packageExpression(String expression){
        if(StringUtils.isBlank(expression)){
            return null;
        }
        FieldDTO fieldDTO = new FieldDTO();
        String relation = instance.expressionRelationAnalyze(expression);

        if(StringUtils.isNotBlank(relation)){
            fieldDTO.setRelation(relation);
            Object value = instance.expressionValueAnalyze(expression);
            if(value !=null && StringUtils.contains(String.valueOf(value),',')){
                String valueStr = String.valueOf(value);
                List<String> valueList = Splitter.on(",").splitToList(valueStr);
                fieldDTO.setListValue(valueList);
            }
            fieldDTO.setValue(value);
            fieldDTO.setFieldName(instance.expressionFieldAnalyze(expression));
        }
        return fieldDTO;
    }

}
