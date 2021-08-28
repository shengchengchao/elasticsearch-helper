package com.xixi.search.inquire.query.search.execute;

import com.xixi.search.common.advice.ElasticSearchException;
import com.xixi.search.common.constant.RegexConstants;
import com.xixi.search.common.dto.BoolQueryDTO;
import com.xixi.search.common.enums.RelateEnum;
import com.xixi.search.common.util.DateUtil;
import com.xixi.search.inquire.transform.analyze.SimpleExpressionAnalyze;
import com.xixi.search.inquire.transform.dto.FieldDTO;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.search.join.ScoreMode;
import org.elasticsearch.index.query.*;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.util.CollectionUtils;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;

import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/23
 */
@Slf4j
public class ExpressionPackageBoolQueryExecute extends AbstractPackageBoolQueryExecute {


    private static RelatePackageBoolQueryExecute instance;


    public static synchronized RelatePackageBoolQueryExecute getInstance() {
        if (instance == null) {
            instance = new RelatePackageBoolQueryExecute();
        }
        return instance;
    }


    public void setNextExecute() {
        super.setNextExecute(ExpressionPackageBoolQueryExecute.getInstance());
    }

    @Override
    public void packageQuery(String expression, BoolQueryDTO boolQueryDTO) {
        setNextExecute();
        SimpleExpressionAnalyze instance = SimpleExpressionAnalyze.getInstance();
        FieldDTO fieldDTO = instance.packageExpression(expression);
        Map<String, String> fieldMap = boolQueryDTO.getFieldMap();
        //不是这个类处理 跳到下一个类
        if(StringUtils.isBlank(fieldDTO.getFieldName()) && fieldDTO.getValue()==null){
             this.getNextExecute().packageQuery(expression, boolQueryDTO);
             return;
        }
        QueryBuilder queryBuilder;
        String fieldName = fieldDTO.getFieldName();
        if(fieldMap.containsKey(fieldName)){
            String type = fieldMap.get(fieldName);
            queryBuilder = packageBaseBool(fieldDTO,type);
            List<QueryBuilder> list = Optional.ofNullable(boolQueryDTO.getQueryBuilder()).orElse(new ArrayList<>());
            list.add(queryBuilder);
            boolQueryDTO.setQueryBuilder(list);
            return;
        }else if(fieldName.contains(RegexConstants.REGEX_POINT)){
            String parent = StringUtils.substringBefore(fieldName, RegexConstants.REGEX_POINT);
            Field[] declaredFields = boolQueryDTO.getDeclaredFields();
            Optional<Field> first = Stream.of(declaredFields).filter(x -> StringUtils.equals(parent, x.getName())).findFirst();
            if(first.isPresent()){
                Field field = first.get();
                org.springframework.data.elasticsearch.annotations.Field annotation = AnnotationUtils.findAnnotation(field, org.springframework.data.elasticsearch.annotations.Field.class);
                getChildType(fieldMap,StringUtils.substringAfter(fieldName, RegexConstants.REGEX_POINT), field,annotation);
                if(fieldMap.containsKey(fieldDTO.getFieldName())){
                    queryBuilder = packageBaseBool(fieldDTO, fieldMap.getOrDefault(fieldDTO.getFieldName(), ""));
                    List<QueryBuilder> list = Optional.ofNullable(boolQueryDTO.getQueryBuilder()).orElse(new ArrayList<>());
                    list.add(queryBuilder);
                    boolQueryDTO.setQueryBuilder(list);
                    return;
                }else{
                    throw new ElasticSearchException("找不到对应字段");
                }
            }
        }else{
            throw new ElasticSearchException("找不到对应字段");
        }


    }

    /**
     * 包装基本的Bool语句类型
     * @param fieldDTO
     * @param type 对应类型
     * @return
     */
    private QueryBuilder packageBaseBool(FieldDTO fieldDTO,String type) {
        String fieldName = fieldDTO.getFieldName();
        if(fieldDTO.getValue()==null){
             throw new ElasticSearchException("结果不存在");
        }
        if(FieldType.Double.name().equals(type)||FieldType.Long.name().equals(type)||FieldType.Integer.name().equals(type)){
            RangeQueryBuilder rangeQueryBuilder = QueryBuilders.rangeQuery(fieldName);
            compareBuilder(fieldDTO.getRelation(),Double.parseDouble(fieldDTO.getValue().toString()),rangeQueryBuilder);
            return packageNeBool(rangeQueryBuilder,fieldDTO);
        }else if(FieldType.Text.name().equals(type)){
            MatchQueryBuilder matchQuery = QueryBuilders.matchQuery(fieldName, fieldDTO.getValue());
            return packageNeBool(matchQuery,fieldDTO);
        }else if(FieldType.Keyword.name().equals(type)){
            return packageKeywordQuery(fieldDTO);
        }else if(FieldType.Date.name().equals(type) && fieldDTO.getValue() !=null){
            String value = DateUtil.returnDateTime(fieldDTO.getValue().toString());
            RangeQueryBuilder rangeQueryBuilder = QueryBuilders.rangeQuery(fieldDTO.getFieldName());
            compareBuilder(fieldDTO.getRelation(),value,rangeQueryBuilder);
            return packageNeBool(rangeQueryBuilder,fieldDTO);
        }
        return null;


    }

    /**
     * 对于keyWord的数据要特殊处理下
     * @param fieldDTO
     * @return
     */
    private QueryBuilder packageKeywordQuery(FieldDTO fieldDTO) {
        Object value = fieldDTO.getValue();
        List<String> listValue = fieldDTO.getListValue();
        if(!CollectionUtils.isEmpty(listValue)){
            BoolQueryBuilder queryBuilder = new BoolQueryBuilder();
            listValue.forEach(each->{
                TermQueryBuilder termQueryBuilder = QueryBuilders.termQuery(fieldDTO.getFieldName(), each);
                queryBuilder.should(termQueryBuilder);
            });
            return queryBuilder;
        }else if(value!=null){
            String s = value.toString();
            boolean flag = RegexConstants.isLikeRegex(s);
            String fieldName = fieldDTO.getFieldName();
            if(flag){
                WildcardQueryBuilder wildcardQueryBuilder = QueryBuilders.wildcardQuery(fieldName, s);
                return wildcardQueryBuilder;
            }else {
                return QueryBuilders.termQuery(fieldName, s);
            }
        }else{
            return QueryBuilders.boolQuery();
        }
    }

    /**
     * 额外处理mostNot的语句
     * @param boolQuery
     * @param fieldDTO
     * @return
     */
    private QueryBuilder packageNeBool(QueryBuilder boolQuery,FieldDTO fieldDTO) {
        BoolQueryBuilder defaultQueryBuilder = QueryBuilders.boolQuery();
        if(fieldDTO.getFieldName().contains(RegexConstants.REGEX_POINT)){
            String parent= StringUtils.substringBefore(fieldDTO.getFieldName(), RegexConstants.REGEX_POINT);
            NestedQueryBuilder nestedQueryBuilder = new NestedQueryBuilder(parent, boolQuery, ScoreMode.None);
            if(RelateEnum.NE.getCode().equals(fieldDTO.getRelation())){
                defaultQueryBuilder.mustNot(nestedQueryBuilder);
                return defaultQueryBuilder;
            }else{
                return nestedQueryBuilder;
            }
        }else{
            if(RelateEnum.NE.getCode().equals(fieldDTO.getRelation())){
                defaultQueryBuilder.mustNot(boolQuery);
                return defaultQueryBuilder;
            }else{
                return boolQuery;
            }
        }
    }

    /**
     * 比较进行语句构建
     * @param compareType
     * @param value
     * @param rangeQueryBuilder
     */
    protected void compareBuilder(String compareType, Object value,
                                  RangeQueryBuilder rangeQueryBuilder) {

        if (StringUtils.isEmpty(compareType)) {
            return;
        }

        switch (compareType) {
            case "<":
                rangeQueryBuilder.lt(value);
                break;
            case "<=":
                rangeQueryBuilder.lte(value);
                break;
            case "=":
            case "!=":
                rangeQueryBuilder.lte(value).gte(value);
                break;
            case ">=":
                rangeQueryBuilder.gte(value);
                break;
            case ">":
                rangeQueryBuilder.gt(value);
                break;
            default:
                break;
        }
    }

    private void getChildType( Map<String, String> fieldMap ,String childField, Field parent, org.springframework.data.elasticsearch.annotations.Field fieldAnnotation) {
        //获取属性的类型 List<PatientChecksInfo>
        Type genericType = parent.getGenericType();
        //强转成具体的实现类
        Field[] childFields = new Field[0];
        //isAssignableFrom：从类继承的角度是判断是否为某个类的父类 与instanceof 不同
        //instanceof：从实例继承的角度去判断。是否为某个类的子类
        //ParameterizedTypeImpl  泛型类的参数化
        // try catch 中做的操作就是动态的获得childFiled这个类的属性 这这里就是 PatientChecksInfo的属性
        if (genericType.getClass().isAssignableFrom(ParameterizedTypeImpl.class)) {
            try {
                ParameterizedTypeImpl parameterizedType = (ParameterizedTypeImpl) genericType;
                //取得包含的泛型类型
                Type[] genericTypes = parameterizedType.getActualTypeArguments();
                Class detailType = (Class)genericTypes[0];

                childFields = detailType.getDeclaredFields();
            } catch (Exception e) {
                log.error("获取泛型的类型报错，请检查数据配置:" + fieldAnnotation.type().name(), e);
            }
            String finalChildField = childField;

            Optional<Field> children = Stream.of(childFields).filter(x -> StringUtils.equals(x.getName(), finalChildField)).findFirst();
            if (children.isPresent() && children.get() != null) {
                Field field = children.get();
                org.springframework.data.elasticsearch.annotations.Field childFieldAnnotation = AnnotationUtils.findAnnotation(field, org.springframework.data.elasticsearch.annotations.Field.class);

                if (childFieldAnnotation != null) {
                    StringBuffer name = new StringBuffer(parent.getName());
                    name.append(".").append(childField);
                    fieldMap.put(name.toString(),childFieldAnnotation.type().name());
                }

            }
        }

    }
}
