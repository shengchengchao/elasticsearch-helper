package com.xixi.search.inquire.query.search.execute;

import com.xixi.search.common.advice.ElasticSearchAssert;
import com.xixi.search.common.advice.ElasticSearchException;
import com.xixi.search.common.constant.RegexConstants;
import com.xixi.search.common.dto.BoolQueryDTO;
import com.xixi.search.common.enums.RelateEnum;
import com.xixi.search.common.util.BeanUtils;
import com.xixi.search.common.util.DateUtil;
import com.xixi.search.inquire.transform.analyze.SimpleExpressionAnalyze;
import com.xixi.search.inquire.transform.dto.CompleteFieldDTO;
import com.xixi.search.inquire.transform.dto.FieldDTO;
import com.xixi.search.inquire.transform.dto.FieldRelateDTO;
import com.xixi.search.inquire.transform.dto.FieldTreeRelateDTO;
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
import java.util.stream.Collectors;
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
        //????????????????????? ??????????????????
        if(StringUtils.isBlank(fieldDTO.getFieldName()) && fieldDTO.getValue()==null){
             this.getNextExecute().packageQuery(expression, boolQueryDTO);
             return;
        }
        QueryBuilder queryBuilder = getQueryBuilder(fieldDTO, fieldMap, boolQueryDTO.getDeclaredFields());
        List<QueryBuilder> list = Optional.ofNullable(boolQueryDTO.getQueryBuilder()).orElse(new ArrayList<>());
        list.add(queryBuilder);
        boolQueryDTO.setQueryBuilder(list);
    }

    private QueryBuilder getQueryBuilder(FieldDTO fieldDTO,Map<String, String> fieldMap,Field[] declaredFields){
        String fieldName = fieldDTO.getFieldName();
        if(fieldMap.containsKey(fieldName)){
            String type = fieldMap.get(fieldName);
            return packageBaseBool(fieldDTO,type);
        }else if(fieldName.contains(RegexConstants.REGEX_POINT)){
            String parent = StringUtils.substringBefore(fieldName, RegexConstants.REGEX_POINT);
            Optional<Field> first = Stream.of(declaredFields).filter(x -> StringUtils.equals(parent, x.getName())).findFirst();
            if(first.isPresent()){
                Field field = first.get();
                org.springframework.data.elasticsearch.annotations.Field annotation = AnnotationUtils.findAnnotation(field, org.springframework.data.elasticsearch.annotations.Field.class);
                getChildType(fieldMap,StringUtils.substringAfter(fieldName, RegexConstants.REGEX_POINT), field,annotation);
                if(fieldMap.containsKey(fieldName)){
                    return packageBaseBool(fieldDTO, fieldMap.getOrDefault(fieldName, ""));
                }else{
                    throw new ElasticSearchException("?????????????????????");
                }
            }
        }else{
            throw new ElasticSearchException("?????????????????????");
        }
        return null;
    }



    /**
     * ???????????????Bool????????????
     * @param fieldDTO
     * @param type ????????????
     * @return
     */
    private QueryBuilder packageBaseBool(FieldDTO fieldDTO,String type) {
        String fieldName = fieldDTO.getFieldName();
        if(fieldDTO.getValue()==null){
             throw new ElasticSearchException("???????????????");
        }
        if(FieldType.Double.name().equals(type)||FieldType.Long.name().equals(type)||FieldType.Integer.name().equals(type)){
            RangeQueryBuilder rangeQueryBuilder = QueryBuilders.rangeQuery(fieldName);
            compareBuilder(fieldDTO.getRelation(),Double.parseDouble(fieldDTO.getValue().toString()),rangeQueryBuilder);
            return packageNeBool(rangeQueryBuilder,fieldDTO);
        }else if(FieldType.Text.name().equals(type)){
            MatchQueryBuilder matchQuery = QueryBuilders.matchQuery(fieldName, fieldDTO.getValue());
            return packageNeBool(matchQuery,fieldDTO);
        }else if(FieldType.Keyword.name().equals(type)){
            QueryBuilder queryBuilder = packageKeywordQuery(fieldDTO);
            return packageNeBool(queryBuilder,fieldDTO);
        }else if(FieldType.Date.name().equals(type) && fieldDTO.getValue() !=null){
            String value = DateUtil.returnDateTime(fieldDTO.getValue().toString());
            RangeQueryBuilder rangeQueryBuilder = QueryBuilders.rangeQuery(fieldDTO.getFieldName());
            compareBuilder(fieldDTO.getRelation(),value,rangeQueryBuilder);
            return packageNeBool(rangeQueryBuilder,fieldDTO);
        }
        return null;


    }

    /**
     * ??????keyWord???????????????????????????
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
     * ????????????mostNot?????????
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
     * ????????????????????????
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
        //????????????????????? List<PatientChecksInfo>
        Type genericType = parent.getGenericType();
        //???????????????????????????
        Field[] childFields = new Field[0];
        //isAssignableFrom???????????????????????????????????????????????????????????? ???instanceof ??????
        //instanceof??????????????????????????????????????????????????????????????????
        //ParameterizedTypeImpl  ?????????????????????
        // try catch ????????????????????????????????????childFiled?????????????????? ??????????????? PatientChecksInfo?????????
        if (genericType.getClass().isAssignableFrom(ParameterizedTypeImpl.class)) {
            try {
                ParameterizedTypeImpl parameterizedType = (ParameterizedTypeImpl) genericType;
                //???????????????????????????
                Type[] genericTypes = parameterizedType.getActualTypeArguments();
                Class detailType = (Class)genericTypes[0];

                childFields = detailType.getDeclaredFields();
            } catch (Exception e) {
                log.error("???????????????????????????????????????????????????:" + fieldAnnotation.type().name(), e);
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

    public QueryBuilder packageDefaultExpression(FieldTreeRelateDTO fieldTreeRelateDTO, Map<String, String> fieldMap, Field[] declaredFields) {
        FieldDTO fieldDTO = fieldTreeRelateDTO.getFieldDTO();
        ElasticSearchAssert.meetCondition(!fieldMap.containsKey(fieldDTO.getFieldName()),"?????????????????????");
        return getQueryBuilder(fieldDTO,fieldMap,declaredFields);
    }




    public List<CompleteFieldDTO> getFieldTreeRelateDTO(List<FieldRelateDTO> fieldRelateList, Map<String, String> fieldMap, Field[] declaredFields) {
        List<CompleteFieldDTO> collect = fieldRelateList.stream().map(x -> {
            FieldDTO fieldDTO = x.getFieldDTO();
            ElasticSearchAssert.meetCondition(fieldDTO == null, "????????????");
            CompleteFieldDTO completeFieldDTO = new CompleteFieldDTO();
            BeanUtils.copyProperties(fieldDTO, completeFieldDTO);
            if (StringUtils.contains(fieldDTO.getFieldName(), RegexConstants.REGEX_POINT)) {
                String parentField = StringUtils.substringBefore(fieldDTO.getFieldName(), RegexConstants.REGEX_POINT);
                String childrenField = StringUtils.substringAfter(fieldDTO.getFieldName(), RegexConstants.REGEX_POINT);
                ElasticSearchAssert.meetCondition(!fieldMap.containsKey(parentField), "??????????????????");
                completeFieldDTO.setParentFieldType(fieldMap.get(parentField));
                Optional<Field> first = Stream.of(declaredFields).filter(field -> StringUtils.equals(parentField, field.getName())).findFirst();
                if (first.isPresent()) {
                    Field field = first.get();
                    org.springframework.data.elasticsearch.annotations.Field annotation = AnnotationUtils.findAnnotation(field, org.springframework.data.elasticsearch.annotations.Field.class);
                    getChildType(fieldMap, childrenField, field, annotation);
                    completeFieldDTO.setChildrenNameType(fieldMap.get(fieldDTO.getFieldName()));
                }
            }
            return completeFieldDTO;
        }).collect(Collectors.toList());
        return collect;
    }




    public QueryBuilder getBoolQueryBuilder(List<CompleteFieldDTO> parentCollect, Map<String, String> fieldMap, Field[] declaredFields) {
        // ??????parentField ????????????????????? ????????????????????????
        CompleteFieldDTO completeFieldDTO = parentCollect.remove(0);
        FieldDTO fieldDTO = new FieldDTO();
        BeanUtils.copyProperties(completeFieldDTO,fieldDTO);
        NestedQueryBuilder queryBuilder = (NestedQueryBuilder) getQueryBuilder(fieldDTO, fieldMap, declaredFields);
        QueryBuilder query = queryBuilder.query();
        for (CompleteFieldDTO completeField : parentCollect) {
            BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
            FieldDTO field = new FieldDTO();
            BeanUtils.copyProperties(completeField,field);
            NestedQueryBuilder nestedQueryBuilder = (NestedQueryBuilder) getQueryBuilder(fieldDTO, fieldMap, declaredFields);
            if(RelateEnum.OR.getCode().equals(completeField.getExpressionRelate())){
                boolQueryBuilder.should(nestedQueryBuilder.query());
                boolQueryBuilder.should(query);
            }else if(RelateEnum.AND.getCode().equals(completeField.getExpressionRelate())){
                boolQueryBuilder.must(nestedQueryBuilder.query());
                boolQueryBuilder.must(query);
            }
            query=boolQueryBuilder;
        }
        return query;
    }
}
