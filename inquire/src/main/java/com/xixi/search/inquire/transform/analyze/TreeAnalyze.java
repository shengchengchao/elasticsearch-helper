package com.xixi.search.inquire.transform.analyze;

import com.google.common.base.Splitter;
import com.xixi.search.common.advice.ElasticSearchAssert;
import com.xixi.search.common.dto.SearchTreeDTO;
import com.xixi.search.common.enums.RelateEnum;
import com.xixi.search.inquire.transform.dto.FieldDTO;
import com.xixi.search.inquire.transform.dto.FieldRelateDTO;
import com.xixi.search.inquire.transform.dto.FieldTreeRelateDTO;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/9/1
 */
public class TreeAnalyze extends SimpleExpressionAnalyze {

    private static TreeAnalyze instance;



    public static synchronized TreeAnalyze getInstance() {
        if (instance == null) {
            instance = new TreeAnalyze();
        }
        return instance;
    }


    public FieldTreeRelateDTO getFieldTreeRelateDTO(SearchTreeDTO searchTreeDTO) {
        FieldTreeRelateDTO fieldTreeRelateDTO = new FieldTreeRelateDTO();
        fieldTreeRelateDTO.setTreeType(searchTreeDTO.getType());
        String expression = searchTreeDTO.getExpression();
        // 如果包含与或非 需要特殊处理  这里会处理 SQL92的行行匹配 的情况
        if(StringUtils.contains(expression,RelateEnum.AND.getCode()) || StringUtils.contains(expression,RelateEnum.OR.getCode())){
            fieldTreeRelateDTO.setExpression(expression);
        }else{
            FieldDTO fieldDTO = super.packageExpression(expression);
            fieldTreeRelateDTO.setFieldDTO(fieldDTO);
        }
        fieldTreeRelateDTO.setExpressionRelate(searchTreeDTO.getRelation());
        List<FieldTreeRelateDTO> childrenList = new ArrayList<>();
        List<SearchTreeDTO> childrenLists = searchTreeDTO.getChildrenLists();
        if(!CollectionUtils.isEmpty(childrenLists)){
            childrenLists.forEach(each->{
                childrenList.add(getFieldTreeRelateDTO(searchTreeDTO));
            });
        }
        fieldTreeRelateDTO.setChildrenList(childrenList);
        return fieldTreeRelateDTO;
    }


    public List<FieldRelateDTO> fieldTreeDTO(String expression){
        ElasticSearchAssert.meetCondition(StringUtils.isBlank(expression),"字符串为空");
        // 先去进行拆分成 FieldRelateDTO 后续再处理成  NestFieldDTO
        String relateType = RelateEnum.AND.getCode();

        List<String> list = Splitter.on(RelateEnum.AND.getCode()).splitToList(expression);
        if(list.size()==1){
            list = Splitter.on(RelateEnum.OR.getCode()).splitToList(expression);
            relateType =  RelateEnum.OR.getCode();
        }
        String finalRelateType = relateType;
        List<FieldRelateDTO> collect = list.stream().map(x -> {
            FieldDTO fieldDTO = super.packageExpression(expression);
            FieldRelateDTO fieldRelateDTO = new FieldRelateDTO();
            fieldRelateDTO.setFieldDTO(fieldDTO);
            fieldRelateDTO.setExpression(x);
            fieldRelateDTO.setExpressionRelate(finalRelateType);
            return fieldRelateDTO;
        }).collect(Collectors.toList());
        return collect;
    }

}
