package com.xixi.search.inquire.transform.parse;

import com.google.common.base.Splitter;
import com.xixi.search.common.enums.RelateEnum;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/11
 */
public abstract class DefaultExpressionParse implements ExpressionParse {


    /**
     * 将检索条件 先进行拆分 拆分后将数据进行
     * @param expression
     * @return
     */
    public List<String> parseTree(String expression) {
        if(StringUtils.isBlank(expression)){
            return new ArrayList<>();
        }
        //分割 && ||
        List<String> list = spiltExpression(expression);
        return list;
    }

    /**
     * 进行分隔
     * @param expression
     * @return
     */
    protected  List<String>  spiltExpression(String expression){
        List<String> midList = Splitter.on(RelateEnum.AND.getCode()).splitToList(expression);
        LinkedList<String> result = new LinkedList<>();

        List<String> orCollect = midList.stream().filter(x -> StringUtils.contains(x, "||")).collect(Collectors.toList());
        List<String> andCollect = midList.stream().filter(x ->!StringUtils.contains(x, "||")).collect(Collectors.toList());

        andCollect.forEach(x->{
            result.add(x);
            result.add(RelateEnum.AND.getCode());
        });
        if(CollectionUtils.isEmpty(orCollect)){
            result.removeLast();
            return result;
        }
        List<String> orList = new ArrayList<>();
        orCollect.forEach(x->{
            orList.addAll(Splitter.on(RelateEnum.OR.getCode()).splitToList(x));
        });
        orList.forEach(each->{
            result.add(each);
            result.add(RelateEnum.OR.getCode());
        });
        result.removeLast();
        return result;
    }


    public abstract List<String> nestParseTree(String expression);


    protected List<String> inorderToPosted(List<String> list){
        if(CollectionUtils.isEmpty(list)){
            return new ArrayList<>();
        }
        List<String> result = new ArrayList<>();
        for (int i = 0; i < list.size(); i++) {
            String expression = list.get(i);
            if(RelateEnum.OR.getCode().equals(expression) || RelateEnum.AND.getCode().equals(expression)){
                if(i+1<list.size()){
                    result.add(list.get(i+1));
                    i++;
                }
            }
            result.add(expression);
        }
        return result;
    }


}
