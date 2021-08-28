package com.xixi.search.inquire.transform.parse;

import com.xixi.search.common.advice.ElasticSearchAssert;
import com.xixi.search.inquire.transform.dto.ExpressionDTO;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;


/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/16
 */
public class NestExpressionParse extends DefaultExpressionParse {

    public static final String REPLACEMENT = "nestParse";


    /**
     * 嵌套模式的拆分
     *
     * @param expression
     * @return
     */
    @Override
    public List<String> nestParseTree(String expression) {
        expression = StringUtils.deleteWhitespace(expression);
        ElasticSearchAssert.meetCondition(StringUtils.isBlank(expression)  ,"表达式为空");
        if(!StringUtils.contains(expression,'(')){
            List<String> list = parseTree(expression);
            return inorderToPosted(list);
        }else{
            //先校验括号是否匹配
            ElasticSearchAssert.meetCondition(!ParenthesesRegex.isSymbolTwin(expression),"该表达式括号匹配失败");
            // 多层嵌套的括号 要扒干净 比如 ((a)) -> a
            expression = ParenthesesRegex.removeExcludeParentheses(expression);
            if(StringUtils.isBlank(expression)){
                return null;
            }
            List<ExpressionDTO> splitList = ParenthesesRegex.getSymbolStr(expression, '(');

            // 这里做法是这样的 上一行得到了文本中带括号的语句集合，之后将带括号的替换成nestParse
            String formatExpression = replaceExpression(expression,splitList,REPLACEMENT);
            List<String> list = super.parseTree(formatExpression);
            List<String> convert = convert(list, splitList);
            return inorderToPosted(convert);
        }

    }

    private List<String> convert(List<String> list, List<ExpressionDTO> splitList) {
        if (CollectionUtils.isEmpty(list)) {
            return new ArrayList<>();
        }
        List<String> result = new ArrayList<>();
        list.forEach(each->{
             if(StringUtils.startsWith(each,REPLACEMENT)){
                 String endIndex = StringUtils.substringAfter(each, REPLACEMENT);
                 Optional<ExpressionDTO> first = splitList.stream().filter(x -> x.getEndIndex().equals(Integer.valueOf(endIndex))).findFirst();
                 if(first.isPresent()){
                     StringBuffer expression = first.get().getExpression();
                     if(expression!=null){
                         result.add(ParenthesesRegex.removeExcludeParentheses(expression.toString()));
                     }
                 }
             }else{
                 result.add(each);
             }
        });
        return result;
    }

    private String replaceExpression(String expression, List<ExpressionDTO> splitList, String replacement) {
         if(CollectionUtils.isEmpty(splitList)){
             return expression;
         }else{
             StringBuffer stringBuffer = new StringBuffer(expression);
             splitList = splitList.stream().sorted(Comparator.comparing(ExpressionDTO::getStartIndex).reversed()).collect(Collectors.toList());
             splitList.forEach(each->{
                 // end 无法取到 要+1
                 stringBuffer.replace(each.getStartIndex(),each.getEndIndex()+1,replacement+each.getEndIndex());
             });
             return stringBuffer.toString();
         }

    }
}
