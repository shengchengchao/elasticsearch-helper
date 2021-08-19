package com.xixi.search.inquire.transform.parse;

import com.xixi.search.common.advice.ElasticSearchAssert;
import com.xixi.search.inquire.transform.dto.ExpressionDTO;
import com.xixi.search.inquire.transform.dto.ExpressionTree;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
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
    public ExpressionTree nestParseTree(String expression) {
        ElasticSearchAssert.meetCondition(StringUtils.isBlank(expression) ||StringUtils.isBlank(ParenthesesRegex.removeExcludeParentheses(expression)) ,"表达式为空");
        //先校验括号是否匹配
        ElasticSearchAssert.meetCondition(!ParenthesesRegex.isSymbolTwin(expression),"该表达式括号匹配失败");
        // 多层嵌套的括号 要扒干净 比如 ((a)) -> a
        expression = ParenthesesRegex.removeExcludeParentheses(expression);
        List<ExpressionDTO> splitList = ParenthesesRegex.getSymbolStr(expression, '(');
        // 这里做法是这样的 上一行得到了文本中带括号的语句集合，之后将带括号的替换成nestParse
        String formatExpression = replaceExpression(expression,splitList,REPLACEMENT);
        ExpressionTree tree = super.parseTree(formatExpression);
        AtomicInteger count=new AtomicInteger(0);
        convert(tree,splitList,count);
        return tree;
    }

    private void convert(ExpressionTree tree, List<ExpressionDTO> splitList,AtomicInteger count) {
        if (tree == null) {
            return;
        }
        convert(tree.getLeft(),splitList,count);
        if(REPLACEMENT.equals(tree.getValue())){
            ExpressionDTO expressionDTO = Optional.ofNullable(splitList.get(count.getAndIncrement())).orElse(new ExpressionDTO());
            ElasticSearchAssert.meetCondition(expressionDTO.getExpression()==null ,"表达式为空");
            String s = expressionDTO.getExpression().toString();
            s = ParenthesesRegex.removeExcludeParentheses(s);
            tree.setValue(s);
        }
        convert(tree.getRight(),splitList,count);
    }

    private String replaceExpression(String expression, List<ExpressionDTO> splitList, String replacement) {
         if(CollectionUtils.isEmpty(splitList)){
             return expression;
         }else{
             StringBuffer stringBuffer = new StringBuffer(expression);
             splitList = splitList.stream().sorted(Comparator.comparing(ExpressionDTO::getStartIndex).reversed()).collect(Collectors.toList());
             splitList.forEach(each->{
                 // end 无法取到 要+1
                 stringBuffer.replace(each.getStartIndex(),each.getEndIndex()+1,replacement);
             });
             return stringBuffer.toString();
         }

    }






}
