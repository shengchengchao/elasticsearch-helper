package com.xixi.search.inquire.transform.parse;

import com.xixi.search.common.advice.ElasticSearchAssert;
import com.xixi.search.common.enums.ParenthesesEnum;
import com.xixi.search.inquire.transform.dto.ExpressionDTO;

import java.util.*;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/13
 */
public class ParenthesesRegex {

    static Map<Character, Character> bracket = new HashMap<>();

    static{
        bracket.put(')','(');
    }
   

    /**
     * 括号是否成对出现
     * 利用栈的先进后出特性来判断字符串的符合是否成对出现
     * 出现一次左边的进栈，出现一次右边的出栈，栈最终为空即为成对出现
     *
     * @param text
     * @return
     */
    public static boolean isSymbolTwin(String text) {
        //定义左右括号关系
        Stack stack = new Stack<>(); //定义栈
        for (int i = 0; i < text.length(); i++) {
            Character nowStr = text.charAt(i); //转换成字符串
            //是否为左括号
            if (bracket.containsValue(nowStr)) {
                stack.push(nowStr);
            } else if (bracket.containsKey(nowStr)) { //是否为右括号
                if (stack.isEmpty()) return false;
                if (stack.peek().equals(bracket.get(nowStr))) { //左右括号匹配
                    stack.pop(); //退栈
                } else {
                    return false;
                }
            }
        }
        return stack.isEmpty() ? true : false;
    }


    /**
     *
     * @param text   语句
     * @param parentheses 指定的符号 比如 (
     * @return
     */
    public static List<ExpressionDTO> getSymbolStr(String text,Character parentheses) {
        ElasticSearchAssert.meetCondition(!bracket.containsValue(parentheses),"暂不支持的符号:"+parentheses);
        //残缺的括号内容
        List<ExpressionDTO> bracketList = new LinkedList<>();
        List<ExpressionDTO> result = new ArrayList<>();
        for (int x = 0; x < text.length(); x++) {
            Character nowStr = text.charAt(x);
            if (bracket.containsValue(nowStr) && nowStr.equals(parentheses)) { //如果是左括号
                //如果不是第一次左括号说明之前还有左括号如：（工信部网安〔2018〕105号） 有俩左括号
                if (bracketList.size() > 0) {
                    for (int i = 0; i < bracketList.size(); i++) {
                        ExpressionDTO expressionDTO = bracketList.get(i);
                        expressionDTO.getExpression().append(nowStr);
                    }
                }
                StringBuffer sb = new StringBuffer();
                sb.append(nowStr);
                ExpressionDTO expressionDTO = new ExpressionDTO();
                expressionDTO.setStartIndex(x);
                expressionDTO.setExpression(sb);
                bracketList.add(expressionDTO);
                //是右括号
            } else if (bracket.containsKey(nowStr) && nowStr.equals(ParenthesesEnum.getValueByLeft(parentheses))) {
                for (int i = 0; i < bracketList.size(); i++) {
                    ExpressionDTO expressionDTO = bracketList.get(i);

                    StringBuffer expression = expressionDTO.getExpression();
                    //添加右括号
                    expression.append(nowStr);
                    //判断当前文本是否符合成对符号
                    if (isSymbolTwin(expression.toString())) {
                        //符合
                        expressionDTO.setEndIndex(x);
                        result.add(expressionDTO);
                        //删除已经成对的内容 确保不会出现多次
                        bracketList.remove(expressionDTO);
                    } else {
                    }
                }
                //已经有了左括号
            } else if (bracketList.size() > 0) {
                for (int i = 0; i < bracketList.size(); i++) {
                    ExpressionDTO expressionDTO = bracketList.get(i);
                    expressionDTO.getExpression().append(nowStr);
                }
            }
        }
        return result;
    }

    public static void main(String[] args) {
        String s ="a>2 && b>3 || (c<5 && d>6 || (e<5))";
        List<ExpressionDTO> symbolStr = getSymbolStr(s, '(');
        System.out.println(symbolStr);
    }
}