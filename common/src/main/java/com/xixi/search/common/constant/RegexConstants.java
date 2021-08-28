package com.xixi.search.common.constant;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/21
 */
public class RegexConstants {

    /**
     * 表达式中的问号
     */
    public static final String REGEX_QUESTION = "?";

    /**
     * 表达式里面的竖杆
     */
    public static final String REGEX_OR_STR = "\\|\\|";
    public static final String REGEX_OR_STR_NORMAL = "||";
    /**
     * 表达式中的与语法
     */
    public static final String REGEX_AND_STR = "&&";

    /**
     * 范围比较开始的字符串
     */
    public static final String RAND_START_STR = "[";
    public static final String RAND_START_STR_REPLACE = "\\[";
    /**
     * 范围比较结束的字符串
     */
    public static final String RAND_END_STR = "]";
    public static final String RAND_START_END_REPLACE = "\\]";
    /**
     * 空格分割
     */
    public static final String SPLIT_BLANK = " ";

    /**
     * 表达式的键值对
     */
    public static final String REGEX_EQ = "=";

    /**
     * 嵌套表的固定格式
     */
    public static final String REGEX_NESTED = "@";

    /**
     * 分割符逗号
     */
    public static final String REGEX_COMMA = ",";

    /**
     * 点
     */
    public static final String REGEX_POINT = ".";

    /**
     * 非 ~1~,~2~
     */
    public static final String REGEX_NOT = "~";

    /**
     * 非正则表达式 ~1~ 不等于1
     */
    public static final String REGEX_NOT_EXP = "(?<=\\~)(.+?)(?=\\~)";

    /**
     * 双模糊 *1234* 1234是模糊表达式
     */
    public static final String REGEX_LIKE_EXP = "(?<=\\*)(.+?)(?=\\*)";

    /**
     * 范围搜索的正则表达式 [122] 122是范围表达式
     */
    public static final String REGEX_RANGE_EXP = "(?<=\\[)(.+?)(?=\\])";


    /**
     * 模糊搜索的符号
     */
    public static final String REGEX_LIKE_SYMBOL = "*";


    /**
     * 根据正则表达式获取值
     * @param content
     * @param exp
     * @return
     */
    public static String getRegexValue(String content, String exp) {
        Pattern xx = Pattern.compile(exp);
        Matcher matcher = xx.matcher(content);

        while(matcher.find()){
            return matcher.group();
        }
        return content;
    }

    /**
     * 是否是模糊的表达式
     * @param content
     * @return
     */
    public static boolean isLikeRegex(String content) {
        Pattern xx = Pattern.compile(REGEX_LIKE_EXP);
        Matcher matcher = xx.matcher(content);
        return matcher.find();
    }

    /**
     * 是否是不等于的表达式
     * @param content
     * @return
     */
    public static boolean isNotRegex(String content) {
        Pattern xx = Pattern.compile(REGEX_NOT_EXP);
        Matcher matcher = xx.matcher(content);
        return matcher.find();
    }

    /**
     * 是否是范围的表达式
     * @param content
     * @return
     */
    public static boolean isRangeRegex(String content) {
        Pattern xx = Pattern.compile(REGEX_RANGE_EXP);
        Matcher matcher = xx.matcher(content);
        return matcher.find();
    }


    /**
     * 是否是精确查询，如果没有特殊符号，则使用精确
     * @param content
     * @return
     */
    public static boolean isTerm(String content) {
        if (content.contains(REGEX_LIKE_SYMBOL)) {
            return false;
        }
        return true;
    }

    /**
     * 判断是否为数字
     * @param content
     * @return
     */
    public static boolean isDigit(String content) {
        if (content.indexOf("=")>0 ||content.indexOf(">")>0 || content.indexOf("<")>0 ) {
            return true;
        }
        return false;
    }
}
