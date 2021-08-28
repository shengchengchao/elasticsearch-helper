package com.xixi.search.inquire.query.search.parse;


import com.xixi.search.common.constant.EsConstants;
import com.xixi.search.common.constant.RegexConstants;

import java.util.List;
import java.util.Map;

/**
 * 表达式解析器
 */
public abstract class AbstractRegexParse {
    
    /**
     * 根据表达式开始解析 分拆为key value的对应关系
     * @param esFieldRegex es的表达式字段
     * @param result key-value的结果集
     * @return 中间符号是或（||）还是与（&&） 是或返回true 否则返回false 
     */
    abstract boolean buildKeyValueToResult(String esFieldRegex, Map<String, String[]> result);

    /**
     * 根据表达式解析出对应的数据
     * @param esFieldRegex
     * @param result
     */
    abstract void transferToResult(String esFieldRegex, Map<String, String[]> result);

    /**
     * 分割参数
     * xx.yy=*1*&&xx.zz=2
     * 先分割xx.yy=*1* 和 xx.zz=1,2,3
     * 再根据 = 分割 xx.yy *1*  xx.zz 2 value再进行多值分割的方式存入map
     * @param param 对应的表达式参数
     * @param result key-value的结果集
     */
    public void splitParam(String param, Map<String, String[]> result) {

    }

    /**
     * 表达式过滤器，传入目标的list,返回过滤后的数据
     * @param targets
     * @param fieldRegex
     * @return
     */
    public List<Map<String, Object>> filterByRegex(List<Map<String, Object>> targets,String fieldRegex) {
       return null;


    }

    /**
     * 表达式判断 逻辑与或非
     *
     * @param valueRegex
     * @return
     */
    public String getInverterRelation(String valueRegex) {
        if (RegexConstants.isLikeRegex(valueRegex)) {
            return EsConstants.SEARCH_TYPE_WILDCARD;
        }
        if (RegexConstants.isNotRegex(valueRegex)) {
            return EsConstants.SEARCH_TYPE_NOT;
        }
        if (RegexConstants.isRangeRegex(valueRegex)) {
            return EsConstants.SEARCH_TYPE_REGEX;
        }
        return EsConstants.SEARCH_TYPE_TERM;
    }


    /**
     * 将一个表达式分割，并返回分割的类型
     * @param regex 表达式
     * @param result 保存结果
     * @return
     */
    public String splitRegexSaveToResult(String regex, List<String> result) {
          return null;
    }

    /**
     * 按照空格进行分割
     * @param str
     * @return
     */
    public List<String> splitByBlank(String str){
          return null;
    }


}