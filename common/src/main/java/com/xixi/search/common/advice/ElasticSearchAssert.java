package com.xixi.search.common.advice;

import com.xixi.search.common.enums.ResultCodeEnum;

import java.util.Map;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/3
 */
public class ElasticSearchAssert {


    public static void meetCondition(boolean condition, Integer code, String msg) {
        if (condition) {
            throw new ElasticSearchException(code, msg);
        }
    }

    public static void meetCondition(boolean condition, String msg) {
        if (condition) {
            throw new ElasticSearchException(ResultCodeEnum.BUSINESS_FAILED.getCode(), msg);
        }
    }

    public static void meetCondition(boolean condition, Integer code, String msg, Map<String, Object> otherInfo) {
        if (condition) {
            throw new ElasticSearchException(code, msg, otherInfo);
        }
    }
}
