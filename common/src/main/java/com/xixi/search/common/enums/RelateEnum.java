package com.xixi.search.common.enums;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/12
 */
public enum RelateEnum {
    /**各种表达式*/
    EQ("=","等于"),
    NE("!=","不等于"),
    GT(">","大于"),
    GE(">=","大于等于"),
    LT("<","小于"),
    LE("<=","小于等于"),
    AND("&&","与"),
    OR("||","或");

    private String code;
    private String value;

    private RelateEnum(String code, String value) {
        this.code = code;
        this.value = value;
    }

    public String getCode() {
        return code;
    }

    public String getValue() {
        return value;
    }


}
