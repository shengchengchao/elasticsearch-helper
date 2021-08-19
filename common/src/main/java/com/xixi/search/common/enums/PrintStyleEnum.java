package com.xixi.search.common.enums;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/17
 */
public enum PrintStyleEnum {


    /**遍历方式*/
    LEVEL_ORDER("LEVEL_ORDER","层序打印"),
    INORDER("INORDER","中序打印");

    private String code;
    private String value;

    private PrintStyleEnum(String code, String value) {
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
