package com.xixi.search.elasticsearchhelper.result;

/**
 * 枚举了一些常用API操作码
 * Created by shengchengchao on 2019/4/19.
 */
public enum ResultCodeEnum {
    SUCCESS(200, "操作成功"),
    FAILED(500, "操作失败"),
    VALIDATE_FAILED(400, "参数检验失败"),
    UNAUTHORIZED(401, "暂未登录或token已经过期"),
    BUSINESS_FAILED(9901, "业务异常"),
    SYSTEM_FAILED(9999, "系统异常"),
    FORBIDDEN(403, "没有相关权限");
    private Integer code;
    private String message;

    private ResultCodeEnum(Integer code, String message) {
        this.code = code;
        this.message = message;
    }

    public Integer getCode() {
        return code;
    }

    public String getMessage() {
        return message;
    }
}
