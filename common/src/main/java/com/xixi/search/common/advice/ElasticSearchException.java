package com.xixi.search.common.advice;

import com.xixi.search.common.enums.ResultCodeEnum;

import java.util.Map;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/29
 */
public class ElasticSearchException extends RuntimeException {

    private static final long serialVersionUID = -654893533794556357L;
    // 错误码
    private Integer code;

    //错误消息
    private String msg;

    //ios的事件
    private String event;

    // 参数
    private String[] params;

    //其他信息
    private Map<String, Object> otherInfo;

    String COMMA = ",";

    /**
     * 用错误信息构造
     *
     * @param msg msg
     */
    public ElasticSearchException(String msg) {
        super(msg);
        this.code = ResultCodeEnum.BUSINESS_FAILED.getCode();
        this.msg = msg;
    }

    public ElasticSearchException(Integer code, String msg) {
        super(msg);
        this.code = code;
        this.msg = msg;
    }

    public ElasticSearchException(Integer code, String msg, String event) {
        super(msg);
        this.code = code;
        this.msg = msg;
        this.event = event;
    }

    /**
     * 创建异常
     *
     * @param msg msg
     * @param ex  异常
     */
    public ElasticSearchException(String msg, Throwable ex) {
        super(ex);
        this.msg = msg;
    }

    /**
     * 创建异常
     *
     * @param msg    msg
     * @param params 参数
     */
    public ElasticSearchException(String msg, String[] params) {
        this.msg = msg;
        this.params = params;
    }

    /**
     * 创建异常
     *
     * @param msg    msg
     * @param otherInfo 其他信息
     */
    public ElasticSearchException(Integer code, String msg, Map<String, Object> otherInfo) {

        this.code = code;
        this.msg = msg;
        this.otherInfo = otherInfo;
    }

    /**
     * 创建异常
     *
     * @param msg    msg
     * @param ex     异常
     * @param params 参数
     */
    public ElasticSearchException(String msg, Throwable ex, String[] params) {
        super(ex);
        this.msg = msg;
        this.params = params;
    }

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    /**
     * 取msg
     *
     * @return 获取msg
     */
    public String getMsg() {
        return this.msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }

    /**
     * 取参数
     *
     * @return 去参数
     */
    public String[] getParams() {
        return params;
    }

    public void setParams(String[] params) {
        this.params = params;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(msg);
        if (params != null && params.length > 0) {
            for (Object s : params) {
                sb.append(COMMA).append(s);
            }
        }
        return sb.toString();
    }

    public String getEvent() {
        return event;
    }

    public void setEvent(String event) {
        this.event = event;
    }

}
