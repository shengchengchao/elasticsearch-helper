package com.xixi.search.common.result;

import com.xixi.search.common.enums.ResultCodeEnum;
import lombok.Data;

import java.util.Objects;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/27
 */
@Data
public class Result<T>  {

    private Boolean success;

    private Integer code;

    private String message;

    private T data;

    public Result() {
    }

    private static <T> Result<T> newInstance(Integer code, String message, T data) {
        Result<T> result = new Result<>();
        result.setMessage(message);
        result.setCode(code);
        result.setData(data);
        result.setSuccess(Objects.equals(code, ResultCodeEnum.SUCCESS.getCode()));
        return result;
    }



    public static <T> Result<T> fail(String failMessage) {
        return newInstance(ResultCodeEnum.FAILED.getCode(), failMessage, null);
    }

    public static <T> Result<T> fail(Integer code, String message) {
        return newInstance(code, message, null);
    }
    public static <T> Result<T> success(Integer code, String message) {
        return newInstance(code, message, null);
    }

    public static <T> Result<T> success(T data) {
        return newInstance(ResultCodeEnum.SUCCESS.getCode(), ResultCodeEnum.SUCCESS.getMessage(), data);
    }

    public static <T> Result<T> success() {
        return newInstance(ResultCodeEnum.SUCCESS.getCode(), ResultCodeEnum.SUCCESS.getMessage(), null);
    }

    public boolean isSuccess() {
        return this.success;
    }
}
