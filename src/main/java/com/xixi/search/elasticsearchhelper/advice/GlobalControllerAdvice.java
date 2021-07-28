package com.xixi.search.elasticsearchhelper.advice;

import com.xixi.search.elasticsearchhelper.result.Result;
import com.xixi.search.elasticsearchhelper.result.ResultCodeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import javax.servlet.http.HttpServletResponse;
import java.text.MessageFormat;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/27
 */
@RestControllerAdvice
@Slf4j
public class GlobalControllerAdvice {

    /**
     * 其他异常统一处理
     */
    @ExceptionHandler(value = Exception.class)
    public Result<Object> handleException(HttpServletResponse response, Exception e) {
        log.error(MessageFormat.format("message: {0}", e.getMessage()), e);
        return Result.fail(ResultCodeEnum.FAILED.getMessage());
    }



}
