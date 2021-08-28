package com.xixi.search.inquire.query.search.handle;

import com.xixi.search.common.index.IndexOperate;
import com.xixi.search.common.param.BaseQueryParam;

import java.util.Map;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
public interface BaseHandler {

    /**
     * 执行构建过程 这里最后会将map中需要转换的参数 都放在baseQueryParam 对象中
     * 主要是放在  excludeQuery otherQuery 中
     * @param param 处理的参数
     * @param baseQueryParam 封装的对象
     */
    void execute(IndexOperate baseEsIndex, Map param, BaseQueryParam baseQueryParam);
}
