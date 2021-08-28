package com.xixi.search.inquire.query.search.handle;

import lombok.Data;

import java.util.Map;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
@Data
public class EsHandleParam {

    private BaseHandler handle;

    private Map param;
}
