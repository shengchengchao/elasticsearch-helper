package com.xixi.search.common.param;

import lombok.Data;

import java.util.Map;

/**
 * 扩展检索参数
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
@Data
public class ExtentSearchParam extends SearchParam {


    /**
     * 自定义的map
     */
    private Map extendMap;


}
