package com.xixi.search.common.param;

import lombok.Data;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
@Data
public class ExtentSearchPageParam extends ExtentSearchParam {

    /** 页数 */
    private Integer pageNum;

    /** 每页总数 */
    private Integer pageCount;

}
