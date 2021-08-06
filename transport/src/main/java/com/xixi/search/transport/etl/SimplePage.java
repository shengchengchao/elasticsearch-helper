package com.xixi.search.transport.etl;

import lombok.Data;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/1
 */
@Data
public class SimplePage {


    Long pageNum=1L;



    Long pageSize;

    /**
     * 成功标志
     */
    private Boolean successFlag;
}
