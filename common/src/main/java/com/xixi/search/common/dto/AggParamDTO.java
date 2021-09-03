package com.xixi.search.common.dto;

import lombok.Data;

import java.util.List;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/29
 */
@Data
public class AggParamDTO {


    private String searchField;

    private String searchTyped;



    private Integer size;


    private AggParamDTO child;

    private String unit;

    private List<String> rangeList;
}

