package com.xixi.search.transport.etl;

import lombok.Data;

import java.util.List;

/** 数据处理分页类
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/31
 */
@Data
public class DataPage<T> extends SimplePage {






    private List<String> relationList;


    private List<T> dataResultList;


    private String taskName;

}
