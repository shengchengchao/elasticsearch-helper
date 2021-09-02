package com.xixi.search.common.dto;

import lombok.Data;

import java.util.List;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/31
 */
@Data
public class SearchTreeDTO extends BaseSearchDTO {

    private List<SearchTreeDTO> childrenLists;
    /**类型 是 树父节点 LOGIC 还是 CONDITION*/
    private String type;
}
