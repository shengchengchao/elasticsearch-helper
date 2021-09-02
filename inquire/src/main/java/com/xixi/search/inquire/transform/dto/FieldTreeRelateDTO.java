package com.xixi.search.inquire.transform.dto;

import lombok.Data;

import java.util.List;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/9/1
 */
@Data
public class FieldTreeRelateDTO extends FieldRelateDTO {

    private List<FieldTreeRelateDTO> childrenList;

    private String treeType;
}
