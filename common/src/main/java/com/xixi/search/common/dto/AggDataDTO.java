package com.xixi.search.common.dto;


import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/2/22
 */

public class AggDataDTO implements Serializable {
    private String result;

    /**
     * 百分比
     */
    private BigDecimal percent;

    private String docCount;
    /**
     * 子类
     */
    List<AggDataDTO> children;

    public AggDataDTO(String result, BigDecimal percent, String docCount) {
        this.result = result;
        this.percent = percent;
        this.docCount = docCount;
    }

    public String getResult() {
        return result;
    }

    public BigDecimal getPercent() {
        return percent;
    }

    public String getDocCount() {
        return docCount;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public void setPercent(BigDecimal percent) {
        this.percent = percent;
    }

    public void setDocCount(String docCount) {
        this.docCount = docCount;
    }

    @Override
    public String toString() {
        return "AggDataDTO{" +
                "result='" + result + '\'' +
                ", percent=" + percent +
                ", docCount='" + docCount + '\'' +
                '}';
    }

    public List<AggDataDTO> getChildren() {
        return children;
    }

    public void setChildren(List<AggDataDTO> children) {
        this.children = children;
    }
}
