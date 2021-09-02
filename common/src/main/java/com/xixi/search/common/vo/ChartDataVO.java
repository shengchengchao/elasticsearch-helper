package com.xixi.search.common.vo;

import com.xixi.search.common.dto.AggDataDTO;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * @author huajiejun
 * @date 2020/5/9 3:17 下午
 */
@Data
public class ChartDataVO implements Serializable {

    private String chartName;

    private List<AggDataDTO> aggData;

    public ChartDataVO(String chartName, List<AggDataDTO> aggData) {
        this.chartName = chartName;
        this.aggData = aggData;
    }

    public ChartDataVO() {
    }
}
