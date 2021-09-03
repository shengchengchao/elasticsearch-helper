package com.xixi.search.example;

import com.google.common.collect.Lists;
import com.xixi.search.common.constant.EsConstants;
import com.xixi.search.common.dto.AggParamDTO;
import com.xixi.search.common.param.ExtentSearchPageParam;
import com.xixi.search.common.util.PagingHelper;
import com.xixi.search.common.vo.ChartDataVO;
import com.xixi.search.example.search.EmployeeAggQuery;
import com.xixi.search.example.search.EmployeeExpressionSearchQuery;
import com.xixi.search.example.transport.index.EmployeeEsEntity;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.ArrayList;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/25
 */
@RunWith(SpringRunner.class)
@SpringBootTest
@Slf4j
class SearchTest {


    @Autowired
    private EmployeeExpressionSearchQuery employeeSearchQuery;

    @Autowired
    private EmployeeAggQuery employeeAggQuery;

    @Test
    public void getResults(){
        ExtentSearchPageParam extentSearchPageParam = new ExtentSearchPageParam();
        extentSearchPageParam.setSearchExpression("lastName=Kitai&&(hireDate>1955-08-16&&gender=M &&(salary.salary>60000))||firstName=Parto");
        PagingHelper<EmployeeEsEntity> employeeEsEntityPagingHelper = employeeSearchQuery.selectPage(extentSearchPageParam);
        System.out.println(employeeEsEntityPagingHelper.getRecords().toString());

    }


    @Test
    public void aggreTest(){
        String expression = "lastName=Kitai&&(hireDate>1955-08-16&&gender=M)||firstName=Parto";
        AggParamDTO aggParamDTO = new AggParamDTO();
        aggParamDTO.setSearchField("salary.salary");
        aggParamDTO.setSearchTyped(FieldType.Nested.name());
//        AggParamDTO children = new AggParamDTO();
//        children.setSearchTyped(EsConstants.AGG_TERMS);
//        children.setSearchField("deptList.deptName");
//        aggParamDTO.setChild(children);
        ArrayList<String> rangeList = Lists.newArrayList("3000-4000", "40000-450000");
        aggParamDTO.setRangeList(rangeList);
        ChartDataVO chartDataVO = employeeAggQuery.rangeHistogram(expression, aggParamDTO);
        System.out.println(chartDataVO);
    }

    @Test
    public void aggreTest2(){
        String expression = "lastName=Kitai&&(hireDate>1955-08-16&&gender=M)||firstName=Parto";
        AggParamDTO aggParamDTO = new AggParamDTO();
        aggParamDTO.setSearchField("salary.fromDate");
        aggParamDTO.setSearchTyped(FieldType.Nested.name());
//        AggParamDTO children = new AggParamDTO();
//        children.setSearchTyped(EsConstants.AGG_TERMS);
//        children.setSearchField("deptList.deptName");
//        aggParamDTO.setChild(children);
        aggParamDTO.setUnit(EsConstants.INTERVAL_MONTH);
        ChartDataVO chartDataVO = employeeAggQuery.dateHistogram(expression, aggParamDTO);
        System.out.println(chartDataVO);
    }
}
