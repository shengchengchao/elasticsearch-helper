package com.xixi.search.example;

import com.xixi.search.common.param.ExtentSearchPageParam;
import com.xixi.search.common.util.PagingHelper;
import com.xixi.search.example.search.EmployeeSearchQuery;
import com.xixi.search.example.transport.index.EmployeeEsEntity;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

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
    private EmployeeSearchQuery employeeSearchQuery;

    @Test
    public void getResults(){
        ExtentSearchPageParam extentSearchPageParam = new ExtentSearchPageParam();
        extentSearchPageParam.setSearchExpression("lastName=Kitai&&(hireDate>1955-08-16&&gender=M &&(salary.salary>60000))||firstName=Parto");
        PagingHelper<EmployeeEsEntity> employeeEsEntityPagingHelper = employeeSearchQuery.selectPage(extentSearchPageParam);
        System.out.println(employeeEsEntityPagingHelper.getRecords().toString());

    }
}
