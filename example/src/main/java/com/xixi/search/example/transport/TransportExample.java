package com.xixi.search.example.transport;

import com.xixi.search.common.result.Result;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

/**
 * 传输实例类
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/3
 */
public class TransportExample {

    @Autowired
    private EmployeeEtl employeeEtl;


    @GetMapping("/etl/{task}")
    public Result etlExample(@PathVariable String task) {
        employeeEtl.dataImport(task);
        return Result.success();
    }

}
