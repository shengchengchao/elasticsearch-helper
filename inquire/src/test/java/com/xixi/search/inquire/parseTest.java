package com.xixi.search.inquire;

import com.xixi.search.inquire.transform.parse.NestExpressionParse;
import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/17
 */
@RunWith(SpringRunner.class)
@SpringBootTest
@Slf4j
public class parseTest {





    @Test
    public void testParser(){
        String s = "lastName=Kitai&&(hireDate>1955-08-16&&gender=M)||firstName=Parto";
        NestExpressionParse nestExpressionParse = new NestExpressionParse();
        List<String> list = nestExpressionParse.nestParseTree(s);
        System.out.println(list.stream().collect(Collectors.joining(",")));
    }

    @Test
    public void testParser4(){
        String s = "x>5||c>5";
        NestExpressionParse nestExpressionParse = new NestExpressionParse();
        List<String> list = nestExpressionParse.nestParseTree(s);
        System.out.println(list.stream().collect(Collectors.joining(",")));

    }
                                    

    @Test
    public void testParser5(){
       

    }

    @Test
    public void testParser2(){
        String s = "a>2&&b<3||(c<4&&e>5)";
        NestExpressionParse nestExpressionParse = new NestExpressionParse();
        List<String> list = nestExpressionParse.nestParseTree(s);
        System.out.println(list.stream().collect(Collectors.joining(",")));
    }


    @Test
    public void testParser3(){
        String s = "a>2&&b<3&&(h<5||x>5)||(((x<5)))";
        NestExpressionParse nestExpressionParse = new NestExpressionParse();
        List<String> list = nestExpressionParse.nestParseTree(s);
        System.out.println(list.stream().collect(Collectors.joining(",")));
    }


}
