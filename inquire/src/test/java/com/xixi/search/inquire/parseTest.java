package com.xixi.search.inquire;

import com.xixi.search.inquire.printer.BinaryTreeInfo;
import com.xixi.search.inquire.printer.BinaryTrees;
import com.xixi.search.inquire.transform.dto.ExpressionTree;
import com.xixi.search.inquire.transform.parse.NestExpressionParse;
import com.xixi.search.inquire.transform.tree.TreeOperate;
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


    public void printTree(ExpressionTree finalTree){
        BinaryTreeInfo info1 = new BinaryTreeInfo() {
            @Override
            public Object root() {
                return finalTree;
            }

            @Override
            public Object left(Object node) {
                ExpressionTree tree = (ExpressionTree) node;
                if(tree.getLeft()==null) {
                    return null;
                }else{
                    return tree.getLeft();
                }
            }

            @Override
            public Object right(Object node) {
                ExpressionTree tree = (ExpressionTree) node;
                if(tree.getRight()==null) {
                    return null;
                }else{
                    return tree.getRight();
                }
            }

            @Override
            public Object string(Object node) {
                if(node!=null){
                    return ((ExpressionTree)node).getValue();
                }else{
                    return "";
                }
            }

        };
        BinaryTrees.println(info1);
    }


    @Test
    public void testParser(){
        String s = "e>5&&a>2||c<4||c<7||d>9";
        NestExpressionParse nestExpressionParse = new NestExpressionParse();
        ExpressionTree tree = nestExpressionParse.parseTree(s);
        printTree(tree);
        List<String> list = TreeOperate.getInstance().inorderTraversal(tree);
        System.out.println(list.stream().collect(Collectors.joining(",")));
    }

    @Test
    public void testParser4(){
        String s = "h<5&&x>5||c>5";
        NestExpressionParse nestExpressionParse = new NestExpressionParse();
        ExpressionTree tree = nestExpressionParse.parseTree(s);
        printTree(tree);
    }



    @Test
    public void testParser2(){
        String s = "a>2&&b<3||(c<4&&e>5)";
        NestExpressionParse nestExpressionParse = new NestExpressionParse();
        ExpressionTree tree = nestExpressionParse.nestParseTree(s);
        ExpressionTree finalTree = tree;
        printTree(finalTree);
    }


    @Test
    public void testParser3(){
        String s = "a>2&&b<3&&(h<5||x>5)||(((x<5)))";
        NestExpressionParse nestExpressionParse = new NestExpressionParse();
        ExpressionTree tree = nestExpressionParse.nestParseTree(s);
        ExpressionTree finalTree = tree;
        printTree(finalTree);
    }
}
