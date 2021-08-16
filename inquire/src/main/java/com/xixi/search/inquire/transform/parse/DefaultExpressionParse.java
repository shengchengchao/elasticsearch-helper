package com.xixi.search.inquire.transform.parse;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.xixi.search.common.enums.RelateEnum;
import com.xixi.search.inquire.transform.dto.ExpressionTree;
import com.xixi.search.inquire.transform.tree.TreeOperate;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/11
 */
public class DefaultExpressionParse implements ExpressionParse {


    @Override
    public ExpressionTree parseTree(String expression) {
        if(StringUtils.isBlank(expression)){
            return new ExpressionTree();
        }
        // 先按照&& 进行拆分 遍历每一个元素 如果元素中包含 || 就继续进行拆分
        List<String> midList = Splitter.on(RelateEnum.AND.getCode()).splitToList(expression);
        ExpressionTree expressionTree =null;
        TreeOperate treeOperate = new TreeOperate();
        for (String s : midList) {
            ExpressionTree tree =null;
            if(StringUtils.contains(s,RelateEnum.OR.getCode())){
                List<String> list = Splitter.on(RelateEnum.OR.getCode()).splitToList(s);
                tree = packageNode(list, RelateEnum.OR.getCode());
            }else{
                tree = packageNode(Lists.newArrayList(s), RelateEnum.AND.getCode());
            }
            ExpressionTree header =treeOperate.getEndLeft(tree);
            header.setLeft(expressionTree);
            expressionTree=tree;
        }
        return expressionTree;
    }

    private ExpressionTree packageNode(List<String> list, String code) {
        ExpressionTree tree =null;
        for (String s : list) {
            ExpressionTree leftTree = new ExpressionTree(s);
            leftTree.setLeft(tree);
            tree=new ExpressionTree(code);
            tree.setLeft(leftTree);
        }
        return tree;
    }



    public static void main(String[] args) {
        DefaultExpressionParse defaultExpressionParse = new DefaultExpressionParse();
        ExpressionTree tree = defaultExpressionParse.parseTree("d!=3&&a>2||b<3||c>4&&e>5");
        System.out.println(tree);
    }

}
