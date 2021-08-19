package com.xixi.search.inquire.transform.parse;

import com.google.common.base.Splitter;
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
public abstract class DefaultExpressionParse implements ExpressionParse {

    protected static TreeOperate treeOperate;

    static {
        treeOperate = TreeOperate.getInstance();
    }

    /**
     * 将检索条件变更为树的接口 目前做法是先进行拆分 拆分后将数据进行
     * @param expression
     * @return
     */
    public ExpressionTree parseTree(String expression) {
        if(StringUtils.isBlank(expression)){
            return new ExpressionTree();
        }

        List<String> midList = Splitter.on(RelateEnum.AND.getCode()).splitToList(expression);
        ExpressionTree  expressionTree= packageAndNode(midList);
        if(expressionTree!=null){
            expressionTree = expressionTree.getLeft();
        }
        ExpressionTree result = new ExpressionTree("");
        //由于与节点都在左分支上 只需要遍历节点就可以了
        while(expressionTree!=null){
            if(StringUtils.contains(expressionTree.getValue(), RelateEnum.OR.getCode())){
                ExpressionTree left = expressionTree.getLeft();
                ExpressionTree tree = packageOrNode(expressionTree.getValue());
                ExpressionTree endLeft = treeOperate.getEndLeft(result);
                endLeft.setLeft(tree);
                expressionTree=left;
            }else{
                ExpressionTree endLeft = treeOperate.getEndLeft(result);
                endLeft.setLeft(new ExpressionTree(expressionTree.getValue()) );
                expressionTree =expressionTree.getLeft();
            }
        }
        return result.getLeft();


    }





    /**
     * 包装且节点
     * @param list
     * @param code
     * @return
     */
    protected ExpressionTree packageAndNode(List<String> list) {
        ExpressionTree tree =null;
        for (String express : list) {
            ExpressionTree leftTree = new ExpressionTree(express);
            leftTree.setLeft(tree);
            tree=new ExpressionTree(RelateEnum.AND.getCode());
            tree.setLeft(leftTree);
        }
        return tree;
    }

    /**
     * 包装与节点
     * @param list
     * @param code
     * @return
     */
    protected ExpressionTree packageOrNode(String expression) {
        List<String> list = Splitter.on(RelateEnum.OR.getCode()).splitToList(expression);
        ExpressionTree tree = new ExpressionTree(list.get(0));
        ExpressionTree head =null;
        for (int i = 1; i < list.size(); i++) {
            ExpressionTree right = new ExpressionTree(list.get(i));
            right.setRight(head);
            head=new ExpressionTree(RelateEnum.OR.getCode());
            head.setRight(right);
        }
        head.setLeft(tree);
        return head;
    }


    public abstract ExpressionTree nestParseTree(String expression);




}
