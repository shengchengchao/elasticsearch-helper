package com.xixi.search.inquire.transform.tree;

import com.xixi.search.inquire.transform.dto.ExpressionTree;
import org.apache.commons.lang3.StringUtils;

import java.util.*;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/15
 */
public class TreeOperate implements BaseTreeOperate {

    private static TreeOperate instance;

    private TreeOperate (){}

    public static synchronized TreeOperate getInstance() {
        if (instance == null) {
            instance = new TreeOperate();
        }
        return instance;
    }


    @Override
    public ExpressionTree getEndLeft(ExpressionTree tree) {
        ExpressionTree left = tree;
        while(left != null && left.getLeft() != null  ){
            left = left.getLeft();
        }
        return left;
    }

    @Override
    public ExpressionTree getEndRight(ExpressionTree tree) {
        ExpressionTree right = tree;
        while(right != null && right.getRight() != null ){
            right = right.getRight();
        }
        return right;
    }

    @Override
    public ExpressionTree getLeftAndCount(ExpressionTree tree, Integer count) {
        ExpressionTree left = tree;
        while(left != null && left.getLeft() != null && count!=0 ){
            left = left.getLeft();
            count--;
        }
        return left;
    }

    @Override
    public ExpressionTree getRightAndCount(ExpressionTree tree, Integer count) {
        ExpressionTree right = tree;
        while(right != null && right.getLeft() != null && count!=0 ){
            right = right.getLeft();
        }
        return right;
    }

    /**
     * 前序遍历
     * 中左右 -> 右左中
     * @param root
     * @return
     */
    @Override
    public List<String> preOrderTraversal(ExpressionTree root) {
        List<String> res = new ArrayList<>();  //保存结果
        Stack<ExpressionTree> call = new Stack<>();   //调用栈
        call.push(root);    //先将根结点入栈
        while (!call.isEmpty()) {
            ExpressionTree t = call.pop();
            if (t != null) {
                if (t.getRight() != null) call.push(t.getRight());
                if (t.getLeft() != null) call.push(t.getLeft());
                call.push(t);   //完全模拟递归
                call.push(null);
            } else {
                res.add(call.pop().getValue());
            }
        }
        return res;
    }

    /**
     * 中序遍历
     *    左中右 -> 右中左
     * @param root
     * @return
     */
    @Override
    public List<String> inorderTraversal(ExpressionTree root) {
        List<String> res = new ArrayList<>();  //保存结果
        Stack<ExpressionTree> call = new Stack<>();   //调用栈
        call.push(root);    //先将根结点入栈
        while (!call.isEmpty()) {
            ExpressionTree t = call.pop();
            if (t != null) {
                if (t.getRight() != null) call.push(t.getRight());
                call.push(t);   //完全模拟递归
                call.push(null);
                if (t.getLeft() != null) call.push(t.getLeft());
            } else {
                res.add(call.pop().getValue());
            }
        }
        return res;
    }

    /**
     * 后序遍历
     *   左右中 -> 中右左
     * @param root
     * @return
     */
    @Override
    public List<String> postOrderTraversal(ExpressionTree root) {
        List<String> res = new ArrayList<>();  //保存结果
        Stack<ExpressionTree> call = new Stack<>();   //调用栈
        call.push(root);    //先将根结点入栈
        while (!call.isEmpty()) {
            ExpressionTree t = call.pop();
            if (t != null) {
                call.push(t);   //完全模拟递归
                call.push(null);
                if (t.getRight() != null) call.push(t.getRight());
                if (t.getLeft() != null) call.push(t.getLeft());
            } else {
                res.add(call.pop().getValue());
            }
        }
        return res;
    }

    /**
     * 中序遍历
     *
     * @param root
     * @return
     */
    @Override
    public List<String> levelOrderBottom(ExpressionTree root) {
        Queue<ExpressionTree> queue = new LinkedList<>();
        List<String> result = new ArrayList<>();
        if(root ==null){
            return result;
        }
        queue.offer(root);
        while (!queue.isEmpty()){
            int len =queue.size(); //记录层级的含有节点数量
            for(int i=0;i<len;i++){
                ExpressionTree p =queue.poll();
                if(p==null){
                    result.add(StringUtils.EMPTY);
                }else{
                    result.add(p.getValue());
                    queue.offer(p.getLeft());
                    queue.offer(p.getRight());
                }
            }
        }
        return result;
    }
}
