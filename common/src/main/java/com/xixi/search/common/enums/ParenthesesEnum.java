package com.xixi.search.common.enums;

import java.util.Arrays;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/15
 */
public enum ParenthesesEnum {

    BIG_PARENTHESES('{','}'),
    MIDDLE_PARENTHESES('[',']'),
    SMALL_PARENTHESES('(',')');



    private Character left;
    private Character right;

    ParenthesesEnum(Character left, Character right) {
        this.left = left;
        this.right = right;
    }

    public Character getLeft() {
        return left;
    }

    public Character getRight() {
        return right;
    }


    public static Character getValueByLeft(Character left) {
        return Arrays.stream(ParenthesesEnum.values()).filter(x -> x.getLeft()==left).map(ParenthesesEnum::getRight).findFirst().orElse(' ');
    }
}
