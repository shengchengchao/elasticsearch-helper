package com.xixi.search.inquire.printer;

import com.xixi.search.common.enums.PrintStyleEnum;

/**
 * 
 * @author shengchengchao
 *
 */
public final class BinaryTrees {

	private BinaryTrees() {
	}

	public static void print(BinaryTreeInfo tree) {
		print(tree, null);
	}

	public static void println(BinaryTreeInfo tree) {
		println(tree, null);
	}

	public static void print(BinaryTreeInfo tree, String style) {
		if (tree == null || tree.root() == null) return;
		printer(tree, style).print();
	}

	public static void println(BinaryTreeInfo tree,  String style) {
		if (tree == null || tree.root() == null) return;
		printer(tree, style).println();
	}

	public static String printString(BinaryTreeInfo tree) {
		return printString(tree, null);
	}

	public static String printString(BinaryTreeInfo tree,  String style) {
		if (tree == null || tree.root() == null) return null;
		return printer(tree, style).printString();
	}

	private static Printer printer(BinaryTreeInfo tree,  String style) {
		if (PrintStyleEnum.INORDER.getCode().equals(style)) return new InorderPrinter(tree);
		return new LevelOrderPrinter(tree);
	}


}