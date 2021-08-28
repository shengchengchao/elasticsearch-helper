package com.xixi.search.common.util;

import lombok.Data;
import org.springframework.data.elasticsearch.core.aggregation.AggregatedPage;

import java.util.List;
import java.util.Map;

/**
 * 分页对象
 * 
 * @description 用于封装分页<br/>
 */
@Data
public class PagingHelper<T> implements java.io.Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/** 每页大小 */
	private int size = 10;
	/** 当前页 */
	private int current = 1;
	/** 包含的记录数据 */
	private List<T> records;
	/** 总的记录数 */
	private long total = 0;
	/** 其他参数 */
	Map<String, Object> otherParam;


	public void fromEs(AggregatedPage<T> page) {
		if (page == null) {
			return;
		}

		this.setTotal(page.getTotalElements());
		this.setCurrent(page.getPageable().getPageNumber() + 1);
		this.setSize(page.getPageable().getPageSize());
		this.setRecords(page.getContent());
	}

	public PagingHelper() {
	}

	public PagingHelper(List<T> ls) {
		this.records = ls;
		this.total = ls.size();
	}

	public int getCurrent() {
		return current;
	}

	public void setCurrent(int current) {
		this.current = current;
	}

	/**
	 * 返回总页数
	 * 
	 * @return
	 */
	public int getPageNum() {
		if (total % size > 0) {
			return (int) (total / size) + 1;
		} else {
			return (int) (total / size);
		}
	}

	public List<T> getRecords() {
		return records;
	}

	public void setRecords(List<T> result) {
		this.records = result;
	}

	public long getTotal() {
		return total;
	}

	public void setTotal(long totalRowSize) {
		this.total = totalRowSize;
	}

	public boolean haveNextPage() {
		if ((current - 1) * size + records.size() < total) {
			return true;
		} else {
			return false;
		}
	}

	public boolean havePreviousPage() {
		if (current > 1) {
			return true;
		} else {
			return false;
		}
	}

	public int getPreviousPageNum() {
		return current - 1;
	}

	public int getNextPageNum() {
		return current + 1;
	}

	public void setSize(int size) {
		this.size = size;
	}

	public int getSize() {
		return size;
	}

	/**
	 * 开始记录数
	 * 
	 * @return
	 */
	public int getBeginRowNumber() {
		return (this.getCurrent() - 1) * this.getSize();
	}

	/**
	 * 结束记录数
	 * 
	 * @return
	 */
	public int getEndRowNumber() {

		return (this.getCurrent()) * this.getSize();
	}

}
