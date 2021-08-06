package com.xixi.search.common.index;

import com.google.common.collect.Lists;
import com.xixi.search.common.util.EsUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.search.join.ScoreMode;
import org.elasticsearch.index.query.InnerHitBuilder;
import org.elasticsearch.index.query.NestedQueryBuilder;
import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.SearchHit;
import org.elasticsearch.search.SearchHits;
import org.elasticsearch.search.aggregations.AbstractAggregationBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.annotation.Id;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.core.AbstractResultMapper;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.aggregation.AggregatedPage;
import org.springframework.data.elasticsearch.core.query.*;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/28
 */
@Slf4j
public abstract class IndexOperate<T> {

    @Autowired
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    /**
     * 对于T的class
     */
    Class<T> tClass;

    /**
     * 所有的属性
     */
    Field[] declaredFields;

    /**
     * 主键id的字段名字
     */
    String idField;
    /**
     * 索引名
     */
    String indexName;

    public IndexOperate() {
        ParameterizedType genericSuperclass = (ParameterizedType) this.getClass().getGenericSuperclass();
        tClass =(Class<T>) genericSuperclass.getActualTypeArguments()[0];
        declaredFields = tClass.getDeclaredFields();
        Class<? super T> superclass = tClass.getSuperclass();
        if (superclass != null) {
            Field[] superclassDeclaredFields = superclass.getDeclaredFields();
            List<Field> fields = Lists.newArrayList(declaredFields);
            fields.addAll(Lists.newArrayList(superclassDeclaredFields));
            declaredFields = fields.stream().toArray(Field[]::new);
        }
        if(StringUtils.isBlank(idField) && declaredFields!=null && declaredFields.length>0){
            for (Field declaredField : declaredFields) {
                Id annotation = AnnotationUtils.findAnnotation(declaredField, Id.class);
                if(annotation!=null){
                    idField=declaredField.getName();
                    break;
                }
            }

            Document annotation = tClass.getAnnotation(Document.class);
            Assert.notNull(annotation,"注解不存在");
            indexName = annotation.indexName();

        }
    }

    /**
     * 得到索引名
     * @return
     */
    public  String getIndexName(){
        if(StringUtils.isBlank(indexName)){
            throw new RuntimeException("找不到索引名");
        }
        return indexName;
    }

    /**
     * 初始化索引
     */
    public void initIndex(){
        createIndex(indexName);
        putMapping(indexName);
    }

    public ElasticsearchRestTemplate getElasticsearchRestTemplate() {
        return elasticsearchRestTemplate;
    }

    /**
     * 创建索引
     * @param indexName
     */
    public void createIndex(String indexName){
       if(!elasticsearchRestTemplate.indexExists(indexName)){
           log.info(" IndexOperate.createIndex  创建索引 {}",indexName);
           elasticsearchRestTemplate.createIndex(indexName);
       }
    }

    /**
     * mapping 映射
     * @param indexName
     */
    public void putMapping(String indexName){
        if(elasticsearchRestTemplate.indexExists(indexName)){
            log.info(" IndexOperate.putMapping 进行mapping  ");
            elasticsearchRestTemplate.putMapping(tClass);
        }
    }

    /**
     * 创建索引
     */
    public void createIndex(){
        if(!elasticsearchRestTemplate.indexExists(tClass)){
            elasticsearchRestTemplate.createIndex(tClass);
        }
    }
    /**
     * 获取类型名称
     * @return
     */
    public String getTypeName() {
        Document annotation = AnnotationUtils.findAnnotation(tClass, Document.class);

        if (annotation == null) {
            throw new RuntimeException("不存在indexName,请检查数据配置");
        } else {
            return annotation.type();
        }
    }

    /**
     * 新增数据
     * @param doc
     * @return 成功标志
     */
    public boolean putData(T doc) {
        elasticsearchRestTemplate.index(new IndexQueryBuilder().withObject(doc).build());
        return true;
    }

    /**
     * 批量新增数据
     * @return 成功标志
     */
    public boolean putData(List<T> list) {
        if (CollectionUtils.isEmpty(list)) {
            return false;
        }
        List<IndexQuery> queries = new ArrayList<>();
        list.forEach(each -> queries.add(new IndexQueryBuilder().withObject(each).build()));
        log.info("索引：{},执行存入数据：{}", getIndexName(), queries.size());
        elasticsearchRestTemplate.bulkIndex(queries);
        return true;
    }




    /**
     *  根据主键id批量获取指定字段的数据
     * @param ids    数据的id字段
     * @param fields  要查出来的es字段
     * @param resultsMapper  对应的映射
     * @return
     */
    public AggregatedPage<T> selectByIds(String[] ids, String[] fields, AbstractResultMapper resultsMapper) {
        NativeSearchQueryBuilder nativeSearchQueryBuilder =
                new NativeSearchQueryBuilder()
                        .withFields(fields)
                        .withQuery(QueryBuilders.termsQuery(idField, ids))
                        .withIndices(getIndexName())
                        .withPageable(PageRequest.of(0,ids.length));

        NativeSearchQuery build = nativeSearchQueryBuilder.build();
        return elasticsearchRestTemplate.queryForPage(build, tClass,resultsMapper);
    }

    /**
     *  根据主键id批量获取指定字段的数据
     * @param indexName    索引名
     * @param ids    数据的id字段
     * @param fields  要查出来的es字段
     * @param resultsMapper  对应的映射
     * @return
     */
    public AggregatedPage<T> selectByIds(String indexName,String[] ids, String[] fields, AbstractResultMapper resultsMapper) {
        NativeSearchQueryBuilder nativeSearchQueryBuilder =
                new NativeSearchQueryBuilder()
                        .withFields(fields)
                        .withQuery(QueryBuilders.termsQuery(idField, ids))
                        .withIndices(getIndexName())
                        .withPageable(PageRequest.of(0,ids.length));

        NativeSearchQuery build = nativeSearchQueryBuilder.build();
        return elasticsearchRestTemplate.queryForPage(build, tClass,resultsMapper);
    }

    /**
     * 不推荐使用 推荐
     * 根据主键id批量获取指定字段的数据
     * @param ids 数据的id字段
     * @return
     */
    public AggregatedPage<T> selectByIds(String[] ids) {
        NativeSearchQueryBuilder nativeSearchQueryBuilder =
                new NativeSearchQueryBuilder()
                        .withQuery(QueryBuilders.termsQuery(idField, ids))
                        .withIndices(getIndexName())
                        .withPageable(PageRequest.of(0,ids.length));

        NativeSearchQuery build = nativeSearchQueryBuilder.build();
        return elasticsearchRestTemplate.queryForPage(build, tClass);
    }


    /**
     * 获取某一页的数据
     * @return
     */
    public List<T> selectList(QueryBuilder queryBuilder, Pageable pageable) {
        NativeSearchQueryBuilder nativeSearchQueryBuilder =
                new NativeSearchQueryBuilder()
                        .withQuery(queryBuilder)
                        .withIndices(getIndexName())
                        .withPageable(pageable);
        SearchQuery searchQuery = nativeSearchQueryBuilder.build();
        String searchQuerySource = EsUtil.getSearchQuerySource(queryBuilder);
        log.info(" IndexOperate.selectList  查询的dsl语句为 {} ",searchQuerySource);

        return elasticsearchRestTemplate.queryForList(searchQuery, tClass);
    }


    /**
     * 获取某一页的数据
     * @return
     */
    public AggregatedPage<T> selectPage(QueryBuilder queryBuilder,Pageable pageable) {
        NativeSearchQueryBuilder nativeSearchQueryBuilder =
                new NativeSearchQueryBuilder()
                        .withQuery(queryBuilder)
                        .withIndices(getIndexName())
                        .withPageable(pageable);
        String searchQuerySource = EsUtil.getSearchQuerySource(queryBuilder);
        log.info(" IndexOperate.selectPage  查询dsl语句为 {} ",searchQuerySource);
        SearchQuery searchQuery = nativeSearchQueryBuilder.build();
        return elasticsearchRestTemplate.queryForPage(searchQuery, tClass);
    }

    /**
     * 获取某一页的数据
     * @return
     */
    public AggregatedPage<T> selectPage(QueryBuilder queryBuilder,String[] fields,Pageable pageable) {
        NativeSearchQueryBuilder nativeSearchQueryBuilder =
                new NativeSearchQueryBuilder()
                        .withQuery(queryBuilder)
                        .withFields(fields)
                        .withIndices(getIndexName())
                        .withPageable(pageable);
        SearchQuery searchQuery = nativeSearchQueryBuilder.build();
        String searchQuerySource = EsUtil.getSearchQuerySource(queryBuilder);
        log.info(" IndexOperate.selectPage 查询dsl语句为 {} ",searchQuerySource);
        return elasticsearchRestTemplate.queryForPage(searchQuery, tClass);
    }


    /**
     * 获取某一页的数据
     *
     * @return
     */
    public AggregatedPage<T> selectAggregationPage(QueryBuilder queryBuilder, AbstractAggregationBuilder aggregationBuilder) {
        NativeSearchQueryBuilder nativeSearchQueryBuilder =
                new NativeSearchQueryBuilder()
                        .withQuery(queryBuilder)
                        .withIndices(getIndexName())
                        .addAggregation(aggregationBuilder);
        SearchQuery searchQuery = nativeSearchQueryBuilder.build();
        String searchQuerySource = EsUtil.getSearchQuerySource(queryBuilder);
        log.info(" IndexOperate.selectAggregationPage  查询dsl语句为 {} ",searchQuerySource);
        return elasticsearchRestTemplate.queryForPage(searchQuery, tClass);
    }


    /**
     * 获取某一页的数据
     * @return
     */
    public List<T> selectNestList(QueryBuilder queryBuilder,String nestPath,Pageable pageable) {
        //嵌套query
        NestedQueryBuilder nestedQueryBuilder =
                QueryBuilders.nestedQuery(nestPath, queryBuilder, ScoreMode.None).innerHit(new InnerHitBuilder());

        NativeSearchQueryBuilder nativeSearchQueryBuilder =
                new NativeSearchQueryBuilder()
                        .withQuery(nestedQueryBuilder)
                        .withIndices(getIndexName())
                        .withPageable(pageable);
        SearchQuery searchQuery = nativeSearchQueryBuilder.build();
        String searchQuerySource = EsUtil.getSearchQuerySource(queryBuilder);
        log.info(" IndexOperate.selectNestList 查询dsl语句为 {} ",searchQuerySource);
        return elasticsearchRestTemplate.queryForList(searchQuery, tClass);
    }


    /**
     * 获取值
     * @param id
     * @param field
     * @return
     */
    public List<Object> getValue(String id,String field) {
        NativeSearchQueryBuilder nativeSearchQueryBuilder =
                new NativeSearchQueryBuilder()
                        .withFields(field)
                        .withQuery(QueryBuilders.termQuery(idField, id))
                        .withIndices(getIndexName());
        NativeSearchQuery build = nativeSearchQueryBuilder.build();

        List query = elasticsearchRestTemplate.query(build, response -> {
            List<String> list = new ArrayList<>();
            SearchHits hits = response.getHits();
            SearchHit[] hits1 = hits.getHits();
            for (SearchHit documentFields : hits1) {
                Map<String, Object> result = documentFields.getSourceAsMap();
                Object o = result.get(field);
                list.add(String.valueOf(o));
            }
            return list;
        });


        return query;
    }

    /**
     * 根据主键id批量获取指定字段的数据
     * @return 对于的结果集
     */
    public List<T> selectByIds(String id) {
        NativeSearchQueryBuilder nativeSearchQueryBuilder =
                new NativeSearchQueryBuilder()
                        .withQuery(QueryBuilders.termQuery(indexName, id))
                        .withIndices();

        NativeSearchQuery build = nativeSearchQueryBuilder.build();
        return elasticsearchRestTemplate.queryForList(build, tClass);
    }


    /**
     * 根据searchQuery查询
     * @param searchQuery
     * @param ResultMapper
     * @return
     */
    public AggregatedPage<T> selectBySearchQuery(SearchQuery searchQuery, AbstractResultMapper ResultMapper) {
        try {
            return elasticsearchRestTemplate.queryForPage(searchQuery, tClass, ResultMapper);
        } catch (Exception e) {
            log.error("查询es报错", e);
        }
        return null;
    }


}
