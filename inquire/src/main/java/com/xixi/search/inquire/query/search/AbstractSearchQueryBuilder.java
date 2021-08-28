package com.xixi.search.inquire.query.search;

import com.google.common.collect.Lists;
import com.xixi.search.common.dto.BoolQueryDTO;
import com.xixi.search.common.index.IndexOperate;
import com.xixi.search.common.param.BaseQueryParam;
import com.xixi.search.common.param.ExtentSearchPageParam;
import com.xixi.search.common.util.EsUtil;
import com.xixi.search.common.util.PagingHelper;
import com.xixi.search.inquire.query.resultMap.EsDataResultMapper;
import com.xixi.search.inquire.query.search.base.BaseSearch;
import com.xixi.search.inquire.query.search.execute.ParenthesesPackageBoolQueryExecute;
import com.xixi.search.inquire.query.search.handle.EsHandleParam;
import com.xixi.search.inquire.query.search.handle.HandleRegistry;
import com.xixi.search.inquire.transform.parse.NestExpressionParse;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.fetch.subphase.highlight.HighlightBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.AbstractResultMapper;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.aggregation.AggregatedPage;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.query.SearchQuery;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.Optional;


/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
@Slf4j
@Component
public abstract class AbstractSearchQueryBuilder<T> extends IndexOperate<T> implements BaseSearch {

    @Autowired 
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    private static NestExpressionParse parseInstance;

    public static synchronized NestExpressionParse getParseInstance() {
        if (parseInstance == null) {
            parseInstance = new NestExpressionParse();
        }
        return parseInstance;
    }

    private static ParenthesesPackageBoolQueryExecute executeInstance;

    public static synchronized ParenthesesPackageBoolQueryExecute getExecuteInstance() {
        if (executeInstance == null) {
            executeInstance = new ParenthesesPackageBoolQueryExecute();
        }
        return executeInstance;
    }

    
    
    /**
     * 分页 默认使用这个
     * @param params
     * @param ResultMapper
     * @return
     */
    PagingHelper<T> selectPage(ExtentSearchPageParam params, AbstractResultMapper ResultMapper){
        String[] fields =  params.getFields()!=null && params.getFields().length>0 ?params.getFields() : getFields();
        BaseQueryParam param = BaseQueryParam.builder()
                .indexName(getIndexName())
                .fields(fields)
                .baseEsQueryParams(params.getSearchExpression())
                .pageable(PageRequest.of(Optional.ofNullable(params.getPageNum()).orElse(1) - 1, Optional.ofNullable(params.getPageCount()).orElse(10)))
                .build();
        HandleRegistry handleRegistry = addSearchHandler(params.getExtendMap());
        AggregatedPage<T> pageResult = selectPage(param, ResultMapper, handleRegistry);
        PagingHelper myPage = new PagingHelper<>();
        myPage.fromEs(pageResult);
        buildOtherResult(pageResult,myPage);
        return myPage;
    }

    /**
     * 添加额外的结果
     * @param pageResult es结果
     * @param myPage 返回结果
     */
    protected abstract void buildOtherResult(AggregatedPage<T> pageResult, PagingHelper myPage);


    /**
     * 添加检索拦截处理类 最基本的就是高亮
     * @param extendMap
     * @return
     */
    protected abstract HandleRegistry addSearchHandler(Map extendMap);


    /**
     * 得到所有的结果
     * @return
     */
    protected  String[] getFields(){
        Field[] declaredFields = this.getDeclaredFields();
        String[] fields = new String[declaredFields.length];
        for (int i = 0; i < declaredFields.length; i++) {
            String name = declaredFields[i].getName();
            fields[i]=name;
        }
        return fields;
    }


    /**
     * 采用默认的对象转换类
     * @param params
     * @return
     */
    public PagingHelper<T> selectPage(ExtentSearchPageParam params){
        return selectPage(params,new EsDataResultMapper());
    }

    /**
     * 检索分页
     * @param baseQueryParam
     * @param ResultMapper
     * @param handleRegistry 处理中心
     * @return
     */
    private AggregatedPage<T> selectPage(BaseQueryParam baseQueryParam, AbstractResultMapper ResultMapper, HandleRegistry handleRegistry) {
        //构建其他类型的条件
        List<EsHandleParam> esHandleParams = Optional.ofNullable(handleRegistry.getHandlerList()).orElse(Lists.newArrayList());
        esHandleParams.forEach(each->each.getHandle().execute(this,each.getParam(),baseQueryParam));

        NativeSearchQueryBuilder searchQueryBuilder = getSearchQueryBuilder(baseQueryParam);
        //获取查询
        SearchQuery searchQuery = searchQueryBuilder.build();
        try {
            return elasticsearchRestTemplate.queryForPage(searchQuery, gettClass(), ResultMapper);
        } catch (Exception e) {
            log.error("查询es报错", e);
        }
        return null;
    }


    /**
     * 构建外层检索DSL语句
     * @param baseQueryParam
     * @return
     */
    @Override
    public NativeSearchQueryBuilder getSearchQueryBuilder(BaseQueryParam baseQueryParam) {
        BoolQueryBuilder finalQueryBuilder = QueryBuilders.boolQuery();


        if (baseQueryParam.getBaseEsQueryParams() != null) {
            BoolQueryBuilder queryBuilder = Optional.ofNullable(buildFinalQuery(baseQueryParam.getBaseEsQueryParams())).orElse(QueryBuilders.boolQuery());
            finalQueryBuilder.filter(queryBuilder);
        }


        // 其他语句
        if (baseQueryParam.getOtherQuery() != null) {
            BoolQueryBuilder otherBoolQueryBuilder = QueryBuilders.boolQuery();
            baseQueryParam.getOtherQuery().forEach(each -> otherBoolQueryBuilder.must(each));
            finalQueryBuilder.filter(otherBoolQueryBuilder);
        }

        // 需要排除的语句
        if (!CollectionUtils.isEmpty(baseQueryParam.getExcludeQuery())) {
            for (QueryBuilder queryBuilder : baseQueryParam.getExcludeQuery()) {
                finalQueryBuilder.mustNot(queryBuilder);
            }
        }

        // 额外添加额id集合
        if (baseQueryParam.getAppendIdList() != null) {
            BoolQueryBuilder finalQueryBuilder2 = QueryBuilders.boolQuery();
            finalQueryBuilder2.should(finalQueryBuilder);
            finalQueryBuilder2.should(QueryBuilders.termsQuery(getIdField(), baseQueryParam.getAppendIdList()));
            finalQueryBuilder = finalQueryBuilder2;
        }



        log.info(EsUtil.getSearchQuerySource(finalQueryBuilder));

        NativeSearchQueryBuilder nativeSearchQueryBuilder =
                new NativeSearchQueryBuilder()
                        .withFields(baseQueryParam.getFields())
                        .withQuery(finalQueryBuilder)
                        .withIndices(baseQueryParam.getIndexName())
                        .withPageable(baseQueryParam.getPageable());

        //封装其他参数
        baseQueryParam.buildSearchQuery(nativeSearchQueryBuilder);

        if (!CollectionUtils.isEmpty(baseQueryParam.getHighLightFields())) {
            nativeSearchQueryBuilder.withHighlightFields(baseQueryParam.getHighLightFields().stream().toArray(HighlightBuilder.Field[]::new));
        }

        return nativeSearchQueryBuilder;

    }

    /**
     * 将表达式进行转换成树的格式 继续解析后处理
     * @param baseEsQueryParams
     * @return
     */
    protected  BoolQueryBuilder buildFinalQuery(String baseEsQueryParams){
        NestExpressionParse instance = getParseInstance();
        //解析表达式
        List<String> expressionList = instance.nestParseTree(baseEsQueryParams);

        // 得到所有字段的类型关系
        Map<String, String> fieldMap = getFieldMap();
        BoolQueryDTO boolQueryDTO = new BoolQueryDTO();
        boolQueryDTO.setFieldMap(fieldMap);
        Field[] declaredFields = getDeclaredFields();
        boolQueryDTO.setDeclaredFields(declaredFields);
        //根据对应字段名称 得到字段对应的类型
        ParenthesesPackageBoolQueryExecute executeInstance = getExecuteInstance();
        for (String s : expressionList) {
            executeInstance.packageQuery(s,boolQueryDTO);
        }
        BoolQueryBuilder boolQueryBuilder = boolQueryDTO.getBoolQueryBuilder();
        return  boolQueryBuilder;
    }


    /**
     * 得到indexName
     * @return
     */
    @Override
    public String getIndexName() {
        return super.getIndexName();
    }
}
