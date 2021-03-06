package com.xixi.search.inquire.query.search;

import com.google.common.collect.Lists;
import com.xixi.search.common.dto.BoolQueryDTO;
import com.xixi.search.common.dto.SearchTreeDTO;
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
public abstract class AbstractExpressionSearchQueryBuilder<T> extends IndexOperate<T> implements BaseSearch {

    @Autowired 
    protected ElasticsearchRestTemplate elasticsearchRestTemplate;

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
     * ?????? ??????????????????
     * @param params
     * @param ResultMapper
     * @return
     */
    PagingHelper<T> selectPage(ExtentSearchPageParam params, AbstractResultMapper ResultMapper){
        BaseQueryParam param = buildQueryParam(params);
        param.setPageable(PageRequest.of(params.getPageNum()-1,params.getPageNum()));

        //??????????????????
        HandleRegistry handleRegistry = addSearchHandler(params.getExtendMap());
        AggregatedPage<T> pageResult = selectPage(param, ResultMapper, handleRegistry);
        PagingHelper myPage = new PagingHelper<>();
        myPage.fromEs(pageResult);
        buildOtherResult(pageResult,myPage);
        return myPage;
    }

    /**
     * ????????????
     * @param params   ??????
     * @return
     */
    List<T> searchQuery(ExtentSearchPageParam params){
        BaseQueryParam param = buildQueryParam(params);
        HandleRegistry handleRegistry = addSearchHandler(params.getExtendMap());
        return searchQuery(param, handleRegistry);
    }


    protected  List<T> searchQuery(BaseQueryParam param, HandleRegistry handleRegistry){
        SearchQuery searchQuery = buildSearchQuery(param, handleRegistry);
        return elasticsearchRestTemplate.queryForList(searchQuery,gettClass());
    }


    /**
     * ?????????????????????
     * @param pageResult es??????
     * @param myPage ????????????
     */
    protected abstract void buildOtherResult(AggregatedPage<T> pageResult, PagingHelper myPage);


    /**
     * ??????????????????????????? ????????????????????????
     * @param extendMap
     * @return
     */
    protected abstract HandleRegistry addSearchHandler(Map extendMap);

    /**
     * ????????????
     * @param params
     * @return
     */
    protected BaseQueryParam buildQueryParam(ExtentSearchPageParam params){
        String[] fields =  params.getFields()!=null && params.getFields().length>0 ?params.getFields() : getFields();
        return buildQueryParam(fields,getIndexName(),params.getSearchExpression());
    }

    /**
     * ????????????
     * @param fields  ?????????????????????
     * @param indexName  ????????????
     * @param searchExpression  ???????????????
     * @return
     */
    protected BaseQueryParam buildQueryParam(String[] fields,String indexName,String searchExpression){
        BaseQueryParam param = BaseQueryParam.builder()
                .indexName(indexName)
                .fields(fields)
                .baseEsQueryParams(searchExpression)
                .build();
        return param;
    }


    /**
     * ?????????????????????
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
     * ??????????????????????????????
     * @param params   ????????????
     * @return
     */
    public PagingHelper<T> selectPage(ExtentSearchPageParam params){
        return selectPage(params,new EsDataResultMapper());
    }

    /**
     * ????????????
     * @param baseQueryParam   ????????????
     * @param ResultMapper    ???????????????
     * @param handleRegistry ????????????
     * @return
     */
    private AggregatedPage<T> selectPage(BaseQueryParam baseQueryParam, AbstractResultMapper ResultMapper, HandleRegistry handleRegistry) {
        SearchQuery searchQuery = buildSearchQuery(baseQueryParam, handleRegistry);
        try {
            return elasticsearchRestTemplate.queryForPage(searchQuery, gettClass(), ResultMapper);
        } catch (Exception e) {
            log.error("??????es??????", e);
        }
        return null;
    }

    /**
     * ??????searchQuery??????
     * @param baseQueryParam  ????????????
     * @param handleRegistry  ???????????????
     * @return
     */
    protected  SearchQuery buildSearchQuery(BaseQueryParam baseQueryParam,HandleRegistry handleRegistry){
        //???????????????????????????
        List<EsHandleParam> esHandleParams = Optional.ofNullable(handleRegistry.getHandlerList()).orElse(Lists.newArrayList());
        esHandleParams.forEach(each->each.getHandle().execute(this,each.getParam(),baseQueryParam));

        NativeSearchQueryBuilder searchQueryBuilder = getSearchQueryBuilder(baseQueryParam);
        //????????????
        return searchQueryBuilder.build();
    }


    /**
     * ??????????????????DSL??????
     * @param baseQueryParam   ????????????
     * @return
     */
    @Override
    public NativeSearchQueryBuilder getSearchQueryBuilder(BaseQueryParam baseQueryParam) {
        BoolQueryBuilder finalQueryBuilder = QueryBuilders.boolQuery();
        /**
         * ??????DSL????????????
         */
        if(baseQueryParam.getSearchTreeDTO()!=null){
            BoolQueryBuilder queryBuilder = Optional.ofNullable(buildTreeQuery(baseQueryParam.getSearchTreeDTO())).orElse(QueryBuilders.boolQuery());
            finalQueryBuilder.filter(queryBuilder);
        }

        /**
         * ????????????DSL????????????
         */
        if (baseQueryParam.getBaseEsQueryParams() != null) {
            BoolQueryBuilder queryBuilder = Optional.ofNullable(buildExpressionQuery(baseQueryParam.getBaseEsQueryParams())).orElse(QueryBuilders.boolQuery());
            finalQueryBuilder.filter(queryBuilder);
        }

        /**
         * ??????????????????
         */
        if (baseQueryParam.getOtherQuery() != null) {
            BoolQueryBuilder otherBoolQueryBuilder = QueryBuilders.boolQuery();
            baseQueryParam.getOtherQuery().forEach(each -> otherBoolQueryBuilder.must(each));
            finalQueryBuilder.filter(otherBoolQueryBuilder);
        }

        /**
         * ?????????????????????????????????
         */
        if (!CollectionUtils.isEmpty(baseQueryParam.getExcludeQuery())) {
            for (QueryBuilder queryBuilder : baseQueryParam.getExcludeQuery()) {
                finalQueryBuilder.mustNot(queryBuilder);
            }
        }

        /**
         * ?????????id????????????
         */
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

        //??????????????????
        baseQueryParam.buildSearchQuery(nativeSearchQueryBuilder);

        if (!CollectionUtils.isEmpty(baseQueryParam.getHighLightFields())) {
            nativeSearchQueryBuilder.withHighlightFields(baseQueryParam.getHighLightFields().stream().toArray(HighlightBuilder.Field[]::new));
        }

        return nativeSearchQueryBuilder;

    }

    protected abstract BoolQueryBuilder buildTreeQuery(SearchTreeDTO searchTreeDTO);

    /**
     * ??????????????????????????????????????? ?????????????????????
     * @param baseEsQueryParams ????????????
     * @return
     */
    protected  BoolQueryBuilder buildExpressionQuery(String baseEsQueryParams){
        NestExpressionParse instance = getParseInstance();
        //???????????????
        List<String> expressionList = instance.nestParseTree(baseEsQueryParams);

        // ?????????????????????????????????
        Map<String, String> fieldMap = getFieldMap();
        BoolQueryDTO boolQueryDTO = new BoolQueryDTO();
        boolQueryDTO.setFieldMap(fieldMap);
        Field[] declaredFields = getDeclaredFields();
        boolQueryDTO.setDeclaredFields(declaredFields);
        //???????????????????????? ???????????????????????????
        ParenthesesPackageBoolQueryExecute executeInstance = getExecuteInstance();
        for (String s : expressionList) {
            executeInstance.packageQuery(s,boolQueryDTO);
        }
        BoolQueryBuilder boolQueryBuilder = boolQueryDTO.getBoolQueryBuilder();
        return  boolQueryBuilder;
    }


    /**
     * ??????indexName
     * @return
     */
    @Override
    public String getIndexName() {
        return super.getIndexName();
    }
}
