package com.xixi.search.inquire.query.search.execute;

import com.xixi.search.common.dto.BoolQueryDTO;
import com.xixi.search.common.enums.RelateEnum;
import com.xixi.search.common.util.BeanUtils;
import com.xixi.search.inquire.transform.parse.NestExpressionParse;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/23
 */
public class ParenthesesPackageBoolQueryExecute extends AbstractPackageBoolQueryExecute {

    private static ExpressionPackageBoolQueryExecute instance;



    public static synchronized ExpressionPackageBoolQueryExecute getInstance() {
        if (instance == null) {
            instance = new ExpressionPackageBoolQueryExecute();
        }
        return instance;
    }


    public void setNextExecute() {
        super.setNextExecute(ParenthesesPackageBoolQueryExecute.getInstance());
    }

    @Override
    public void packageQuery(String expression, BoolQueryDTO boolQueryDTO) {
        // 先去判断你的 expression 是否包含 || 或者 &&
        if(StringUtils.isBlank(expression)){
            return;
        }
        setNextExecute();
        if(StringUtils.isNotBlank(expression) && expression.length()==2){
            this.getNextExecute().packageQuery(expression,boolQueryDTO);
            return;
        }
        if(StringUtils.contains(expression, RelateEnum.OR.getCode()) || StringUtils.contains(expression, RelateEnum.AND.getCode())){
            NestExpressionParse parseInstance = getParseInstance();
            List<String> list = parseInstance.nestParseTree(expression);
            BoolQueryDTO helper = BeanUtils.copyProperties(boolQueryDTO, BoolQueryDTO.class);
            helper.setQueryBuilder(null);
            helper.setBoolQueryBuilder(null);
            for (String s : list) {
                this.packageQuery(s,helper);
            }
            boolQueryDTO.setBoolQueryBuilder(helper.getBoolQueryBuilder());
        }else{
            this.getNextExecute().packageQuery(expression,boolQueryDTO);
        }

    }
}
