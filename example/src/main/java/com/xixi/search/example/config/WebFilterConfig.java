package com.xixi.search.example.config;


import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurationSupport;

import java.util.ArrayList;
import java.util.List;


@Configuration
public class WebFilterConfig extends WebMvcConfigurationSupport {



    /**
     * 处理跨浏览器请求
     * @param registry
     */
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedOrigins("*")
                .allowCredentials(false)
                .allowedMethods("GET", "POST", "PUT", "DELETE")
                .allowedHeaders("*");
        super.addCorsMappings(registry);
    }




    @Override
    public void addInterceptors(InterceptorRegistry registry) {

        List<String> jwtExcludePatterns = new ArrayList();
        //swagger
        jwtExcludePatterns.add("/webjars/**");
        jwtExcludePatterns.add("/swagger/**");
        jwtExcludePatterns.add("/v3/**");
        jwtExcludePatterns.add("/swagger-ui/**");
        jwtExcludePatterns.add("/swagger-resources/**");
        jwtExcludePatterns.add("/swagger-resources/**");
    }

    /**
     * 发现如果继承了WebMvcConfigurationSupport，则在yml中配置的相关内容会失效。 需要重新指定静态资源
     *
     * @param registry
     */
    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.
                addResourceHandler( ""+"/swagger-ui/**")
                .addResourceLocations("classpath:/META-INF/resources/webjars/springfox-swagger-ui/")
                .resourceChain(false);
    }
}
