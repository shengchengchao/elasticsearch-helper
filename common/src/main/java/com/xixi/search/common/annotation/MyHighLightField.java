package com.xixi.search.common.annotation;

import java.lang.annotation.*;

/**
 * 表示可以用于高亮显示
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@Documented
@Inherited
public @interface MyHighLightField {
    String value() default "";
}
