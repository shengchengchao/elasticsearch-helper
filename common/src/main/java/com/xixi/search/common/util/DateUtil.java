package com.xixi.search.common.util;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.apache.commons.lang3.time.DateUtils;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/27
 */
@Slf4j
public class DateUtil {
    /**
     * 时间格式的长度
     */
    public static final int DATE_LENGTH = 19;
    /**
     * 1000
     */
    public static final long MILLIS_PER_SECOND = 1000;
    /**
     * 60*1000
     */
    public static final long MILLIS_PER_MINUTE = 60 * MILLIS_PER_SECOND;
    /**
     * 60*60*1000
     */
    public static final long MILLIS_PER_HOUR = 60 * MILLIS_PER_MINUTE;
    /**
     * 24*60*60*1000
     */
    public static final long MILLIS_PER_DAY = 24 * MILLIS_PER_HOUR;

    /**
     * yyyyMMdd
     */
    public static final String YEARMONTHDAY_FORMAT = "yyyyMMdd";

    /**
     * yyyyMM
     */
    public static final String YEARMONTH_FORMAT = "yyyyMM";
    /**
     * yyyy-MM-dd
     */
    public static final String DATE_FORMAT_PATTERN = "yyyy-MM-dd";
    /**
     * yyyy年MM月dd日
     */
    public static final String DATE_FORMAT_PATTERN_CN = "yyyy年MM月dd日";
    /**
     * yyyy-MM-dd HH:mm:ss
     */
    public static final String DATETIME_FORMAT_PATTERN = "yyyy-MM-dd HH:mm:ss";
    /**
     * yyyy-MM-dd HH:mm:ss
     */
    public static final String DATETIME_OTHER_FORMAT_PATTERN = "yyyy年MM月dd日 HH:mm:ss";
    /**
     * yyyy/MM/dd HH:mm:ss
     */
    public static final String APP_DATE_FORMAT_PATTERN = "yyyy/MM/dd HH:mm:ss";
    /**
     * yyyy/MM/dd HH:mm
     */
    public static final String APP_MM_DATE_FORMAT_PATTERN = "yyyy/MM/dd HH:mm";
    /**
     * yyyy-MM-dd HH:mm:ss.SSS
     */
    public static final String DATETIME_SSS_FORMAT_PATTERN = "yyyy-MM-dd HH:mm:ss.SSS";
    /**
     * HH:mm:ss
     */
    public static final String TIME_FORMAT_PATTERN = "HH:mm:ss";
    /**
     * yyyyMMddHHmmss
     */
    public static final String COMPACT_DATETIME_FORMAT_PATTERN = "yyyyMMddHHmmss";
    /**
     * 30
     */
    public static final Integer MONTH_DAYS = 30;
    /**
     * 31
     */
    public static final Integer LONGEST_MONTH_DAYS = 31;
    /**
     * 365
     */
    public static final Integer YEAR_DAYS = 365;
    /**
     * 12
     */
    public static final Integer YEAR_MONTHS = 12;
    /**
     * 10
     */
    public static final Integer MID_TIME_DECIMAL_DIGIT = 10;

    public static final String MONTH_START = "START";

    public static final String MONTH_END = "END";

    /**
     * 降序
     */
    public static final String DESC = "DESC";

    /**
     * 升序
     */
    public static final String ASC = "ASC";


    // 0
    public static final long var0 = 0;
    // 0-30分钟 刚刚上传
    public static final long var1 = 30 * 60 * 1000;
    // 一小时前更新
    public static final long var2 = 60 * 60 * 1000;
    // 24小时前更新
    public static final long var3 = 24 * 60 * 60 * 1000;
    // 24-48昨天更新
    public static final long var4 = 2 * 24 * 60 * 60 * 1000;

    /**
     * 时间戳转化为string时间
     * unixTimeToString
     *
     * @param time
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-24 14:03
     */
    public static final String unixTimeToString(Object time) {
        if (time != null && !"0".equals(String.valueOf(time))) {
            String res = null;
            SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            String str_time = time.toString();
            long long_time = Long.parseLong(str_time);
            if (str_time.length() > 10) {
                long lt = new Long(long_time);
                Date date = new Date(lt);
                res = simpleDateFormat.format(date);
                return res;
            } else {
                long lt = new Long(long_time * 1000);
                Date date = new Date(lt);
                res = simpleDateFormat.format(date);
                return res;
            }
        } else {
            return null;
        }
    }

    /**
     * 取下一年的时间戳
     *
     * @param time
     * @return
     */
    public static Long getNextYear(Long time) {
        Calendar calendar = Calendar.getInstance();
        calendar.clear();
        calendar.setTimeInMillis(time);
        calendar.add(Calendar.YEAR, 1);
        return calendar.getTimeInMillis();
    }


    /**
     * 日期调整 默认为天
     * dateAdjust
     *
     * @param date 日期格式
     * @param adj  日期调整 正数 往后，负数往前
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-24 14:03
     */
    public static final String dateAdjust(String date, int adj) {
        return dateAdjust(date, adj, Calendar.DAY_OF_YEAR);
    }


    /**
     * 日期调整
     * dateAdjust
     *
     * @param date     日期格式
     * @param adj      日期调整 正数 往后，负数往前
     * @param timeUnit 为时间单位 天、周、月、年
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-24 14:03
     */
    public static final String dateAdjust(String date, int adj, int timeUnit) {
        if (StringUtils.isBlank(date)) {
            return StringUtils.EMPTY;
        }
        Calendar cal = Calendar.getInstance();
        cal.setTime(DateUtil.parseDay(date));
        cal.add(timeUnit, adj);
        return DateUtil.format(cal.getTime());
    }


    /**
     * 解析字符串成java.util.Date 使用yyyy-MM-dd
     * parseDay
     *
     * @param str 日期
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-24 14:03
     */
    public static Date parseDay(String str) {
        String pattern = DATE_FORMAT_PATTERN;
        if (str != null && str.length() >= 21) {
            pattern = DATETIME_SSS_FORMAT_PATTERN;
        } else if (str != null && str.length() == 19) {
            pattern = DATETIME_FORMAT_PATTERN;
        }
        return parse(str, pattern);
    }

    /**
     * 解析字符串成java.util.Date，保留时间精度
     * parse
     *
     * @param str 日期
     * @param str 日期格式
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 14:03
     */
    public static Date parse(String str, String pattern) {
        if (StringUtils.isBlank(str)) {
            return null;
        }
        try {
            return DateUtils.parseDate(str, pattern);
        } catch (ParseException e) {
            throw new IllegalArgumentException("Can't parse " + str + " using " + pattern);
        }
    }


    /**
     * 时间endDate的当月第N天（当第N天大于endDate,默认时间为endDate）
     * juestEndDate
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date juestEndDate(Date endDate, Integer endDay) {
        Calendar cal = Calendar.getInstance();
        cal.setTime(endDate);
        cal.set(Calendar.DAY_OF_MONTH, endDay);
        if (cal.getTime().before(endDate)) {
            return cal.getTime();
        } else {
            return endDate;
        }
    }

    /**
     * 根据时间变量返回时间字符串
     * format
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static String format(Date date, String pattern) {
        if (date == null || StringUtils.isBlank(pattern)) {
            return "";
        }
        return DateFormatUtils.format(date, pattern);
    }


    /**
     * 根据时间变量返回时间
     * format
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date format(String date, String format) {
        SimpleDateFormat sdf;
        try {
            sdf = new SimpleDateFormat(format);
            return sdf.parse(date);
        } catch (Exception e) {
            log.info("format", e);
        }
        return null;
    }

    /**
     * 按<code>yyyy-MM-dd</code>格式化日期成字符串
     * format
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static String format(Date date) {
        return format(date, DATE_FORMAT_PATTERN);
    }

    /**
     * 取当前系统时间【HH:mm:ss】
     * getCurrentTimeAsString
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static String getCurrentTimeAsString() {
        return format(new Date(), TIME_FORMAT_PATTERN);
    }


    /**
     * 取当前系统时间【HH:mm:ss】
     * getCurrentTimeAsString
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static String getCurrentTimeAsString(String pattern) {
        return format(new Date(), pattern);
    }


    /**
     * 返回当前日期，按<code>yyyyMMddHHmmss</code>格式化日期成字符串
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static String getCurrentCompactDateTimeAsString() {
        return format(new Date(), COMPACT_DATETIME_FORMAT_PATTERN);
    }

    /**
     * 返回当前日期，按<code>yyyyMMddHHmmss</code>格式化日期成字符串
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Long getCurrentCompactDateTime() {
        return System.currentTimeMillis();
    }


    /**
     * 返回当前日期，按<code>yyyy-MM-dd</code>格式化日期成字符串
     * getCurrentDateAsString
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static String getCurrentDateAsString() {
        return format(new Date(), DATE_FORMAT_PATTERN);
    }

    /**
     * 返回按照当前日期，按<code>pattern</code>格式化的字符串
     * getCurrentDateAsString
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static String getCurrentDateAsString(String pattern) {
        return format(new Date(), pattern);
    }


    /**
     * 返回当前日期，按 <code>yyyy-MM-dd HH:mm:ss</code>格式化成字符串
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static String getCurrentDateTimeAsString() {
        return format(new Date(), DATETIME_FORMAT_PATTERN);
    }

    /**
     * 将日期调整后返回调整日期，按 <code>yyyy-MM-dd HH:mm:ss</code>格式化成字符串
     *
     * @param field 日期部分
     * @param delta 偏移值
     * @return 调整日期
     * @return: String
     * @author: huajiejun
     * @version 创建时间：2017年6月21日 下午5:42:32
     */
    public static String getCurrentDateTimeAsString(int field, int delta) {
        Calendar cal = Calendar.getInstance();
        cal.add(field, delta);
        Date date = cal.getTime();
        return format(date, DATETIME_FORMAT_PATTERN);
    }

    /**
     * 返回当前日期所在月份的第一天
     * getCurrentDateAsString
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date getStartDateTimeOfCurrentMonth() {
        return getStartDateTimeOfMonth(new Date());
    }


    /**
     * 返回指定日期所在月份的第一天 The value of，日期为空抛出异常NullPointerException
     * getStartDateTimeOfMonth
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date getStartDateTimeOfMonth(Date date) {
        if (null == date) {
            throw new NullPointerException("指定日期不能为空");
        }
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        cal.set(Calendar.DAY_OF_MONTH, 1);
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        return cal.getTime();
    }

    /**
     * 返回当前日期所在月份的最后一天
     * getEndDateTimeOfCurrentMonth
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date getEndDateTimeOfCurrentMonth() {
        return getEndDateTimeOfMonth(new Date());
    }

    /**
     * 返回指定日期所在月份的最后一天,日期为空抛出异常NullPointerException
     * getEndDateTimeOfMonth
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date getEndDateTimeOfMonth(Date date) {
        if (null == date) {
            throw new NullPointerException("指定日期不能为空");
        }
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        cal.set(Calendar.DAY_OF_MONTH, cal.getActualMaximum(Calendar.DAY_OF_MONTH));
        cal.set(Calendar.HOUR_OF_DAY, 23);
        cal.set(Calendar.MINUTE, 59);
        cal.set(Calendar.SECOND, 59);
        cal.set(Calendar.MILLISECOND, 999);
        return cal.getTime();
    }

    /**
     * 返回当天的凌晨时间
     * getStartTimeOfCurrentDate
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date getStartTimeOfCurrentDate() {
        return getStartTimeOfDate(new Date());
    }

    /**
     * 返回指定日期的凌晨时间,日期为空抛出异常NullPointerException
     * getStartTimeOfDate
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date getStartTimeOfDate(Date date) {
        if (null == date) {
            throw new NullPointerException("指定日期不能为空");
        }
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        return cal.getTime();
    }

    /**
     * 返回指定日期的凌晨时间,日期为空抛出异常NullPointerException
     * getStartTimeOfTime
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Long getStartTimeOfTime(Long time) {
        if (null == time) {
            throw new NullPointerException("指定日期不能为空");
        }
        Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis(time);
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        return cal.getTimeInMillis();
    }

    /**
     * 返回当天的结束时间  2005-12-27 23:59:59
     * getEndTimeOfCurrentDate
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date getEndTimeOfCurrentDate() {
        return getEndTimeOfDate(new Date());
    }


    /**
     * 返回指定日期的结束时间,日期为空抛出异常NullPointerException
     * getEndTimeOfCurrentDate
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date getEndTimeOfDate(Date date) {
        if (null == date) {
            throw new NullPointerException("指定日期不能为空");
        }
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        cal.set(Calendar.HOUR_OF_DAY, 23);
        cal.set(Calendar.MINUTE, 59);
        cal.set(Calendar.SECOND, 59);
        cal.set(Calendar.MILLISECOND, 999);
        return cal.getTime();
    }

    /**
     * 返回指定日期的结束时间,日期为空抛出异常NullPointerException
     * getEndTimeOfCurrentDate
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Long getEndTimeOfTime(Long time) {
        if (null == time) {
            throw new NullPointerException("指定日期不能为空");
        }
        Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis(time);
        cal.set(Calendar.HOUR_OF_DAY, 23);
        cal.set(Calendar.MINUTE, 59);
        cal.set(Calendar.SECOND, 59);
        cal.set(Calendar.MILLISECOND, 999);
        return cal.getTimeInMillis();
    }

    /**
     * 计算两个日期之间的天数
     * daysBetween
     *
     * @return: int
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static final int daysBetween(Date early, Date late) {
        Calendar ecal = Calendar.getInstance();
        Calendar lcal = Calendar.getInstance();
        ecal.setTime(early);
        lcal.setTime(late);

        long etime = ecal.getTimeInMillis();
        long ltime = lcal.getTimeInMillis();

        return (int) ((ltime - etime) / MILLIS_PER_DAY);
    }

    /**
     * 计算两个日期之间的分钟数
     * minuteBetween
     *
     * @return: int
     * @author: shengchengchao
     * @version 创建时间：2020-04-22
     */
    public static final int minuteBetween(Date early, Date late) {
        Calendar ecal = Calendar.getInstance();
        Calendar lcal = Calendar.getInstance();
        ecal.setTime(early);
        lcal.setTime(late);

        long etime = ecal.getTimeInMillis();
        long ltime = lcal.getTimeInMillis();

        return (int) ((ltime - etime) / MILLIS_PER_MINUTE);
    }

    /**
     * 计算两个日期之间的分钟数
     * minuteBetween
     *
     * @return: int
     * @author: shengcehngchao
     * @version 创建时间：2020-04-22
     */
    public static final int minuteBetween(String earlydate, String latedate) {
        Date dateEarly = format(earlydate, DATETIME_FORMAT_PATTERN);
        Date dateLate = format(latedate, DATETIME_FORMAT_PATTERN);
        return minuteBetween(dateEarly, dateLate);
    }

    /**
     * 计算两个日期之间的分钟数
     * daysBetween
     *
     * @return: int
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static final int daysBetween(String earlydate, String latedate) {
        Date dateEarly = parseDay(earlydate);
        Date dateLate = parseDay(latedate);
        return daysBetween(dateEarly, dateLate);
    }

    /**
     * yyyy-MM-dd日期转化yyyyMMdd格式日期
     * toDate8Char
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static final String toDate8Char(String yyyy_mm_dd) {
        return StringUtils.replace(yyyy_mm_dd, "-", "");
    }


    /**
     * yyyymmdd日期转化yyyy-MM-dd格式日期
     * toDate10Char
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static final String toDate10Char(String yyyymmdd) {
        if (yyyymmdd == null || yyyymmdd.length() < 8) {
            return yyyymmdd;
        } else {
            return yyyymmdd.substring(0, 4) + "-" + yyyymmdd.substring(4, 6) + "-" + yyyymmdd.substring(6, 8);
        }
    }


    /**
     * 在指定时间基础上增加 指定小时,日期为空抛出异常NullPointerException
     * addHours
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date addHours(Date date, int hours) {
        return add(date, Calendar.HOUR_OF_DAY, hours);
    }

    /**
     * 在指定时间基础上增加 指定分钟,日期为空抛出异常NullPointerException
     * addMinutes
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date addMinutes(Date date, int minutes) {
        return add(date, Calendar.MINUTE, minutes);
    }


    /**
     * 在指定时间基础上增加 指定天数,日期为空抛出异常NullPointerException
     * addDays
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date addDays(Date date, int days) {
        return add(date, Calendar.DATE, days);
    }

    /**
     * 在指定时间基础上增加 指定月份,日期为空抛出异常NullPointerException
     * addMonths
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date addMonths(Date date, int months) {
        return add(date, Calendar.MONTH, months);
    }

    /**
     * 在指定时间基础上增加 指定年份,日期为空抛出异常NullPointerException
     * addYears
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Date addYears(Date date, int years) {
        return add(date, Calendar.YEAR, years);
    }

    /**
     * 时间相加减,日期为空抛出异常NullPointerException
     * add
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    private static Date add(Date date, int field, int amount) {
        if (null == date) {
            throw new NullPointerException("指定日期不能为空");
        }
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        cal.add(field, amount);
        return cal.getTime();
    }

    /**
     * 时间相加减,日期为空抛出异常NullPointerException
     * add
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static Long addDay(Long time, int day) {
        if (null == time) {
            throw new NullPointerException("指定日期不能为空");
        }
        Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis(time);
        cal.add(Calendar.DAY_OF_MONTH, day);
        return cal.getTimeInMillis();
    }

    /**
     * 返回年份
     * getNowYear
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static int getNowYear() {
        Calendar c = Calendar.getInstance();
        return c.get(Calendar.YEAR);
    }

    /**
     * 返回月份
     * getNowMonth
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static int getNowMonth() {
        Calendar c = Calendar.getInstance();
        return c.get(Calendar.MONTH) + 1;
    }

    /**
     * 返回指定时间第N天，日期为空抛出异常NullPointerException
     *
     * @param now 指定日期
     * @return
     * @return: int
     * @author: huajiejun
     * @version 创建时间：2017年6月21日 下午6:33:33
     */
    public static int getDaysOfMonth(Date now) {
        if (null == now) {
            throw new NullPointerException("指定日期不能为空");
        }
        Calendar calender = Calendar.getInstance();
        calender.setTime(now);
        return calender.get(Calendar.DAY_OF_MONTH);
    }

    /**
     * 返回当前时间第N天
     * getNowDayOfMonth
     *
     * @return: int
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static int getNowDayOfMonth() {
        return getDaysOfMonth(new Date());
    }

    /**
     * 返回当前时间第N小时
     * getNowHourOfDay
     *
     * @return: int
     * @author: huasheng
     * @version 创建时间：2017-05-25 17:41
     */
    public static int getNowHourOfDay() {
        Calendar c = Calendar.getInstance();
        return c.get(Calendar.HOUR_OF_DAY);
    }

    /**
     * 日期转换成字符串
     * dateToString
     *
     * @return: String
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static String dateToString(Date date, String format) {
        String ret = "";
        if (date == null) {
            return ret;
        }
        SimpleDateFormat formatter = new SimpleDateFormat(format);
        ret = formatter.format(date);
        return ret;
    }

    /**
     * 获取明天
     * nextDay
     *
     * @return: Date  指定日期明天
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static Date nextDay(Date resDate) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(resDate);
        calendar.add(Calendar.DATE, 1);
        return calendar.getTime();
    }

    /**
     * 计算月份差，date1要小于date2
     * getMonthNum
     *
     * @return: int
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static int getMonthNum(String date1, String date2) {
        if (StringUtils.isNotEmpty(date1) || date1.length() < 10 || StringUtils.isEmpty(date2) || date2.length() < 10) {
            return 0;
        }
        String[] strArra1 = date1.split("-");
        String[] strArra2 = date2.split("-");
        int day1 = Integer.valueOf(strArra1[2]);
        int day2 = Integer.valueOf(strArra2[2]);
        int dayNum = day1 - day2;
        int day = 0;
        if (dayNum < 0) {
            day = 1;
        }
        return Math.abs(Integer.valueOf(strArra1[0]) * 12 - Integer.valueOf(strArra2[0]) * 12 + Integer.valueOf(strArra1[1]) - Integer.valueOf(strArra2[1])) + day;
    }

    /**
     * 判断时间是否在某个时间段内
     * isbetween
     *
     * @return: boolean
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static boolean isbetween(Date bdate, Date edate, Date date) {
        return (date.after(bdate) || date.equals(bdate)) && (date.before(edate) || date.equals(edate));
    }

    /**
     * 将时间日期从str变成date
     * strToDate
     *
     * @return: boolean
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static Date strToDate(String dateStr) {
        if (dateStr == null || "".equals(dateStr.trim())) {
            return null;
        }
        try {
            return strToDate(dateStr, DATE_FORMAT_PATTERN);
        } catch (Exception e) {
            log.info("strToDate", e);
        }
        return null;
    }

    /**
     * 将时间日期从str变成date
     * strToDate
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static Date strToDate(String dateStr, String format) {
        Date ret = null;
        try {
            SimpleDateFormat formatter = new SimpleDateFormat(format);
            ret = formatter.parse(dateStr);
        } catch (ParseException e) {
            log.info("strToDate", e);
        }
        return ret;
    }

    /**
     * 获取昨天
     * yesterDay
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static Date yesterDay(Date resDate) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(resDate);
        calendar.add(Calendar.DATE, -1);
        return calendar.getTime();
    }

    /**
     * 获取N天时间戳
     * yesterDay
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static Long getDayTime(Long time, int day) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeInMillis(time);
        calendar.add(Calendar.DATE, day);
        return calendar.getTimeInMillis();
    }

    /**
     * 日期转化
     * formatDate
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static Date formatDate(Date date, String formatStr) {
        Date ret = null;
        try {
            SimpleDateFormat formatter = new SimpleDateFormat(formatStr);
            String dateStr = formatter.format(date);
            ret = formatter.parse(dateStr);
        } catch (Exception e) {
            log.info("formatDate", e);

        }
        return ret;
    }


    /**
     * 把格林时间转本地时间
     * formatGMLData
     *
     * @return: Date
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static String formatGMLData(String date) {
        if (date.indexOf("Z") != -1) {
            String dateStr = date.replace("Z", " UTC");
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS Z");
            try {
                Date dt = sdf.parse(dateStr);
                SimpleDateFormat format = new SimpleDateFormat(DATETIME_FORMAT_PATTERN);
                dateStr = format.format(dt);
                return dateStr;
            } catch (ParseException e) {
                log.info("formatGMLData", e);
            }
        }
        return date;
    }


    /**
     * 判断时间七天内的时间
     * isLatestWeek
     *
     * @return: boolean
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static boolean isLatestWeek(String date) {
        if (StringUtils.isBlank(date)) {
            return false;
        }
        Calendar calendar = Calendar.getInstance();  //得到日历
        calendar.setTime(new Date());//把当前时间赋给日历
        calendar.add(Calendar.DAY_OF_MONTH, 7);  //设置为7天前
        Date after7time = calendar.getTime();   //得到7天前的时间
        Date addDate = parse(date, DATETIME_FORMAT_PATTERN);
        return addDate.getTime() > after7time.getTime();
    }
    /////////////////copy By isz_api

    /**
     * 获取指定时间所在年份
     * getYear
     *
     * @return: int
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static int getYear(Date date) {
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        return c.get(Calendar.YEAR);
    }


    /**
     * 获取指定时间所在月份,日期为空抛出异常NullPointerException
     * getMonth
     *
     * @return: int
     * @author: huasheng
     * @version 创建时间：2017-05-26 17:41
     */
    public static int getMonth(Date date) {
        if (null == date) {
            throw new NullPointerException("指定日期不能为空");
        }
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        return c.get(Calendar.MONTH) + 1;
    }


    /**
     * 获取每月的开始日期或者结束日期
     *
     * @param year
     * @param month
     * @param type
     * @return
     */
    public static Date getStartOrEndMonthDayByMonth(String year, String month, String type) {
        if (org.apache.commons.lang3.StringUtils.isNotEmpty(year) && StringUtils.isNotEmpty(month)) {
            //如果月份的数字是1，则需要前面加0
            if (month.length() == 1) {
                month = "0" + month;
            }
            String yearMonth = year + month;
            Date monthDate = parse(yearMonth, DateUtil.YEARMONTH_FORMAT);
            if (MONTH_START.equals(type)) {
                return getStartDateTimeOfMonth(monthDate);
            } else if (MONTH_END.equals(type)) {
                return getEndDateTimeOfMonth(monthDate);
            }
        }
        return null;
    }

    /**
     * 获取某天的时间戳
     *
     * @param time
     * @param day
     * @return
     */
    public static Long getTime(Long time, Integer day) {
        Calendar c = Calendar.getInstance();
        c.clear();
        c.setTimeInMillis(time);
        c.add(Calendar.DATE, day);
        return c.getTimeInMillis();
    }

    /**
     * 返回格式化后的日期（yyyy-MM-dd）
     *
     * @param format 格式化对象
     * @param time   时间
     * @return 格式化后的日期
     */
    public static String getDateTime(String format, Long time) {
        SimpleDateFormat s = new SimpleDateFormat(format);
        return s.format(time);
    }

    /**
     * 取时间戳
     *
     * @param time
     * @return
     * @throws ParseException
     */
    public static Long getDateTime(String format, String time) throws ParseException {
        SimpleDateFormat s = new SimpleDateFormat(format);
        return s.parse(time).getTime();
    }

    /**
     * 获取当前年份
     *
     * @return
     * @throws ParseException
     */
    public static Integer getCurrentYear() {
        Calendar date = Calendar.getInstance();
        return date.get(Calendar.YEAR);
    }

    /**
     * 计算2个日期之间相差的  相差多少年月日
     * 比如：2011-02-02 到  2017-03-02 相差 6年，1个月，0天
     *
     * @param fromDate
     * @param toDate
     * @return
     */
    public static String dayComparePrecise(long fromDate, long toDate) {
        Calendar from = Calendar.getInstance();
        from.setTimeInMillis(fromDate);
        Calendar to = Calendar.getInstance();
        to.setTimeInMillis(toDate);

        int fromYear = from.get(Calendar.YEAR);
        int fromMonth = from.get(Calendar.MONTH) + 1;
        int fromDay = from.get(Calendar.DAY_OF_MONTH);
        int fromHour = from.get(Calendar.HOUR_OF_DAY);

        int toYear = to.get(Calendar.YEAR);
        int toMonth = to.get(Calendar.MONTH) + 1;
        int toDay = to.get(Calendar.DAY_OF_MONTH);
        int toHour = to.get(Calendar.HOUR_OF_DAY);


        int year = toYear - fromYear;
        if (0 != year) {
            return year + "年前";
        }
        int month = toMonth - fromMonth;
        if (0 != month) {
            return month + "个月前";
        }

        int day = toDay - fromDay;
        if (0 != day) {
            return day + "天前";
        }
        int hour = toHour - fromHour;
        if (0 != hour) {
            return hour + "小时前";
        }

        return "1小时前";
    }

    /**
     * 获取更新时间
     */
    public static String getLastTime(Long fromLastTime, Long toLastTime) {
        return "最近更新: " + DateUtil.dayComparePrecise(fromLastTime, toLastTime);
    }

    /**
     * 获取最新更新时间
     */
    public static String getUpdateTime(Long from, Long nowTime) {

        String msg;
        long dateDiff = nowTime - from;

        if (dateDiff < 0) {
            msg = "输入的时间不对";
        } else {
            /** 秒 */
            long dateTemp1 = dateDiff / 1000;
            /** 分钟 */
            long dateTemp2 = dateTemp1 / 60;
            /** 小时 */
            long dateTemp3 = dateTemp2 / 60;
            /** 天数 */
            long dateTemp4 = dateTemp3 / 24;
            /** 月数 */
            long dateTemp5 = dateTemp4 / 30;
            /** 年数 */
            long dateTemp6 = dateTemp5 / 12;

            if (dateTemp6 > 0) {
                msg = dateTemp6 + "年前更新";
            } else if (dateTemp5 > 0) {
                msg = dateTemp5 + "个月前更新";
            } else if (dateDiff > var4 && dateTemp4 >= 2) {
                msg = dateTemp4 + "天前更新";
            } else if (dateDiff > var3 && dateDiff < var4) {
                msg = "昨天更新";
            } else if (dateDiff > var2 && dateDiff < var3) {
                msg = dateTemp3 + "小时前更新";
            } else if (dateDiff > var1 && dateDiff < var2) {
                msg = "30分钟前更新";
            } else if (dateDiff > var0 && dateDiff < var1) {
                msg = "刚刚上传";
            } else {
                msg = "刚刚上传";
            }
        }

        return msg;
    }

    /**
     * 根据传入的年份，计算当前年龄
     *
     * @param year
     * @return
     */
    public static Integer getAge(Integer year) {
        if (year != null && year != 0) {
            return DateUtil.getCurrentYear() - year;
        }
        return null;
    }


    /**
     * 判断str是否是标准格式的时间日期，并返回一个标准的时间格式 时分秒
     *
     * @param str
     */
    public static String returnDateTime(String str) {
        if (StringUtils.isEmpty(str)) {
            return str;
        }

        if (str.length() == 10 && str.indexOf("-") > -1) {
            DateFormat format = new SimpleDateFormat(DATE_FORMAT_PATTERN);
            try {
                Date strDate = format.parse(str);
                return DateUtil.format(strDate, DateUtil.DATETIME_FORMAT_PATTERN);
            } catch (ParseException e) {
                log.error("格式化错误");
            }
        } else if (str.length() == 19 && str.indexOf("-") > -1 && str.indexOf(":") > -1) {
            return str;
        }
        return str;
    }

    public static String getFormatDate(String str) {
        if (StringUtils.isEmpty(str)) {
            return str;
        }
        DateFormat format = new SimpleDateFormat();
        if ((str.length() == 19 || str.length() == 20) && str.indexOf("年") > -1 && str.indexOf("月") > -1 && str.indexOf("日") > -1 && str.indexOf(":") > -1) {
            format = new SimpleDateFormat(DATETIME_OTHER_FORMAT_PATTERN);
        } else if (str.length() == 19 && str.indexOf("/") > -1 && str.indexOf(":") > -1) {
            format = new SimpleDateFormat(APP_DATE_FORMAT_PATTERN);
        } else if (str.length() == 19 && str.indexOf("-") > -1 && str.indexOf(":") > -1) {
            return str;
        }
        try {
            Date strDate = format.parse(str);
            return DateUtil.format(strDate, DateUtil.DATETIME_FORMAT_PATTERN);
        } catch (ParseException e) {
            log.error("格式化错误");
        }
        return null;
    }

    /**
     * 根据模式得到一个比较器
     */
    public static Comparator<String> returnTimeComparator(String pattern, String order) {
        Comparator<String> comparator = (time1, time2) -> {
            try {
                Long date1 = parse(time1, pattern).getTime();
                Long date2 = parse(time2, pattern).getTime();
                return order.equals(DESC) ? date2.compareTo(date1) : date1.compareTo(date2);
            } catch (Exception e) {
                return -1;
            }
        };
        return comparator;
    }

}
