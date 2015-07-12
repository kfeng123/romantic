--------old

--日申购赎回汇总表
create table if not exists lt_user_balance_table_sum_baseline(
 report_date string comment '日期'
,total_purchase_amt bigint comment '日申购总额'
,total_redeem_amt bigint comment '日赎回总额'
) comment '日申购赎回汇总表' partitioned by (ds string);--ds：不同的分区，不同的时间区间。
insert overwrite table lt_user_balance_table_sum_baseline partition(ds='all')
select report_date
,sum(total_purchase_amt) total_purchase_amt
,sum(total_redeem_amt) total_redeem_amt
from tianchi_finance.user_balance_table --内部赛主办方提供的申购赎回记录表。
group by report_date;
--例如要用4-8月数据作为训练集
insert overwrite table lt_user_balance_table_sum_baseline partition(ds='45678month')
select report_date
,total_purchase_amt
,total_redeem_amt
from lt_user_balance_table_sum_baseline
where ds = 'all' and report_date >='20140401' and report_date < '20140901';

--构建训练集基础特征：
create table if not exists lt_basic_feature4to8_baseline as 
select t1.report_date
,case when t1.dayOfWeek=0 then 1 else 0 end as monday
,case when t1.dayOfWeek=1 then 1 else 0 end as tuesday 
,case when t1.dayOfWeek=2 then 1 else 0 end as wednesday
,case when t1.dayOfWeek=3 then 1 else 0 end as thursday
,case when t1.dayOfWeek=4 then 1 else 0 end as friday
,case when t1.dayOfWeek=5 then 1 else 0 end as saturday
,case when t1.dayOfWeek=6 then 1 else 0 end as sunday
,total_purchase_amt
,total_redeem_amt
from (
select report_date
 ,weekday(to_date(report_date,"yyyyMMdd")) dayOfWeek
,total_purchase_amt
,total_redeem_amt
from lt_user_balance_table_sum_baseline
where ds = '45678month' 
) t1;
--构建线上基础特征：
create table if not exists lt_basic_feature9_baseline as 
select t1.report_date
 ,case when t1.dayOfWeek=0 then 1 else 0 end as monday
,case when t1.dayOfWeek=1 then 1 else 0 end as tuesday 
,case when t1.dayOfWeek=2 then 1 else 0 end as wednesday
,case when t1.dayOfWeek=3 then 1 else 0 end as thursday
,case when t1.dayOfWeek=4 then 1 else 0 end as friday
,case when t1.dayOfWeek=5 then 1 else 0 end as saturday
,case when t1.dayOfWeek=6 then 1 else 0 end as sunday
from (
select report_date
 ,weekday(to_date(report_date,"yyyyMMdd")) dayOfWeek
from lt_predict_day9month_baseline --9月份日期数据可以通过R脚本上传。
) t1;




--new
--日申购赎回汇总表
create table if not exists user_balance as 
select 
user_id
,report_date
,tBalance
,yBalance
,total_purchase_amt
,direct_purchase_amt
,purchase_bal_amt
,purchase_bank_amt
,total_redeem_amt
,consume_amt
,transfer_amt
,tftobal_amt
,tftocard_amt
,share_amt
,case when category1 is null then 0 else category1 end as category1
,case when category2 is null then 0 else category2 end as category2
,case when category3 is null then 0 else category3 end as category3
,case when category4 is null then 0 else category4 end as category4
from tianchi_finance.user_balance_table;

create table if not exists myTot as
select 
report_date
,sum(tBalance) as tBalance
,sum(yBalance) as yBalance
,sum(total_purchase_amt) as total_purchase_amt
,sum(direct_purchase_amt) as direct_purchase_amt
,sum(purchase_bal_amt) as purchase_bal_amt
,sum(purchase_bank_amt) as purchase_bank_amt
,sum(total_redeem_amt) as total_redeem_amt
,sum(consume_amt) as consume_amt
,sum(transfer_amt) as transfer_amt
,sum(tftobal_amt) as tftobal_amt
,sum(tftocard_amt) as tftocard_amt
,sum(share_amt) as share_amt
,sum(category1) as category1
,sum(category2) as category2
,sum(category3) as category3
,sum(category4) as category4

from user_balance
group by report_date
;



