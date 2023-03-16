putdocx begin  //新建 Word 文档
putdocx paragraph, halign(center) //段落居中

*-定义字体、大小等基本设置
putdocx text ("附：文中待插入表格"), ///
        font("Times New Roman",16,black) bold linebreak  
  
*-保存名为 My_Table.docx 的 Word 文档   
putdocx save "My_Table.docx", replace

putdocx begin
putdocx pagebreak
putdocx save "My_Table.docx", append

drop if 城市 == "#N/A"
destring code- var185 ,replace
tostring code,replace 
gen citycode4 = substr( code ,1,4)
destring code citycode4 ,replace
merge 1:1 citycode4 year using "C:\Users\10058\Desktop\金融科技与经济韧性\fintech.dta"
keep if _merge == 3
drop _merge
drop if year == 2017
//这是用GDP算的
//求S
bysort  code: egen meangdp=mean( 地区生产总值万元 )
gen bias = abs(地区生产总值万元-meangdp)
gen s = bias/meangdp
gen y = 地区生产总值万元-meangdp
order code y year 
merge m:1 code using "C:\Users\10058\Desktop\金融科技与经济韧性\A1.dta"
keep if _merge == 3
drop _merge
//A是斜率,标准化
norm s , method(zee)
norm result, method(zee)
gen ER = zee_result- zee_s
//归一化
sum s
gen sds=(s-r(min))/(r(max)-r(min)) 
sum result
gen sda=(result-r(min))/(r(max)-r(min)) 
gen ER1 = sda - sds
//这是用人口算的)
gen sumpeople = 第一产业农林牧渔业从业人员数万人+第三产业从业人员数万人+第二产业从业人员数万人
bysort  code: egen meanpeople = mean( sumpeople )
gen bias1 = abs(sumpeople-meanpeople)
gen s1 = bias1/meanpeople
gen y1 = sumpeople -meanpeople
norm s1 , method(zee)
norm result_people, method(zee)
gen ER_people = zee_result_people- zee_s1
sum s1
gen sds1=(s1-r(min))/(r(max)-r(min)) 
sum result_people
gen sda1=(result_people-r(min))/(r(max)-r(min)) 
gen ER_people1 = sda1 - sds1



merge 1:1 code year using "C:\Users\10058\Desktop\金融科技与经济韧性\金融科技发展水平.dta"
keep if _merge == 3
drop _merge

gen rd = ln( 专利授权数件 +1)
gen eco = (货物进口额万元+ 货物出口额万元)/地区生产总值万元
gen fd = ( 年末金融机构存款余额万元+年末金融机构各项贷款余额万元 )/ 地区生产总值万元
gen fintech = ln(index_aggregate+1)
gen MD = ln(人口密度人平方公里 +1 )
gen pd  = 地方财政一般预算内支出万元/ 地区生产总值万元
gen ss = 第三产业增加值占GDP比重百分比/ 第二产业增加值占GDP比重百分比
gen RL = ln(每万人在校大学生数人+1)

/*merge 1:1 year 城市 using  "C:\Users\10058\Desktop\数据大全\地级市控制变量\人均道路面积.dta" 
keep if _merge == 3
drop _merge
merge 1:1 year 城市 using  "C:\Users\10058\Desktop\数据大全\地级市控制变量\科技支出水平.dta"
keep if _merge == 3
drop _merge*/
global control "  fd  eco RL pd"
winsor2 ER index_aggregate fintech $control  ,cut(1 99) replace
reg ER  fintech $control , r  //逐步回归
gen samp=e(sample)
/**************************   T1  Discriptive  ***************************/
sum2docx ER fintech   $control if samp == 1   using "My_Table.docx", append stats(N mean sd min p25 median p75 max) title("Table 1: Descriptive Statistics")
/**************************  T3  Baseline**************************/
xtset code year
reg ER fintech if samp == 1 ,  r
est store m1
reg ER  fintech $control if samp == 1, r  //逐步回归
est store m2
reg2docx m1 m2  using "My_Table.docx", append scalars(N r2_p(%9.4f)) b(%9.3f) t(%7.2f) title("Table 2: Baseline regression results")  note("Note: Robust t-statistics in brackets. Variables are defined in Appendix A. *** p< 0.01, ** p<0.05, * p<0.10")
summarize ER fintech if year == 2011
summarize ER fintech if year == 2014
summarize ER fintech if year == 2016
summarize ER fintech if year == 2020

/*********************    T4  Alternative Proxies  *******************/
*替换普惠金融(x)
reg ER 金融科技水平 $control if samp == 1,  r
est store m3
xtreg ER fintech   if samp == 1,fe r
est store m8
xtreg ER fintech $control  if samp == 1,fe r
est store m7
*稳健性检验都可以//或者只写这个
gen lncoverage_breadth = ln(coverage_breadth)
gen lnusage_depth  = ln(usage_depth)
gen lndigitization_level  = ln(digitization_level)
reg ER  lncoverage_breadth $control  if samp == 1,  r
est store m4
reg ER  lnusage_depth $control  if samp == 1, r
est store m5
reg ER   lndigitization_level $control if samp == 1 ,  r
est store m6

reg2docx   m8 m7 using  "My_Table.docx", append scalars(N r2_p(%9.4f)) b(%9.3f) t(%7.2f) title("Table 3: Alternative Proxies")  note("Note: Robust t-statistics in brackets. Variables are defined in Appendix A. *** p< 0.01, ** p<0.05, * p<0.10")
/**************************  T10 Channel   ***************************/
*创新
reg rd fintech  $control   if samp == 1, r
est store m8
*产业结构
reg ss fintech  $control   if samp == 1,  r
est store m9
reg2docx m8 m9   using  "My_Table.docx", append scalars(N r2_p(%9.4f)) b(%9.3f) t(%7.2f) title("Table 4: channel")  note("Note: Robust t-statistics in brackets. Variables are defined in Appendix A. *** p< 0.01, ** p<0.05, * p<0.10")
/**************************   T7 2SLS    ***************************/
sort code year
merge m:1 code using  "C:\Users\10058\Desktop\数据大全\到杭州的距离.dta"
keep if _merge == 3
drop _merge
gen Hdis = ln(distance+1)
winsor2 Hdis  ,cut(1 99) replace
ivregress2 2sls  fintech    $control (ER=Hdis)   if samp==1 , first
est restore first
est store firstL
ivregress2 2sls  fintech    $control (ER=Hdis)   if samp==1 , first
est store SecondL
reg2docx firstL SecondL  using  "My_Table.docx", append scalars(N r2_p(%9.4f)) b(%9.3f) t(%7.2f) title("Table 5: 2SLS ")  note("Note: Robust t-statistics in brackets. Variables are defined in Appendix A. *** p< 0.01, ** p<0.05, * p<0.10")

/**************************  T11 Cross-sectional***************************/

//基于地区视角
tostring code,replace
gen prov_code = substr(code,1,2)
destring code,replace
destring prov_code ,replace
merge m:m prov_code using "C:\Users\10058\Desktop\数据大全\省级-绿色金融.dta"
rename 所属省份 省份
gen cite = 0
replace cite =1 if 省份=="北京"|省份=="天津"|省份=="河北省"|省份=="上海"|省份=="江苏省"/*
 */|省份=="浙江省"|省份=="福建省"|省份=="山东省"|省份=="广东省"|省份=="海南省"
replace cite =3 if 省份=="内蒙古自治区"|省份=="广西壮族自治区"|省份=="重庆"|省份=="四川省"|省份=="贵州省"/*
  */|省份=="云南省"|省份=="西藏自治区"|省份=="陕西省"|省份=="甘肃省"|省份=="青海省"|省份=="宁夏回族自治区"|省份=="新疆维吾尔自治区"
replace cite=2 if 省份=="山西省"|省份=="安徽省"|省份=="江西省"|省份=="河南省"|省份=="湖北省"|省份=="湖南省"
replace cite = 4 if 省份=="黑龙江省"|省份=="吉林省"|省份=="辽宁省"
reg ER fintech  $control   if cite == 1  & samp == 1,  r
est store a1
reg ER fintech    $control if cite == 2 & samp == 1,  r
est store a2
reg ER fintech    $control  if cite == 3 & samp == 1,  r
est store a3
reg ER fintech    $control  if cite == 4 & samp == 1,  r
est store a4
reg2docx a1 a2 a3 a4  using  "My_Table.docx", append scalars(N r2_p(%9.4f)) b(%9.3f) t(%7.2f) title("Table 6: region")  note("Note: Robust t-statistics in brackets. Variables are defined in Appendix A. *** p< 0.01, ** p<0.05, * p<0.10")
*城市规模
gen size = 2
replace size = 1 if 地区=="张家口"|地区=="邢台"|地区=="廊坊"|地区=="衡水"|地区=="承德"|地区=="长治"|地区=="晋中"|地区=="临汾"|地区=="阳泉"|地区=="运城"|地区=="晋城"|地区=="赤峰"|地区=="乌海"|地区=="鄂尔多斯"|地区=="锦州"|地区=="营口"|地区=="本溪"|地区=="盘锦"|地区=="阜新"|地区=="辽阳"|地区=="丹东"|地区=="葫芦岛"|地区=="朝阳"|地区=="四平"|地区=="牡丹江"|地区=="鸡西"|地区=="佳木斯"|地区=="鹤岗"|地区=="秦州"|地区=="镇江"|地区=="宿迁"|地区=="湖州"|地区=="嘉兴"|地区=="金华"|地区=="舟山"|地区=="蚌埠"|地区=="安庆"|地区=="马鞍山"|地区=="淮北"|地区=="六安"|地区=="宿州"|地区=="滁州"|地区=="铜陵"|地区=="莆田"|地区=="漳州"|地区=="上饶"|地区=="九江"|地区=="抚州"|地区=="宜春"|地区=="吉安"|地区=="萍乡"|地区=="东营"|地区=="德州"|地区=="菏泽"|地区=="日照"|地区=="滨州"|地区=="商丘"|地区=="平顶山"|地区=="焦作"|地区=="新乡"|地区=="安阳"|地区=="信阳"|地区=="漯河"|地区=="濮阳"|地区=="许昌"|地区=="驻马店"|地区=="荆州"|地区=="十堰"|地区=="黄石"|地区=="孝感"|地区=="荆门"|地区=="随州"|地区=="湘潭"|地区=="岳阳"|地区=="常德"|地区=="邵阳"|地区=="郴州"|地区=="益阳"|地区=="怀化"|地区=="永州"|地区=="娄底"|地区=="揭阳"|地区=="茂名"|地区=="肇庆"|地区=="潮州"|地区=="韶关"|地区=="清远"|地区=="阳江"|地区=="玉林"|地区=="梧州"|地区=="北海"|地区=="三亚"|地区=="乐山"|地区=="内江"|地区=="德阳"|地区=="攀枝花"|地区=="遂宁"|地区=="眉山"|地区=="广元"|地区=="曲靖"|地区=="拉萨"|地区=="宝鸡"|地区=="榆林"|地区=="渭南"|地区=="汉中"|地区=="天水"


replace size = 0 if 地区=="朔州"|地区=="沂州"|地区=="吕梁"|地区=="通辽"|地区=="巴彦淖尔"|地区=="乌兰察布"|地区=="呼伦贝尔"|地区=="铁岭"|地区=="松原"|地区=="通化"|地区=="辽源"|地区=="白山"|地区=="白城"|地区=="双鸭山"|地区=="伊春"|地区=="七台河"|地区=="绥化"|地区=="黑河"|地区=="丽水"|地区=="衢州"|地区=="毫州"|地区=="宣城"|地区=="黄山"|地区=="池州"|地区=="龙岩"|地区=="宁德"|地区=="南平"|地区=="三明"|地区=="景德镇"|地区=="新余"|地区=="鹰潭"|地区=="鹤壁"|地区=="三门峡"|地区=="周口"|地区=="咸宁"|地区=="鄂州"|地区=="黄冈"|地区=="张家界"|地区=="梅州"|地区=="河源"|地区=="汕尾"|地区=="云浮"|地区=="贵港"|地区=="钦州"|地区=="百色"|地区=="河池"|地区=="来宾"|地区=="防城港"|地区=="贺州"|地区=="崇左"|地区=="儋州"|地区=="巴中"|地区=="广安"|地区=="资阳"|地区=="雅安"|地区=="安顺"|地区=="毕节"|地区=="铜仁"|地区=="六盘水"|地区=="玉溪"|地区=="保山"|地区=="昭通"|地区=="普洱"|地区=="丽江"|地区=="临沧"|地区=="日喀则"|地区=="昌都"|地区=="山南"|地区=="林芝"|地区=="那曲"|地区=="延安"|地区=="铜川"|地区=="安康"|地区=="商洛"|地区=="酒泉"|地区=="白银"|地区=="武威"|地区=="平凉"|地区=="张掖"|地区=="嘉峪关"|地区=="定西"|地区=="庆阳"|地区=="金昌"|地区=="陇南"|地区=="海东"|地区=="石嘴山"|地区=="吴忠"|地区=="固原"|地区=="中卫"|地区=="克拉玛依"|地区=="哈密"|地区=="吐鲁番"

reg ER fintech   $control  if size == 0& samp == 1,  r
est store b1
reg ER fintech   $control  if size == 1& samp == 1,  r
est store b2
reg ER fintech   $control  if size == 2& samp == 1,  r
est store b3
reg2docx b1 b2 b3 using  "My_Table.docx", append scalars(N r2_p(%9.4f)) b(%9.3f) t(%7.2f) title("Table 7: size")  note("Note: Robust t-statistics in brackets. Variables are defined in Appendix A. *** p< 0.01, ** p<0.05, * p<0.10")
