#' Plot the national flag of the People's Republic of China
#'
#' Draws the national flag of the People's Republic of China at the regulation
#' proportions using pure geometric computation and \code{ggplot2}. The shape
#' and orientation of the stars are computed analytically so that each small
#' star points to the centre of the large star. No external image or SVG
#' resources are used, which makes the function suitable for teaching and
#' programmatic graphics.
#'
#' @param label Logical; whether to display the title and axis text. If
#'   \code{TRUE} (default) the title, designer and reference information are
#'   shown; if \code{FALSE} only the flag is drawn.
#'
#' @return A \code{ggplot} object, which can be printed directly or saved with
#'   \code{ggsave()}.
#'
#' @details
#' \itemize{
#'   \item The flag has a 3:2 aspect ratio.
#'   \item The large star is placed in the upper-left canton.
#'   \item The four small stars are positioned per the specification and
#'     rotated to point at the large star.
#'   \item \code{ggplot2::coord_quickmap()} keeps a 1:1 x:y ratio.
#' }
#'
#' @examples
#' \donttest{
#' plot_P.R.CHINA_flag()
#' plot_P.R.CHINA_flag(label = FALSE)
#' }
#'
#' @author Flag design: Zeng Liansong.
#'
#' @seealso \code{\link[ggplot2]{geom_polygon}},
#'   \code{\link[ggplot2]{coord_quickmap}}
#'
#' @export
plot_P.R.CHINA_flag<-function(label=TRUE){
  # ------------------------------------------------------------
  # 图像几何绘制方法参考 B 站视频：BV1Ni4y1978g
  # 本脚本通过纯几何计算 + ggplot2 绘制标准五星红旗
  # ------------------------------------------------------------
  # ------------------------------------------------------------
  # 标题和坐标轴标签
  # ------------------------------------------------------------
  if(label==TRUE){
    labels<-list(x='\u8bbe\u8ba1\u53c2\u8003B\u7ad9\u89c6\u9891\uff1aBV1Ni4y1978g',  # 设计参考B站视频：BV1Ni4y1978g
                 y='\u8bbe\u8ba1\u8005\uff1a\u66fe\u8054\u677e',  # 设计者：曾联松
                 title='R\u8bed\u8a00\u7ed8\u5236\u6807\u51c6\u4e94\u661f\u7ea2\u65d7')  # R语言绘制标准五星红旗
  }else{
    labels<-list(x='',
                 y='',
                 title='')
  }
  # ------------------------------------------------------------
  # 函数1：计算两条由两点确定的直线的交点
  # (x1,y1)-(x2,y2) 与 (x3,y3)-(x4,y4)
  # ------------------------------------------------------------
  line_line_point<-function(x1,y1,x2,y2,x3,y3,x4,y4){
    # 第一条直线斜率
    k1 = (y2 - y1) / (x2 - x1)
    # 第二条直线斜率
    k2 = (y4 - y3) / (x4 - x3)
    # 直线截距
    b1 = y1 - k1 * x1
    b2 = y3 - k2 * x3
    # 联立求解交点
    x0 = (b2 - b1) / (k1 - k2)
    y0 = k1 * x0 + b1
    result<-c(x0,y0)
    result
  }

  # ------------------------------------------------------------
  # 函数2：构造五角星的 10 个顶点
  # x0, y0 : 星星中心坐标
  # r      : 外接圆半径
  # w      : 整体旋转角度（弧度）
  # ------------------------------------------------------------
  star_construction_point<-function(x0,y0,r,w)#中心坐标，半径，旋转角度
  {
    onefifth2pi=2*pi/5 # 五角星相邻顶点夹角
    # 外圈五个顶点坐标
    x1=x0+r*sin(w)
    y1=y0+r*cos(w)
    x2=x0+r*sin((w+onefifth2pi))
    x3=x0+r*sin((w+2*onefifth2pi))
    x4=x0+r*sin((w+3*onefifth2pi))
    x5=x0+r*sin((w+4*onefifth2pi))
    y2=y0+r*cos((w+onefifth2pi))
    y3=y0+r*cos((w+2*onefifth2pi))
    y4=y0+r*cos((w+3*onefifth2pi))
    y5=y0+r*cos((w+4*onefifth2pi))
    # ----------------------------------------------------------
    # 内部五个顶点：通过外顶点连线求交点
    # ----------------------------------------------------------
    xy6<-c(line_line_point(x1,y1,x3,y3,x5,y5,x2,y2))
    x6=xy6[1]
    y6=xy6[2]
    xy7<-c(line_line_point(x2,y2,x4,y4,x1,y1,x3,y3))
    x7=xy7[1]
    y7=xy7[2]
    xy8<-c(line_line_point(x3,y3,x5,y5,x4,y4,x2,y2))
    x8=xy8[1]
    y8=xy8[2]
    xy9<-c(line_line_point(x1,y1,x4,y4,x5,y5,x3,y3))
    x9=xy9[1]
    y9=xy9[2]
    xy10<-c(line_line_point(x1,y1,x4,y4,x5,y5,x2,y2))
    x10=xy10[1]
    y10=xy10[2]
    # 返回顺序排列的 10 个点（用于 geom_polygon）
    result<-c(x1,x6,x2,x7,x3,x8,x4,x9,x5,x10,
              y1,y6,y2,y7,y3,y8,y4,y9,y5,y10)
    result
  }
  # ------------------------------------------------------------
  # 函数3：计算小星需要旋转的角度
  # 目标：让小星的某个角“指向”大星中心
  # ------------------------------------------------------------
  # x0,y0 : 大星中心
  # x,y   : 小星中心
  # xn,yn : 小星某个顶点
  # r     : 小星半径
  w_cal<-function(x0,y0,x,y,xn,yn,r){
    # 大星中心 - 小星中心 的连线
    k=(y0-y)/(x0-x)
    b=y-k*x
    # 点到直线的距离
    distance=(abs(k*xn-yn+b))/((1+k^2)^0.5)
    # 根据几何关系反推旋转角
    w=asin(distance/r)
    w
  }
  # ------------------------------------------------------------
  # 初始状态下各星的坐标（未旋转）
  # ------------------------------------------------------------
  # 小星 1
  star1_x<-c(star_construction_point(10,18,1,0)[1:10])
  star1_y<-c(star_construction_point(10,18,1,0)[11:20])
  star1<-data.frame(star1_x,star1_y)
  # 小星 2
  star2_x<-c(star_construction_point(12,16,1,0)[1:10])
  star2_y<-c(star_construction_point(12,16,1,0)[11:20])
  star2<-data.frame(star1_x,star1_y)
  # 小星 3
  star3_x<-c(star_construction_point(12,13,1,0)[1:10])
  star3_y<-c(star_construction_point(12,13,1,0)[11:20])
  star3<-data.frame(star1_x,star1_y)
  # 小星 4
  star4_x<-c(star_construction_point(10,11,1,0)[1:10])
  star4_y<-c(star_construction_point(10,11,1,0)[11:20])
  star4<-data.frame(star1_x,star1_y)
  # 大星
  star0_x<-c(star_construction_point(5,15,3,0)[1:10])
  star0_y<-c(star_construction_point(5,15,3,0)[11:20])
  star0<-data.frame(star1_x,star1_y)#构建数据框
  #星1是x4对准，星2是x5对准，星3是x1对准，星4是x5对准
  # ------------------------------------------------------------
  # 计算四颗小星的旋转角度
  # 不同小星指向大星的不同顶点
  # w1为正，w2为负，w3为负，w4正
  # ------------------------------------------------------------
  w1<-w_cal(5,15,10,18,star1_x[7],star1_y[7],1)
  w2<-w_cal(5,15,12,16,star2_x[9],star2_y[9],1)
  w3<-w_cal(5,15,12,13,star3_x[1],star3_y[1],1)
  w4<-w_cal(5,15,10,11,star4_x[9],star4_y[9],1)#计算旋转角度绝对值
  # 方向修正（顺/逆时针）
  w2<-w2*(-1)
  w3<-w3*(-1)#校正角度正负（旋转方向）
  # ------------------------------------------------------------
  # 重新构建旋转后的小星坐标
  # ------------------------------------------------------------
  star0_x<-c(star_construction_point(5,15,3,0)[1:10])
  star0_y<-c(star_construction_point(5,15,3,0)[11:20])
  star0<-data.frame(star1_x,star1_y)
  star1_x<-c(star_construction_point(10,18,1,w1)[1:10])
  star1_y<-c(star_construction_point(10,18,1,w1)[11:20])
  star1<-data.frame(star1_x,star1_y)
  star2_x<-c(star_construction_point(12,16,1,w2)[1:10])
  star2_y<-c(star_construction_point(12,16,1,w2)[11:20])
  star2<-data.frame(star1_x,star1_y)
  star3_x<-c(star_construction_point(12,13,1,w3)[1:10])
  star3_y<-c(star_construction_point(12,13,1,w3)[11:20])
  star3<-data.frame(star1_x,star1_y)
  star4_x<-c(star_construction_point(10,11,1,w4)[1:10])
  star4_y<-c(star_construction_point(10,11,1,w4)[11:20])
  star4<-data.frame(star1_x,star1_y)#重新构建数据框

  # ------------------------------------------------------------
  # 背景红旗矩形
  # ------------------------------------------------------------
  rectangle<-data.frame(c(0,30,30,0),c(0,0,20,20))
  colnames(rectangle)<-c('x','y')
  # ------------------------------------------------------------
  # ggplot 绘图
  # ------------------------------------------------------------
  ggplot2::ggplot()+
    ggplot2::geom_polygon(data = rectangle,ggplot2::aes(x=x,y=y),color="#ee1c25",
                 fill="#ee1c25")+
    ggplot2::geom_polygon(data = star0,ggplot2::aes(x=star0_x,y=star0_y),color="#ffff00",
                 fill="#ffff00")+
    ggplot2::geom_polygon(data = star1,ggplot2::aes(x=star1_x,y=star1_y),color="#ffff00",
                 fill="#ffff00")+
    ggplot2::geom_polygon(data = star2,ggplot2::aes(x=star2_x,y=star2_y),color="#ffff00",
                 fill="#ffff00")+
    ggplot2::geom_polygon(data = star3,ggplot2::aes(x=star3_x,y=star3_y),color="#ffff00",
                 fill="#ffff00")+#绘图星星
    ggplot2::geom_polygon(data = star4,ggplot2::aes(x=star4_x,y=star4_y),color="#ffff00",
                 fill="#ffff00")+#绘图红旗
    ggplot2::coord_quickmap()+#调整为1：1比例显示
    ggplot2::theme(legend.key = ggplot2::element_blank(),
          panel.grid.major=ggplot2::element_line(colour=NA),
          panel.background = ggplot2::element_rect(fill = "transparent",colour = NA),
          plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid  = ggplot2::element_blank())+#隐藏坐标系
    ggplot2::labs(x=labels$x,
         y=labels$y,
         title=labels$title)+
    showtext::showtext_auto()#显示中文文本
}



#' Plot the Five-Colored Flag of the Beiyang Government
#'
#' Draws the Five-Colored Flag used during the Beiyang Government period
#' (circa 1912-1928) of the Republic of China, using horizontal colour bands
#' rendered with \code{ggplot2}. The flag has five equal horizontal stripes in
#' red, yellow, blue, white and black, symbolising the Han, Manchu, Mongol, Hui
#' and Tibetan peoples. The drawing is fully programmatic and uses no external
#' image resources.
#'
#' @param label Logical; whether to display the title and explanatory text.
#'   \code{TRUE} (default) shows the title and annotations; \code{FALSE} draws
#'   only the flag.
#'
#' @return A \code{ggplot} object, which can be printed or saved with
#'   \code{ggsave()}.
#'
#' @details
#' \itemize{
#'   \item The flag is built from five equal-height horizontal rectangles.
#'   \item From top to bottom the colours are red, yellow, blue, white, black.
#'   \item These represent the Han, Manchu, Mongol, Hui and Tibetan peoples.
#'   \item \code{ggplot2::coord_quickmap()} keeps the proportions undistorted.
#'   \item Axes, grid and legend are hidden.
#' }
#'
#' @examples
#' \donttest{
#' plot_ROC_Beiyang_flag()
#' plot_ROC_Beiyang_flag(label = FALSE)
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_rect}},
#'   \code{\link[ggplot2]{coord_quickmap}}
#'
#' @author Historical flag of the Beiyang Government era.
#'
#' @export
plot_ROC_Beiyang_flag<-function(label=TRUE){
  # ------------------------------------------------------------
  # 标题和坐标轴标签
  # ------------------------------------------------------------
  if(label==TRUE){
    labels<-list(x='\u5317\u6d0b\u653f\u5e9c - \u4e94\u65cf\u5171\u548c\u65d7',  # 北洋政府 - 五族共和旗
                 y='\u6c49\u6ee1\u8499\u56de\u85cf\n\uff08\u7ea2\u9ec4\u84dd\u767d\u9ed1\uff09',  # 汉满蒙回藏\n（红黄蓝白黑）
                 title='\u5317\u6d0b\u653f\u5e9c\u65f6\u671f\uff081912-1928\uff09')  # 北洋政府时期（1912-1928）
  }else{
    labels<-list(x='',
                 y='',
                 title='')
  }
  # ------------------------------------------------------------
  # 不同颜色块坐标
  # ------------------------------------------------------------
  rect_data<-rbind(
    data.frame(xmin=0,xmax=8,ymin=4,ymax=5,color='red',order=1),
    data.frame(xmin=0,xmax=8,ymin=3,ymax=4,color='gold',order=2),
    data.frame(xmin=0,xmax=8,ymin=2,ymax=3,color='darkblue',order=3),
    data.frame(xmin=0,xmax=8,ymin=1,ymax=2,color='white',order=4),
    data.frame(xmin=0,xmax=8,ymin=0,ymax=1,color='black',order=5)
  )
  rect_data$order<-factor(rect_data$order,levels=rect_data$order)
  # ------------------------------------------------------------
  # ggplot 绘图
  # ------------------------------------------------------------
  ggplot2::ggplot()+
    ggplot2::geom_rect(data = rect_data,
              mapping = ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
                            fill=order))+
    ggplot2::scale_fill_manual(values=rect_data$color)+
    ggplot2::coord_quickmap()+#调整为1：1比例显示
    ggplot2::theme(legend.key = ggplot2::element_blank(),
          panel.grid.major=ggplot2::element_line(colour=NA),
          panel.background = ggplot2::element_rect(fill = "transparent",colour = NA),
          plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = 'none',
          panel.grid  = ggplot2::element_blank())+#隐藏坐标系
    ggplot2::labs(x=labels$x,
         y=labels$y,
         title=labels$title)+
    showtext::showtext_auto()#显示中文文本
}



#' Plot the Blue Sky, White Sun, and a Wholly Red Earth flag
#'
#' Programmatically draws the Blue Sky, White Sun, and a Wholly Red Earth flag
#' used during the Nationalist Government period (1928-1949) of the Republic of
#' China, using analytic geometry with \code{ggplot2} and \code{ggforce}. The
#' flag has a red field, a blue canton in the upper-left, and a twelve-rayed
#' white sun within the canton, all generated from vector geometry without
#' external image files.
#'
#' @param label Logical; whether to display the title and explanatory text.
#'   \code{TRUE} (default) shows the title, designer and period; \code{FALSE}
#'   draws only the flag.
#'
#' @return A \code{ggplot} object, which can be printed or saved with
#'   \code{ggsave()}.
#'
#' @details
#' \itemize{
#'   \item The red field and blue canton are built from rectangles.
#'   \item The twelve-rayed sun is a closed polygon of 24 vertices.
#'   \item Two concentric circles form the core of the white sun.
#'   \item \code{ggplot2::coord_quickmap()} keeps the proportions undistorted.
#'   \item Axes, grid and legend are hidden by default.
#' }
#'
#' @examples
#' \donttest{
#' plot_ROC_KMT_flag()
#' plot_ROC_KMT_flag(label = FALSE)
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_polygon}},
#'   \code{\link[ggplot2]{geom_rect}},
#'   \code{\link[ggforce]{geom_circle}}
#'
#' @author Design: Sun Yat-sen (proposal) and Lu Haodong (Blue Sky, White Sun).
#'
#' @export
plot_ROC_KMT_flag<-function(label=TRUE){
  # ------------------------------------------------------------
  # 设计参考：https://zh.wikipedia.org/wiki/%E4%B8%AD%E8%8F%AF%E6%B0%91%E5%9C%8B%E5%9C%8B%E6%97%97
  # https://commons.wikimedia.org/wiki/File:Flag_of_the_Republic_of_China_construction_sheet.svg
  # ------------------------------------------------------------

  # ------------------------------------------------------------
  # 标题和坐标轴标签
  # ------------------------------------------------------------
  if(label==TRUE){
    labels<-list(x='\u56fd\u6c11\u653f\u5e9c - \u9752\u5929\u767d\u65e5\u6ee1\u5730\u7ea2\u65d7',  # 国民政府 - 青天白日满地红旗
                 y='\u8bbe\u8ba1\u8005\uff1a\u5b59\u4e2d\u5c71\u3001\u9646\u7693\u4e1c',  # 设计者：孙中山、陆皓东
                 title='\u56fd\u6c11\u653f\u5e9c\u65f6\u671f\uff081928-1949\uff09')  # 国民政府时期（1928-1949）
  }else{
    labels<-list(x='',
                 y='',
                 title='')
  }
  # ------------------------------------------------------------
  # 背景颜色块坐标
  # ------------------------------------------------------------
  rect_bg_red<-data.frame(xmin=0,xmax=48,ymin=0,ymax=32,color='red',order=1)
  rect_bg_blue<-data.frame(xmin=0,xmax=24,ymin=16,ymax=32,color='blue',order=1)
  # ------------------------------------------------------------
  # 十二星顶点和底点坐标（从正上方点顺时针排序ID）
  # ------------------------------------------------------------
  # 外接圆、太阳内小圆半径；圆心坐标
  r1=6;r2=3;loc_center<-c(12,24)
  # 第一象限内
  ##不在坐标轴的顶点坐标
  point_top1<-c(0,r1)
  point_top2<-c(r1*sin(pi/6),r1*cos(pi/6))
  point_top3<-c(r1*sin(pi/3),r1*cos(pi/3))
  ##低点坐标
  point_bottom1<-c(r2*sin(pi/12),r2*cos(pi/12))
  point_bottom2<-c(r2*sin(pi/4),r2*cos(pi/4))
  point_bottom3<-c(r2*cos(pi/12),r2*sin(pi/12))
  # 第二象限：第一象限关于X轴对称
  point_top4<-c(r1,0)
  point_top5<-c(point_top3[1],-point_top3[2])
  point_top6<-c(point_top2[1],-point_top2[2])
  ##低点坐标
  point_bottom4<-c(point_bottom3[1],-point_bottom3[2])
  point_bottom5<-c(point_bottom2[1],-point_bottom2[2])
  point_bottom6<-c(point_bottom1[1],-point_bottom1[2])
  # 第三象限：第一象限关于原点中心对称
  point_top7<-c(0,-r1)
  point_top8<-c(-point_top2[1],-point_top2[2])
  point_top9<-c(-point_top3[1],-point_top3[2])
  ##低点坐标
  point_bottom7<-c(-point_bottom1[1],-point_bottom1[2])
  point_bottom8<-c(-point_bottom2[1],-point_bottom2[2])
  point_bottom9<-c(-point_bottom3[1],-point_bottom3[2])
  # 第四象限：第一象限关于Y轴对称
  point_top10<-c(-r1,0)
  point_top11<-c(-point_top3[1],point_top3[2])
  point_top12<-c(-point_top2[1],point_top2[2])
  ##低点坐标
  point_bottom10<-c(-point_bottom3[1],point_bottom3[2])
  point_bottom11<-c(-point_bottom2[1],point_bottom2[2])
  point_bottom12<-c(-point_bottom1[1],point_bottom1[2])
  # 构建十二星闭合图案数据框
  star12_df<-data.frame(matrix(nrow=24,ncol=2)) #建立空白变量
  colnames(star12_df)<-c('x','y')
  star12_order<-paste0('point_',rep(c('top','bottom'),12),
                       rep(c(as.character(1:12)),each=2))
  for (i in 1:24) {
    star12_df$x[i]<-get(star12_order[i])[1]
    star12_df$y[i]<-get(star12_order[i])[2]
  }
  # 基于圆心平移图案
  star12_df$x<-star12_df$x+loc_center[1];star12_df$y<-star12_df$y+loc_center[2]

  # ------------------------------------------------------------
  # ggplot 绘图
  # ------------------------------------------------------------
  ggplot2::ggplot()+
    ggplot2::geom_rect(data = rect_bg_red,
              mapping = ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill='#DE0000')+
    ggplot2::geom_rect(data = rect_bg_blue,
              mapping = ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill='#0000AA')+
    ggplot2::geom_polygon(data = star12_df,ggplot2::aes(x=x,y=y),color="white",
                 fill="white")+
    ggforce::geom_circle(ggplot2::aes(x0=12, y0=24, r=3*(1+1/15)), fill='#0000AA')+
    ggforce::geom_circle(ggplot2::aes(x0=12, y0=24, r=3), fill='white')+
    ggplot2::coord_quickmap()+#调整为1：1比例显示
    ggplot2::theme(legend.key = ggplot2::element_blank(),
          panel.grid.major=ggplot2::element_line(colour=NA),
          panel.background = ggplot2::element_rect(fill = "transparent",colour = NA),
          plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = 'none',
          panel.grid  = ggplot2::element_blank())+#隐藏坐标系
    ggplot2::labs(x=labels$x,
         y=labels$y,
         title=labels$title)+
    showtext::showtext_auto()#显示中文文本
}





#' Plot the Iron-Blood 18-Star Flag of the Wuchang Uprising
#'
#' Draws the Iron-Blood 18-Star Flag (also called the Nine-Pointed
#' Eighteen-Star Flag) used by the Hubei military government after the Wuchang
#' Uprising of the Xinhai Revolution. The flag has a red field, a central black
#' nine-pointed star, and nine inner and nine outer yellow dots representing the
#' eighteen Han provinces of the time. All shapes are generated by vector
#' computation, without external image files.
#'
#' @param label Logical; whether to display the title and explanatory text.
#'   \code{TRUE} (default) shows the title, designer and historical background;
#'   \code{FALSE} draws only the flag.
#'
#' @return A \code{ggplot} object, which can be printed or saved with
#'   \code{ggsave()}.
#'
#' @details
#' \itemize{
#'   \item The aspect ratio is 5:8; x ranges over [-80, 80], y over [-50, 50].
#'   \item The nine-pointed star has a circumscribed radius of 44 units and an
#'     inner radius of 8 units.
#'   \item Nine outer yellow dots sit along the star tips; nine inner dots sit
#'     midway between the centre and the tips.
#'   \item \code{ggplot2::coord_fixed(ratio = 1)} keeps the proportions fixed.
#'   \item Axes, grid and legend are hidden by default.
#' }
#'
#' @examples
#' \donttest{
#' plot_Han18Star()
#' plot_Han18Star(label = FALSE)
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_polygon}},
#'   \code{\link[ggforce]{geom_circle}},
#'   \code{\link{plot_P.R.CHINA_flag}},
#'   \code{\link{plot_ROC_Beiyang_flag}},
#'   \code{\link{plot_ROC_KMT_flag}}
#'
#' @author Flag design: the Chinese Revolutionary Alliance (Tongmenghui).
#'
#' @references \url{https://www.19111010.com.tw/story?id=93}
#'
#' @export
plot_Han18Star<-function(label=TRUE){

  #参考链接：https://www.19111010.com.tw/story?id=93
  #设置旗子长160个单位，宽100个单位


  # ------------------------------------------------------------
  # 必要函数
  # ------------------------------------------------------------
  # 根据圆心和圆上两点计算对应圆心夹角度数的函数
  getAngle_byABO_circle<-
    function(O,#圆心坐标x,y
             A,#圆上一点坐标x,y
             B,#圆上另一点坐标x,y
             size, #large是优弧对应角，small是劣弧对应角
             r#圆半径
    ){

      # 检验AB点是否在圆上
      if(round(abs(sqrt((A[2]-O[2])^2+(A[1]-O[1])^2)),5)!=round(r,5)){
        message('A point is NOT on the cycle!')
        return(NA)
      }
      if(round(abs(sqrt((B[2]-O[2])^2+(B[1]-O[1])^2)),5)!=round(r,5)){
        message('B point is NOT on the cycle!')
        return(NA)
      }

      # 计算AB点弦长
      distance_AB<-
        abs(sqrt((B[2]-A[2])^2+(B[1]-A[1])^2))
      # 计算劣弧对应角度
      angle_small<-asin((distance_AB/2)/r)*2
      # 基于劣弧结果计算优弧对应结果
      angle_large<-2*pi-asin((distance_AB/2)/r)*2
      #输出
      if(size=='large'){
        return(angle_large)
      }else if(size=='small'){
        return(angle_small)
      }else{
        message('Input of Size is not correct.')
        return(NA)
      }
    }
  # 圆弧与直线交点坐标
  ## 以A点(xp,yp)为圆心，B点(x1,y1)为圆弧起点画圆，与直线CD：C(x2,y2),
  ## D(x3,y3)交与E点(x4,y4)和F(x5,y5)。side为X轴方向上靠左还是靠右。
  getPointCrossLineAndCircle<-
    function(xp,yp,x1,y1,x2,y2,x3,y3,side){
      #开始求圆弧和直线交点
      k=((y2-y3)/(x2-x3))#计算CD斜率
      if(!is.infinite(k)){
        b=y2-(k*x2)#计算CD截距
        r=((x1-xp)^2+(y1-yp)^2)^0.5#计算圆弧对应完整圆的半径
        a=k^2+1
        b2=2*(b*k-k*yp-xp)
        c=xp^2+(b-yp)^2-r^2#标准一元二次方程求根
        delta=b2^2-4*a*c#验证是否有实根
        x4=(b2*(-1)+((delta)^0.5))/(2*a)
        x5=(b2*(-1)-((delta)^0.5))/(2*a)
        y4=k*x4+b
        y5=k*x5+b#求交点坐标
        x_left<-min(c(x4,x5))
        x_right<-max(c(x4,x5))
        if(side=='left'){
          return(c(x_left,k*x_left+b))
        }else if(side=='right'){
          return(c(x_right,k*x_right+b))
        }
      }else if(is.infinite(k)){
        #斜率无限大/无限小，即垂直线与圆交点，此时x2==x3
        if(x2!=x3){
          message('k is infinite but x2 is not equal as x3')
          return(NA)
        }else{
          r=sqrt((xp-x1)^2+(yp-y1)^2)
          x_get=unique(c(x2,x3))
          y1_get<-yp+sqrt(r^2-(x2-xp)^2)
          y2_get<-yp-sqrt(r^2-(x2-xp)^2)
          if(side=='top'){
            return(c(x_get,max(y1_get,y2_get)))
          }else if(side=='bottom'){
            return(c(x_get,min(y1_get,y2_get)))
          }
        }
      }

    }
  # 计算两点之间距离
  dis2points<-function(p1,p2){
    return(
      sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)
    )
  }
  # 计算直线 AB 与直线 CD 的交点
  getLineIntersection <- function(A, B, C, D, tol = 1e-10) {
    # A, B, C, D 均为长度为 2 的向量 c(x, y)
    x1 <- A[1]; y1 <- A[2]
    x2 <- B[1]; y2 <- B[2]
    x3 <- C[1]; y3 <- C[2]
    x4 <- D[1]; y4 <- D[2]
    # 行列式
    denom <- (x1 - x2) * (y3 - y4) -
      (y1 - y2) * (x3 - x4)
    # 平行或重合
    if (abs(denom) < tol) {
      return(list(
        intersect = FALSE,
        type = "parallel_or_collinear",
        point = NULL
      ))
    }
    # 交点坐标
    px <- ((x1*y2 - y1*x2) * (x3 - x4) -
             (x1 - x2) * (x3*y4 - y3*x4)) / denom

    py <- ((x1*y2 - y1*x2) * (y3 - y4) -
             (y1 - y2) * (x3*y4 - y3*x4)) / denom
    return(list(
      intersect = TRUE,
      type = "unique",
      point = c(px, py)
    ))
  }
  # 求两圆交点中靠左或靠右的点
  circle_intersection <- function(x1, y1, r1, x2, y2, r2, side = c("left", "right")) {
    side <- match.arg(side)

    # 圆心距
    dx <- x2 - x1
    dy <- y2 - y1
    d <- sqrt(dx^2 + dy^2)

    # 容差
    eps <- .Machine$double.eps^0.5

    # 无交点或无穷多交点的情况
    if (d > r1 + r2 + eps || d < abs(r1 - r2) - eps || (d < eps && abs(r1 - r2) < eps)) {
      return(c(NA_real_, NA_real_))
    }

    # 从圆心A沿AB方向到弦垂足的距离
    a <- (r1^2 - r2^2 + d^2) / (2 * d)
    # 垂足到交点的距离（半弦长）
    h2 <- r1^2 - a^2
    h <- if (h2 < 0) 0 else sqrt(h2)   # 处理相切或极小数值误差

    # 垂足坐标
    x0 <- x1 + a * dx / d
    y0 <- y1 + a * dy / d

    # 垂直于AB的单位向量（两个方向）
    # 注意：dx, dy 可能为0，但d>0时除法安全
    vx <- -dy / d
    vy <-  dx / d

    # 两个交点
    p1 <- c(x0 - h * vx, y0 - h * vy)
    p2 <- c(x0 + h * vx, y0 + h * vy)

    # 根据 side 选择
    if (side == "left") {
      # x 较小者；若相等则返回第一个（即 p1 和 p2 中x坐标较小的那一个）
      if (p1[1] <= p2[1]) p1 else p2
    } else { # "right"
      if (p1[1] > p2[1]) p1 else p2
    }
  }

  #背景
  p_bg<-
    ggplot2::ggplot()+
    ggplot2::geom_rect(mapping = ggplot2::aes(xmin=-80,xmax=80,ymin=-50,ymax=50),
                       fill='red')+
    ggplot2::coord_fixed(ratio = 1)

  #黑正九角星
  ##建立点坐标
  PointDF<-data.frame(
    ID=NA,
    X=NA,Y=NA
  )
  ##开始按照说明计算点坐标
  ##辅助点坐标数据框
  PointDF<-rbind(PointDF,
                 data.frame(
                   ID='O',X=0,Y=0
                 ))
  ##从数据框中调出坐标的函数
  getPointLoc<-function(ID){
    return(
      list(
        X=PointDF$X[which(PointDF$ID==ID)],
        Y=PointDF$Y[which(PointDF$ID==ID)]
      )
    )
  }
  #1.取大小
  rO_Small=8;rO_Big=44
  #2.定两点
  PointDF<-rbind(PointDF,
                 data.frame(
                   ID=c('G','D','A1','A'),
                   X=c((-rO_Big*sqrt(3)/2),rO_Big*sqrt(3)/2,0,0),
                   Y=c(rep(-rO_Big/2,2),-rO_Big,rO_Big)
                 ))#G、D点的坐标一眼就可以看出
  PointDF<-rbind(PointDF,
                 data.frame(
                   ID=c('B1','C1'),
                   X=c(-rO_Big/2,rO_Big/2),
                   Y=c(rep(rO_Big*sqrt(3)/2,2))
                 ))#B1 C1点的坐标一眼就可以看出
  PointDF<-rbind(PointDF,
                 data.frame(
                   ID=c('D1'),
                   X=c(0),
                   Y=c(-rO_Big/3)
                 ))#D1点的坐标简单计算可以知道
  PointDF<-rbind(PointDF,
                 data.frame(
                   ID=c('F'),
                   X=getPointCrossLineAndCircle(
                     0,0,#O
                     0,-rO_Big,#A1
                     0,-rO_Big/3,#D1
                     0+1,-rO_Big/3+sqrt(3),#子线斜率为3，过D点
                     'left'
                   )[1],
                   Y=getPointCrossLineAndCircle(
                     0,0,#O
                     0,-rO_Big,#A1
                     0,-rO_Big/3,#D1
                     0+1,-rO_Big/3+sqrt(3),#子线斜率为根3，过D点
                     'left'
                   )[2]
                 ))#F
  PointDF<-rbind(PointDF,
                 data.frame(
                   ID=c('E'),
                   X=getPointCrossLineAndCircle(
                     0,0,#O
                     0,-rO_Big,#A1
                     0,-rO_Big/3,#D1
                     0+1,-rO_Big/3-sqrt(3),#子线斜率为3，过D1点
                     'right'
                   )[1],
                   Y=getPointCrossLineAndCircle(
                     0,0,#O
                     0,-rO_Big,#A1
                     0,-rO_Big/3,#D1
                     0+1,-rO_Big/3-sqrt(3),#丑线斜率为负根3，过D1点
                     'right'
                   )[2]
                 ))#E
  #3.移中心
  PointDF<-rbind(PointDF,
                 data.frame(
                   ID=c('E1'),
                   X=0,
                   Y=getPointLoc('E')$Y
                 ))#E1
  PointDF<-rbind(PointDF,
                 data.frame(
                   ID=c('F1'),
                   X=0,
                   Y=mean(c(getPointLoc('E')$Y,getPointLoc('A')$Y))
                 ))#F1
  PointDF$Y<-PointDF$Y-getPointLoc('F1')$Y
  PointDF<-PointDF[which(PointDF$ID %in% c('O','A','G','F','E','D')),]
  #4.连九星
  PointDF<-rbind(PointDF,
                 data.frame(
                   ID=c('E1','F1'),
                   X=c((2*getPointLoc('O')$X-getPointLoc('E')$X),
                       (2*getPointLoc('O')$X-getPointLoc('F')$X)),
                   Y=c((2*getPointLoc('O')$Y-getPointLoc('E')$Y),
                       (2*getPointLoc('O')$Y-getPointLoc('F')$Y))
                 ))
  H_loc<-circle_intersection(getPointLoc('O')$X,getPointLoc('O')$Y,rO_Big,
                             getPointLoc('E1')$X,getPointLoc('E1')$Y,rO_Big,'left')
  B_loc<-circle_intersection(getPointLoc('O')$X,getPointLoc('O')$Y,rO_Big,
                             getPointLoc('E1')$X,getPointLoc('E1')$Y,rO_Big,'right')
  I_loc<-circle_intersection(getPointLoc('O')$X,getPointLoc('O')$Y,rO_Big,
                             getPointLoc('F1')$X,getPointLoc('F1')$Y,rO_Big,'left')
  C_loc<-circle_intersection(getPointLoc('O')$X,getPointLoc('O')$Y,rO_Big,
                             getPointLoc('F1')$X,getPointLoc('F1')$Y,rO_Big,'right')
  PointDF<-rbind(PointDF,
                 data.frame(
                   ID=c('H','B','I','C'),
                   X=c(H_loc[1],B_loc[1],I_loc[1],C_loc[1]),
                   Y=c(H_loc[2],B_loc[2],I_loc[2],C_loc[2])
                 ))
  PointStar<-data.frame(
    ID=NA,X=NA,Y=NA
  )
  for(id in c('A','F','B','G','C','H','D','I','E','A')){
    PointStar<-
      rbind(PointStar,
            data.frame(
              ID=id,
              X=getPointLoc(id)$X,
              Y=getPointLoc(id)$Y
            ))
  }
  PointStar<-PointStar[!is.na(PointStar$X),]
  #windows版会出现geom_polygon导致的空白，因此要单独建立一个图层
  Star_Cross<-data.frame(
    Line1Point1=c('A','I','H','G','F','A','G','H','B'),
    Line1Point2=c('F','E','D','C','B','E','C','C','G'),
    Line2Point1=c('I','H','G','A','E','H','I','B','A'),
    Line2Point2=c('D','C','B','F','I','D','D','F','E'),
    X=NA,
    Y=NA
  )
  for(i in 1:nrow(Star_Cross)){
    Star_Cross_i<-
    getLineIntersection(
      A=c(getPointLoc(Star_Cross$Line1Point1[i])$X,getPointLoc(Star_Cross$Line1Point1[i])$Y),
      B=c(getPointLoc(Star_Cross$Line1Point2[i])$X,getPointLoc(Star_Cross$Line1Point2[i])$Y),
      C=c(getPointLoc(Star_Cross$Line2Point1[i])$X,getPointLoc(Star_Cross$Line2Point1[i])$Y),
      D=c(getPointLoc(Star_Cross$Line2Point2[i])$X,getPointLoc(Star_Cross$Line2Point2[i])$Y),
      tol = 1e-10
    )
    Star_Cross$X[i]<-Star_Cross_i$point[1]
    Star_Cross$Y[i]<-Star_Cross_i$point[2]
  }
  p_Star<-list(
    ggplot2::geom_path(data=PointStar,ggplot2::aes(x=X,y=Y)),
    ggplot2::geom_polygon(data=PointStar,ggplot2::aes(x=X,y=Y),fill='black'),
    ggplot2::geom_polygon(data=Star_Cross,ggplot2::aes(x=X,y=Y),fill='black')
  )

  #外圈的黄圆
  p_Circle_Out<-list(
    ggforce::geom_circle(data=PointStar,
                         ggplot2::aes(x0=X,y0=Y,r=rO_Small/2),
                         fill = "yellow", color=NA)
  )

  #内圈黄圆
  CircleInside<-data.frame(
    ID=1,X=0,Y=1.5*rO_Small
  )
  for(i in 2:nrow(dplyr::distinct(PointStar))){
    CircleInside<-
      rbind(CircleInside,
            data.frame(
              ID=i,
              X=getPointCrossLineAndCircle(xp=getPointLoc('O')$X,yp=getPointLoc('O')$Y,
                                           x1=getPointLoc('O')$X+1.5*rO_Small,y1=getPointLoc('O')$Y,
                                           x2=getPointLoc('O')$X,y2=getPointLoc('O')$Y,
                                           x3=dplyr::distinct(PointStar)$X[i],y3=dplyr::distinct(PointStar)$Y[i],
                                           side=ifelse(dplyr::distinct(PointStar)$X[i]<=0,'left','right'))[1],
              Y=getPointCrossLineAndCircle(xp=getPointLoc('O')$X,yp=getPointLoc('O')$Y,
                                           x1=getPointLoc('O')$X+1.5*rO_Small,y1=getPointLoc('O')$Y,
                                           x2=getPointLoc('O')$X,y2=getPointLoc('O')$Y,
                                           x3=dplyr::distinct(PointStar)$X[i],y3=dplyr::distinct(PointStar)$Y[i],
                                           side=ifelse(dplyr::distinct(PointStar)$X[i]<=0,'left','right'))[2]
            ))
  }
  p_Circle_Inside<-list(
    ggforce::geom_circle(data=CircleInside,
                         ggplot2::aes(x0=X,y0=Y,r=rO_Small/2),
                         fill = "yellow", color=NA)
  )

  if(label==TRUE){
    labels<-list(x='\u94c1\u8840\u5341\u516b\u661f\u65d7',  # 铁血十八星旗
                 y='\u8bbe\u8ba1\uff1a\u4e2d\u56fd\u9769\u547d\u540c\u76df\u4f1a',  # 设计：中国革命同盟会
                 title='\u6b66\u660c\u8d77\u4e49 \u2014 \u4e2d\u534e\u6c11\u56fd\u6e56\u5317\u519b\u653f\u5e9c')  # 武昌起义 — 中华民国湖北军政府
  }else{
    labels<-list(x='',
                 y='',
                 title='')
  }

  return(
    p_bg+
      p_Star+
      p_Circle_Out+p_Circle_Inside+
      ggplot2::labs(
        x=labels$x,y=labels$y,title=labels$title
      )+
      ggplot2::scale_y_continuous(limits = c(-50,50))+
      ggplot2::scale_x_continuous(limits = c(-80,80))+
      ggplot2::theme(legend.key = ggplot2::element_blank(),
                     panel.grid.major=ggplot2::element_line(colour=NA),
                     panel.background = ggplot2::element_rect(fill = "transparent",colour = NA),
                     plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
                     panel.grid.minor = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     legend.position = 'none',
                     plot.title = ggplot2::element_text(face = 'bold',hjust=0.5),
                     panel.grid  = ggplot2::element_blank())+
      showtext::showtext_auto()
  )
}



