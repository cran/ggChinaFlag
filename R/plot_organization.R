#' Plot the flag of the Communist Youth League of China (CYLC)
#'
#' This function programmatically renders the flag of the Communist Youth
#' League of China using analytic geometry and ggplot2-based vector graphics.
#' The symbol is constructed entirely from geometric primitives (circles,
#' arcs, and polygons), without relying on any external image files.
#'
#' The geometric construction strictly follows the national standard
#' GB/T 40055-2021.
#'
#' @param label Logical value indicating whether to display textual annotations
#'   (title and axis labels). Default is \code{TRUE}.
#'
#' @return A \code{ggplot} object representing the CYLC flag.
#'
#' @details
#' The geometric construction follows a stepwise layering strategy. It begins
#' with a red rectangular background, followed by a golden circular ring
#' (composed of outer and inner circles), and finally a golden five-pointed
#' star. The star's ten vertices are precisely calculated through line
#' intersection algorithms. Coordinates and radii are scaled according to the
#' 24:16 aspect ratio specified in the standard.
#'
#' @examples
#' \donttest{
#' plot_CYLC(label = TRUE)
#' plot_CYLC(label = FALSE)
#' }
#'
#' @author Per the national standard GB/T 40055-2021.
#'
#' @seealso \code{\link{plot_CCP}} for the CCP emblem/flag plotting interface.
#'
#' @export
plot_CYLC<-function(label=TRUE){
  # ------------------------------------------------------------
  # 设计参考：国家标准 GB/T 40055-2021
  # ------------------------------------------------------------
  
  # ------------------------------------------------------------
  # 标题和坐标轴标签
  # ------------------------------------------------------------
  if(label==TRUE){
    labels <- list(
      x = '\u4e2d\u56fd\u5171\u4ea7\u4e3b\u4e49\u9752\u5e74\u56e2\u56e2\u65d7',  # 中国共产主义青年团团旗
      y = 'GB/T 40055-2021',  # 纵坐标：国家标准号
      title = '\u4e2d\u56fd\u5171\u4ea7\u4e3b\u4e49\u9752\u5e74\u56e2 CYLC'  # 中国共产主义青年团 CYLC
    )
  } else {
    labels <- list(
      x = '',
      y = '',
      title = ''
    )
  }
  
  # 定义标准色
  std_cols<-list(gold='#ffff00',red='#EE1C25')
  
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
  # 计算与绘图
  # ------------------------------------------------------------
  #图层Layer0：背景红色
  p_bg_layer0<-
    ggplot2::ggplot()+
    ggplot2::geom_rect(mapping = ggplot2::aes(xmin=(-12),xmax=12,ymin=(-8),ymax=8),fill=std_cols$red,color=std_cols$red)
  
  #图层Layer1：金色圆圈
  p_circle_layer1<-list(
    ggforce::geom_circle(mapping = ggplot2::aes(x0 = (-6), y0 = (4), r = (8/3)),fill=std_cols$gold,color=std_cols$gold),
    ggforce::geom_circle(mapping = ggplot2::aes(x0 = (-6), y0 = (4), r = 2),fill=std_cols$red,color=std_cols$red)
  )
  
  #图层Layer2：金色五角星
  # 绘图数据
  star0_x<-c(star_construction_point(x0 = (-6), y0 = (4), r = 2, w = 0)[1:10])
  star0_y<-c(star_construction_point(x0 = (-6), y0 = (4), r = 2, w = 0)[11:20])
  star0<-data.frame(star0_x,star0_y)#构建数据框
  p_star_layer2<-
    ggplot2::geom_polygon(data = star0,ggplot2::aes(x=star0_x,y=star0_y),
                          color=std_cols$gold,
                                           fill=std_cols$gold)
  p_cylc<-
    p_bg_layer0+ #背景图层
    p_circle_layer1+p_star_layer2+
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
  
  #输出
  return(p_cylc)
}