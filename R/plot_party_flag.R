#' Plot the emblem or flag of the Communist Party of China (CCP)
#'
#' This function programmatically renders the emblem of the Communist Party of
#' China using analytic geometry and ggplot2-based vector graphics. The symbol
#' is constructed entirely from geometric primitives (arcs, polygons, and
#' rectangles), without relying on any external image files.
#'
#' Two rendering modes are supported:
#' \itemize{
#'   \item \code{"flag"}: A rectangular background with a 3:2 aspect ratio,
#'   suitable for flag-style visualization.
#'   \item \code{"logo"}: A square (1:1) background, suitable for emblem or
#'   logo-style visualization.
#' }
#'
#' @param plot_type Character string specifying the rendering mode.
#'   Either \code{"flag"} (default) or \code{"logo"}.
#' @param label Logical value indicating whether to display textual annotations
#'   (title and axis labels). Default is \code{FALSE}.
#'
#' @return A \code{ggplot} object representing the CCP emblem or flag.
#'
#' @details
#' The geometric construction follows a stepwise layering strategy, including
#' the outer and inner arcs of the sickle, the handle, and the hammer body.
#' All coordinates are transformed into a unified plotting coordinate system.
#'
#' @examples
#' \donttest{
#' plot_CCP(plot_type = "flag")
#' plot_CCP(plot_type = "logo")
#' }
#'
#' @author Per the regulations on the emblem and flag of the Communist Party of China.
#'
#' @seealso \code{\link{plotCNFlag}} for the unified flag plotting interface.
#'
#' @export
plot_CCP<-function(plot_type='flag',label=FALSE){
  # ------------------------------------------------------------
  # 标题和坐标轴标签
  # ------------------------------------------------------------
  if(label==TRUE){
    labels<-list(x='\u9570\u5200\u9524\u5b50',  # 镰刀锤子
                 y='\u8bbe\u8ba1\uff1a1996\u300a\u4e2d\u56fd\u5171\u4ea7\u515a\u515a\u65d7\u515a\u5fbd\u5236\u4f5c\u548c\u4f7f\u7528\u7684\u82e5\u5e72\u89c4\u5b9a\u300b',  # 设计：1996《中国共产党党旗党徽制作和使用的若干规定》
                 title='\u4e2d\u56fd\u5171\u4ea7\u515a CCP \uff081921-\u81f3\u4eca\uff09')  # 中国共产党 CCP （1921-至今）
  }else{
    labels<-list(x='',
                 y='',
                 title='')
  }
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
  # 把视频中的坐标转换为绘图用第一象限坐标的函数
  loc_trans<-function(x,y){
    return(c((x-1),(33-y)))
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

  # 定义标准色
  std_cols<-list(gold='#FDCF30',red='#ED2C25')


  # ------------------------------------------------------------
  # 计算与绘图
  # ------------------------------------------------------------

  #图层Layer0：背景红色
  #先画背景
  if(plot_type=='flag'){ #旗帜大小3：2背景
    p_bg_layer0<-
      ggplot2::ggplot()+
      ggplot2::geom_rect(mapping = ggplot2::aes(xmin=(-16),xmax=144,ymin=(-52),ymax=44),fill=std_cols$red)
  }else if(plot_type=='logo'){ #徽章大小1：1背景
    p_bg_layer0<-
      ggplot2::ggplot()+
      ggplot2::geom_rect(mapping = ggplot2::aes(xmin=(-16),xmax=48,ymin=(-16),ymax=48),fill=std_cols$red)
  }else{ #把旗帜大小作为输入错误时默认的背景大小
    p_bg_layer0<-
      ggplot2::ggplot()+
      ggplot2::geom_rect(mapping = ggplot2::aes(xmin=(-16),xmax=144,ymin=(-52),ymax=44),fill=std_cols$red)
  }


  #------------------镰刀主体--------------------
  #图层Layer1：镰刀的外圆
  #在画镰刀的外圆右侧扇形，填充黄色（视频中M为圆心，N、O在圆上）
  #原始点坐标：M(17,17), N(17,1), O(17,33)
  loc_N<-loc_trans(17,1)
  loc_M<-loc_trans(17,17)
  loc_O<-loc_trans(17,33)
  p_circle1_layer1<-
    ggforce::geom_arc_bar(
      ggplot2::aes(x0 = loc_trans(17,17)[1],
          y0 = loc_trans(17,17)[2],
          r0 = 0,
          r = abs(17-1),
          start = 0,
          end = (pi)),color=std_cols$gold,fill=std_cols$gold
    )
  #在画镰刀的外圆做侧扇形，填充黄色（视频中P为圆心，Q、O在圆上）
  #原始点坐标：P(17,15), G(8.5,18.5), H(19.5,7.5)
  ##转换点坐标
  loc_P<-loc_trans(17,15)
  loc_G<-loc_trans(8.5,18.5)
  loc_H<-loc_trans(19.5,7.5)
  ##求Q点坐标。Q点为直线GH与以P为圆心、PO为半径的圆的靠左交点
  loc_Q<-getPointCrossLineAndCircle(xp=loc_P[1],yp=loc_P[2],x1=loc_O[1],y1=loc_O[2],
                                    x2=loc_G[1],y2=loc_G[2],x3=loc_H[1],y3=loc_H[2],
                                    side = 'left')
  ##画扇形
  p_circle2_layer1<-
    ggforce::geom_arc_bar(
      ggplot2::aes(x0 = loc_P[1],
          y0 = loc_P[2],
          r0 = 0,
          r = abs(loc_P[2]-loc_O[2]),
          start = (pi),
          end = (pi+getAngle_byABO_circle(O=loc_P,A=loc_O,B=loc_Q,
                                          size='small',r=abs(loc_P[2]-loc_O[2])))),
      color=std_cols$gold,fill=std_cols$gold
    )

  #图层Layer2：镰刀的内圆
  #画镰刀的上半右侧扇形，填充红色（视频中R为圆心，N、S在圆上）
  #原始点坐标：R(11,16.5)
  ##转换点坐标
  loc_R<-loc_trans(11,16.5)
  loc_S<-getPointCrossLineAndCircle(xp=loc_R[1],yp=loc_R[2],x1=loc_N[1],y1=loc_N[2],
                                    x2=loc_R[1],y2=loc_R[2],x3=0,y3=loc_R[2],
                                    side = 'right')
  ##画扇形
  p_circle1_layer2<-
    ggforce::geom_arc_bar(
      ggplot2::aes(x0 = loc_R[1],
          y0 = loc_R[2],
          r0 = 0,
          r = abs(dis2points(loc_R,loc_N)),
          start = (pi/2-getAngle_byABO_circle(O=loc_R,A=loc_N,B=loc_S,
                                              size='small',r=abs(dis2points(loc_R,loc_N)))),
          end = (pi/2)),
      color=std_cols$red,fill=std_cols$red
    )
  #画镰刀的下半右侧扇形，填充红色（视频中R为圆心，N、S在圆上）
  #原始点坐标：T(16.5,16.5)
  ##转换点坐标
  loc_T<-loc_trans(16.5,16.5)
  loc_U<-getPointCrossLineAndCircle(xp=loc_T[1],yp=loc_T[2],x1=loc_S[1],y1=loc_S[2],
                                    x2=loc_T[1],y2=loc_T[2],x3=loc_T[1],y3=0,
                                    side = 'bottom')
  ##画扇形
  p_circle2_layer2<-
    ggforce::geom_arc_bar(
      ggplot2::aes(x0 = loc_T[1],
          y0 = loc_T[2],
          r0 = 0,
          r = abs(dis2points(loc_T,loc_S)),
          start = (pi/2),
          end = (pi)),
      color=std_cols$red,fill=std_cols$red
    )
  #画镰刀的下半左侧扇形，填充红色（视频中V为圆心，U、W在圆上）
  #原始点坐标：V(16.5,11)
  ##转换点坐标
  loc_V<-loc_trans(16.5,11)
  loc_W<-getPointCrossLineAndCircle(xp=loc_V[1],yp=loc_V[2],x1=loc_U[1],y1=loc_U[2],
                                    x2=loc_H[1],y2=loc_H[2],x3=loc_G[1],y3=loc_G[2],
                                    side = 'left')
  ##画扇形
  p_circle3_layer2<-
    ggforce::geom_arc_bar(
      ggplot2::aes(x0 = loc_V[1],
          y0 = loc_V[2],
          r0 = 0,
          r = abs(dis2points(loc_V,loc_U)),
          start = (pi),
          end = (pi+
                   getAngle_byABO_circle(O=loc_V,A=loc_U,B=loc_W,
                                         size='small',r=abs(dis2points(loc_V,loc_U))))),
      color=std_cols$red,fill=std_cols$red
    )
  #------------------镰刀圆柄--------------------
  #图层Layer3：镰刀的圆柄
  #画镰刀的圆柄，填充红色（视频中X为圆心，与坐标轴相切）
  #原始点坐标：X(3.5,30.5)
  loc_X<-loc_trans(3.5,30.5)
  p_circle_layer3<-
    ggforce::geom_arc_bar(
      ggplot2::aes(x0 = loc_X[1],
          y0 = loc_X[2],
          r0 = 0,
          r = abs(loc_trans(3.5,30.5)),
          start = (0),
          end = (2*pi)),
      color=std_cols$gold,fill=std_cols$gold
    )
  #画镰刀柄体连接部
  #原始点坐标：Y(6,30), Z(4,28)
  loc_Y<-loc_trans(6,30)
  loc_Z<-loc_trans(4,28)
  #镰刀柄体连接部两条线与对角线y=x平行
  p_rect_layer3<-
    ggplot2::geom_polygon(
      data =
        data.frame(x=c(loc_Z[1]-1,loc_Z[1]+1,loc_Y[1]+1,loc_Z[1]+1),
                   y=c(loc_Z[2]-1,loc_Z[2]+1,loc_Y[2]+1,loc_Y[2]-1)),
      mapping = ggplot2::aes(x=x,y=y),
      fill=std_cols$gold,
      color=std_cols$gold
    )

  #------------------锤子--------------------
  #锤子柄
  #原始点坐标：E(29,33), F(33,29)
  loc_E<-loc_trans(29,33)
  loc_F<-loc_trans(33,29)
  ##计算另外两个点：与GH直线的连线
  loc_hammer_p1<-getLineIntersection(loc_E,c(loc_E[1]-1,loc_E[2]+1),loc_G,loc_H)$point
  loc_hammer_p2<-getLineIntersection(loc_F,c(loc_F[1]-1,loc_F[2]+1),loc_G,loc_H)$point
  ##锤子柄绘图
  p_rect1_layer4<-
    ggplot2::geom_polygon(
      data =
        data.frame(x=c(loc_E[1],loc_F[1],loc_hammer_p2[1],loc_hammer_p1[1]),
                   y=c(loc_E[2],loc_F[2],loc_hammer_p2[2],loc_hammer_p1[2])),
      mapping = ggplot2::aes(x=x,y=y),fill=std_cols$gold,color=std_cols$gold
    )

  #锤子体
  #原始点坐标：I(4,14), J(17,5), K(13.5,1)
  loc_I<-loc_trans(4,14)
  loc_J<-loc_trans(17,5)
  loc_K<-loc_trans(13.5,1)
  #计算缺角与锤子体另一交点L（以K为圆心，KJ为半径，J、L在圆上，直线IL斜率为1）
  loc_L<-getPointCrossLineAndCircle(xp=loc_K[1],yp=loc_K[2],
                                    x1=loc_J[1],y1=loc_J[2],
                                    x2=loc_I[1],y2=loc_I[2],
                                    x3=loc_I[1]+1,y3=loc_I[2]+1,
                                    side = 'left')
  #计算缺角覆盖的锤子体矩形的隐藏点（直线IL与直线HJ连线）
  loc_hidden<-getLineIntersection(loc_I,loc_L,loc_H,loc_J)$point


  ##锤子柄绘图
  p_rect2_layer4<-
    ggplot2::geom_polygon(
      data =
        data.frame(x=c(loc_G[1],loc_I[1],loc_hidden[1],loc_H[1]),
                   y=c(loc_G[2],loc_I[2],loc_hidden[2],loc_H[2])),
      mapping = ggplot2::aes(x=x,y=y),
      fill=std_cols$gold,
      color=std_cols$gold
    )
  ##锤子柄缺角绘图
  p_circle_layer4<-
    ggforce::geom_arc_bar(
      ggplot2::aes(x0 = loc_K[1],
          y0 = loc_K[2],
          r0 = 0,
          r = abs(dis2points(loc_K,loc_J)),
          start = (pi/2+
                     getAngle_byABO_circle(
                       O=loc_K,
                       A=c(loc_K[1]+abs(dis2points(loc_K,loc_J)),32),
                       B=loc_J,
                       size='small',r=abs(dis2points(loc_K,loc_J)))),
          end = (pi/2+
                   getAngle_byABO_circle(O=loc_K,A=loc_J,B=loc_L,
                                         size='small',r=abs(dis2points(loc_K,loc_J)))+
                   getAngle_byABO_circle(
                     O=loc_K,
                     A=c(loc_K[1]+abs(dis2points(loc_K,loc_J)),32),
                     B=loc_J,
                     size='small',r=abs(dis2points(loc_K,loc_J))))),
      color=std_cols$red,fill=std_cols$red
    )


  ccp_logo<-
    p_bg_layer0+ #背景图层
    p_circle1_layer1+p_circle2_layer1+ #镰刀体黄色图层
    p_circle1_layer2+p_circle2_layer2+p_circle3_layer2+ #镰刀体红色图层
    p_circle_layer3+p_rect_layer3+ #镰刀柄
    p_rect1_layer4+p_rect2_layer4+p_circle_layer4+ #锤子
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
  return(ccp_logo)
}




#' Plot the Blue Sky with a White Sun flag (Kuomintang / Republic of China)
#'
#' This function programmatically renders the Blue Sky with a White Sun symbol
#' using analytic geometry and ggplot2-based vector graphics. The emblem
#' consists of a blue background and a twelve-rayed white sun, constructed
#' entirely from geometric primitives such as polygons, circles, and arcs,
#' without relying on any external image files.
#'
#' The geometric construction follows the official construction sheets and
#' historical specifications of the flag, ensuring proportional accuracy of the
#' twelve rays and concentric circles.
#'
#' @param label Logical value indicating whether to display textual annotations
#'   (title and axis labels). Default is \code{TRUE}.
#'
#' @return A \code{ggplot} object representing the Blue Sky with a White Sun flag.
#'
#' @details
#' The twelve-rayed sun is constructed by alternating outer and inner vertices
#' arranged in clockwise order, forming a closed polygon. Two concentric circles
#' are then overlaid to form the central white sun core.
#'
#' @references
#' Wikipedia contributors. Flag of the Republic of China.
#' \url{https://en.wikipedia.org/wiki/Flag_of_the_Republic_of_China}
#'
#' Wikimedia Commons.
#' \url{https://commons.wikimedia.org/wiki/File:Flag_of_the_Republic_of_China_construction_sheet.svg}
#'
#' @seealso \code{\link{plotCNFlag}} for the unified flag plotting interface.
#'
#' @examples
#' \donttest{
#' plot_KMT()
#' plot_KMT(label = FALSE)
#' }
#'
#' @author Design: Lu Haodong.
#'
#' @export
plot_KMT<-function(label=TRUE){
  # ------------------------------------------------------------
  # 设计参考：https://zh.wikipedia.org/wiki/%E4%B8%AD%E8%8F%AF%E6%B0%91%E5%9C%8B%E5%9C%8B%E6%97%97
  # https://commons.wikimedia.org/wiki/File:Flag_of_the_Republic_of_China_construction_sheet.svg
  # ------------------------------------------------------------

  # ------------------------------------------------------------
  # 标题和坐标轴标签
  # ------------------------------------------------------------
  if(label==TRUE){
    labels<-list(x='\u9752\u5929\u767d\u65e5\u65d7',  # 青天白日旗
                 y='\u8bbe\u8ba1\u8005\uff1a\u9646\u7693\u4e1c',  # 设计者：陆皓东
                 title='\u4e2d\u56fd\u56fd\u6c11\u515a KMT\uff081919-\u81f3\u4eca\uff09')  # 中国国民党 KMT（1919-至今）
  }else{
    labels<-list(x='',
                 y='',
                 title='')
  }
  # ------------------------------------------------------------
  # 背景颜色块坐标
  # ------------------------------------------------------------
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
    ggplot2::geom_rect(data = rect_bg_blue,
              mapping = ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill='#0000AA')+
    ggplot2::geom_polygon(data = star12_df,ggplot2::aes(x=x,y=y),color="white",
                 fill="white")+
    ggforce::geom_circle(ggplot2::aes(x0=12, y0=24, r=3*(1+1/15)), fill='#0000AA',color='#0000AA')+
    ggforce::geom_circle(ggplot2::aes(x0=12, y0=24, r=3), fill='white',color='white')+
    ggplot2::coord_quickmap()+#调整为1：1比例显示
    ggplot2::scale_y_continuous(limits = c(16,32))+
    ggplot2::scale_x_continuous(limits = c(0,24))+
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
