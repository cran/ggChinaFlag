#' Draw the Regional Flag of the Hong Kong Special Administrative Region
#'
#' Constructs the flag of the Hong Kong Special Administrative Region of the
#' People's Republic of China entirely from analytic geometry and renders it as
#' a \pkg{ggplot2} object. The five-petal bauhinia emblem is built by deriving
#' every construction point (circle intersections, line intersections, and
#' rotation angles) from the specification, then replicating a single petal five
#' times at 72-degree increments about the emblem centre.
#'
#' @details
#' The geometry follows the Chinese national standard GB 16689-2004. A single
#' petal is bounded by seven edge segments: six circular arcs drawn on four
#' distinct construction circles, plus one straight chord. Each arc is sampled
#' into a dense polyline (400 points by default) so that
#' \code{\link[ggplot2]{geom_polygon}} can fill the enclosed region; at this
#' sampling density the chordal deviation from the true arc is several orders of
#' magnitude below any practical display or print resolution.
#'
#' The flag field is a red rectangle with a 3:2 width-to-height ratio, sized
#' relative to the emblem's bounding circle radius. Petals are white and each
#' carries a red five-pointed star.
#'
#' All construction is performed in a local coordinate system with the emblem's
#' outer circle centred at the origin with radius 60. The function is
#' self-contained: it defines its own geometric helpers internally and depends
#' only on \pkg{ggplot2} for rendering and \pkg{showtext} for CJK glyph support.
#'
#' @param label Logical scalar. If \code{TRUE} (the default), the plot is
#'   annotated with a title and axis captions identifying the standard and
#'   drawing reference. If \code{FALSE}, all text annotations are suppressed,
#'   yielding a bare flag suitable for embedding or export.
#'
#' @return A \code{ggplot} object. The object can be printed, further modified
#'   with additional \pkg{ggplot2} layers, or written to disk with
#'   \code{\link[ggplot2]{ggsave}}.
#'
#' @section Rendering notes:
#' Correct display of the Chinese title and caption requires a font providing
#' CJK glyphs. The function calls \code{\link[showtext]{showtext_auto}}, but the
#' user must register a suitable font beforehand, for example via
#' \code{\link[sysfonts]{font_add}} or \code{\link[sysfonts]{font_add_google}}.
#' Without a registered CJK font the non-ASCII labels may render as blank boxes.
#'
#' The plot uses \code{\link[ggplot2]{coord_fixed}} to enforce a 1:1 aspect
#' ratio; altering the coordinate system will distort the circular arcs.
#'
#' @references
#' GB 16689-2004. \emph{Regional flag of the Hong Kong Special Administrative
#' Region of the People's Republic of China.} Standardization Administration of
#' China.
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{ggsave}}
#'
#' @examples
#' \donttest{
#' # Register a CJK font before plotting annotated output
#' if (requireNamespace("sysfonts", quietly = TRUE)) {
#'   sysfonts::font_add_google("Noto Sans SC", "notosans")
#'   showtext::showtext_auto()
#' }
#'
#' # Annotated flag
#' plot_HK_SAR_flag()
#'
#' # Bare flag, no text
#' plot_HK_SAR_flag(label = FALSE)
#'
#' }
#'
#' @export
plot_HK_SAR_flag<-function(label=TRUE){

  # ------------------------------------------------------------
  # 标题和坐标轴标签
  # ------------------------------------------------------------
  if(label==TRUE){
    labels<-list(x='\u53c2\u8003\uff1aGB 16689-2004\u3002\u89c6\u9891\u7ed8\u753b\u53c2\u8003\uff1aB\u7ad9 BV12A41167vr',  # 参考：GB 16689-2004。视频绘画参考：B站 BV12A41167vr
                 y='Regional flag of Hong Kong special administrativeregion',
                 title='\u9999\u6e2f\u7279\u522b\u884c\u653f\u533a\u533a\u65d7')  # 香港特别行政区区旗
  }else{
    labels<-list(x='',
                 y='',
                 title='')
  }

  #定义需要的函数
  {
    # ------------------------------------------------------------
    # 函数1：求旋转后点的坐标
    # x1, y1 : 被旋转点的坐标
    # x0, y0 : 旋转中心点坐标
    # x2, y2 : 旋转后点的坐标
    # angle  : 旋转角度数
    # 点A(x1,y1)绕着点O(x0,y0)顺时针旋转angle度角（angle的范围为实数）后为点B(x2,y2)
    # ------------------------------------------------------------
    rotate_point <- function(x1, y1, x0, y0, angle){
      a  <- angle * pi / 180           # 角度转弧度
      dx <- x1 - x0
      dy <- y1 - y0
      x2 <- x0 + dx * cos(a) + dy * sin(a)
      y2 <- y0 - dx * sin(a) + dy * cos(a)
      return(list(x=x2,y=y2))
    }
    # ------------------------------------------------------------
    # 函数2：计算两条由两点确定的直线的交点
    # (x1,y1)-(x2,y2) 与 (x3,y3)-(x4,y4)
    # ------------------------------------------------------------
    line_line_point <- function(x1,y1,x2,y2,x3,y3,x4,y4, tol = 1e-10){
      denom <- (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
      if (abs(denom) < tol) {
        warning("No Cross Point!")
        return(c(NA, NA))
      }
      d12 <- x1*y2 - y1*x2
      d34 <- x3*y4 - y3*x4
      x0 <- (d12*(x3 - x4) - (x1 - x2)*d34) / denom
      y0 <- (d12*(y3 - y4) - (y1 - y2)*d34) / denom
      c(x0, y0)
    }
    # ------------------------------------------------------------
    # 函数3：构造五角星的 10 个顶点
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
      result<-data.frame(
        x=c(x1,x6,x2,x7,x3,x8,x4,x9,x5,x10),
        y=c(y1,y6,y2,y7,y3,y8,y4,y9,y5,y10))
      result
    }
    # ------------------------------------------------------------
    # 函数4：根据圆心和圆上两点计算对应圆心夹角度数的函数
    # ------------------------------------------------------------
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
    # ------------------------------------------------------------
    # 函数5：圆弧与直线交点坐标
    # 以A点(xp,yp)为圆心，B点(x1,y1)为圆弧起点画圆，与直线CD：C(x2,y2),
    # D(x3,y3)交与E点(x4,y4)和F(x5,y5)。side为X轴方向上靠左还是靠右。
    # ------------------------------------------------------------
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
    # ------------------------------------------------------------
    # 函数6：计算两点之间距离
    # ------------------------------------------------------------
    dis2points<-function(p1,p2){
      return(
        sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)
      )
    }
    # ------------------------------------------------------------
    # 函数7：计算直线 AB 与直线 CD 的交点
    # ------------------------------------------------------------
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
    # ------------------------------------------------------------
    # 函数8：计算两圆交点
    # 圆1圆心O1(x01,y01)半径r1，圆2圆心O2(x02,y02)半径r2
    # tol 参数用来吸收浮点误差,尤其是相切时 r1^2 - a^2 理论上为 0 但计算可能得到极小负数
    # 代码里已经把负数截断为 0,避免 sqrt 产生 NaN。
    # ------------------------------------------------------------
    circle_intersect <- function(x01, y01, r1, x02, y02, r2, tol = 1e-10){
      #两圆点坐标距离
      dx <- abs(x02 - x01)
      dy <- abs(y02 - y01)
      d  <- sqrt(dx^2 + dy^2) #圆心连线长度

      # 特殊情况判断
      if (d < tol && abs(r1 - r2) < tol) {
        warning("The two circles coincide; there are infinitely many intersection points") # 两圆重合，交点有无穷多个
        return(NULL)
      }else if (d > r1 + r2 + tol) {
        warning("The two circles are separate; no intersection") # 两圆相离，无交点
        return(NULL)
      }else if (d < abs(r1 - r2) - tol) {
        warning("One circle lies inside the other; no intersection") # 一圆在另一圆内部，无交点
        return(NULL)
      }else if(d == abs(r1 - r2)){ # 两圆相切时交点为两圆心
        cross1_point1<-getPointCrossLineAndCircle(x01,y01,x01,y01-r1,x01,y01,x02,y02,'left')
        cross1_point2<-getPointCrossLineAndCircle(x01,y01,x01,y01-r1,x01,y01,x02,y02,'right')
        cross2_point1<-getPointCrossLineAndCircle(x02,y02,x02,y02-r2,x01,y01,x02,y02,'left')
        cross2_point2<-getPointCrossLineAndCircle(x02,y02,x02,y02-r2,x01,y01,x02,y02,'right')
        crosspoint_df<-
          data.frame(
            x=c(cross1_point1[1],cross1_point2[1],cross2_point1[1],cross2_point2[1]),
            y=c(cross1_point1[2],cross1_point2[2],cross2_point1[2],cross2_point2[2])
          )
        outputDF<-crosspoint_df[duplicated(crosspoint_df),]
        return(list(x=outputDF$x,y=outputDF$y))
      }else{
        # 两连线斜率
        k_o<-(y02-y01)/(x02-x01)

        # a: 从 O1 沿连心线到弦中点的距离
        a <- (d^2 + r1^2 - r2^2) / (2 * d)
        h2 <- r1^2 - a^2
        h  <- if (h2 < 0) 0 else sqrt(h2)   # 防止相切时浮点误差导致负数

        # 弦中点坐标
        if(x01<=x02){
          xm <- x01 + a * dx / d
        }else if(x01>x02){
          xm <- x01 - a * dx / d
        }
        if(y01<=y02){
          ym <- y01 + a * dy / d
        }else if(y01>y02){
          ym <- y01 - a * dy / d
        }
        # 两个交点（相切时两点重合）
        if(k_o>=0){
          x_1 <- xm + h * dy / d
          y_1 <- ym - h * dx / d
          x_2 <- xm - h * dy / d
          y_2 <- ym + h * dx / d
        }else if(k_o<0){
          x_1 <- xm + h * dy / d
          y_1 <- ym + h * dx / d
          x_2 <- xm - h * dy / d
          y_2 <- ym - h * dx / d
        }
        return(data.frame(x=c(x_1,x_2),y=c(y_1,y_2)))
      }
    }
    # ------------------------------------------------------------
    # 函数9：计算点旋转的角度（小于180度的那个角）
    # 旋转中心坐标O(x0,y0)，旋转前坐标A(x1,y1)，旋转后坐标B(x2,y2)
    # ------------------------------------------------------------
    getRoatingAngel<-function(O,A,B,tol = 1e-10){
      #计算弦长
      stringLength<-dis2points(A,B)
      #计算半径
      rA<-dis2points(O,A)
      rB<-dis2points(O,B)
      if(abs(rA - rB) > tol * max(rA, rB)){
        warning('OA is not equal OB!')
        stop()
      }else if(rA==rB){
        r<-(rA+rB)/2
      }
      #计算半弦对应的半旋转角度
      halfAngel<-asin((stringLength/2)/r)
      Angel<-halfAngel*2
      #输出
      return(Angel)
    }
  }

  # ------------------------------------------------------------
  # 绘图函数：单个紫荆花瓣
  # ------------------------------------------------------------
  singlePetalPlot<-function(RotationAngle,CenterLoc=c(0,0)){
    #默认先使用最大圆半径60、中心C1(0,0)绘图
    BigStarPoint<-star_construction_point(0,0,60,0)
    C1_point<-c(0,0)
    A_point<-c(-60,0);B_point<-c(60,0);C_point<-c(0,60);D_point<-c(0,-60)
    C2_point<-c(-30,0)
    F_point<-
      getPointCrossLineAndCircle(C2_point[1],C2_point[2],
                                 0,0,
                                 C2_point[1],C2_point[2],
                                 C_point[1],C_point[2],'right')
    C3_point<-c((F_point[1]+0)/2,(F_point[2]+60)/2)

    #这个点很重要
    P_point<-
    as.numeric(circle_intersect(0,0,60,
                     C3_point[1],C3_point[2],dis2points(C_point,F_point)/2)[2,])

    E_point<-as.numeric(BigStarPoint[5,])
    G_point<-
      getPointCrossLineAndCircle(C2_point[1],C2_point[2],0,0,C2_point[1],
                                 C2_point[2],E_point[1],E_point[2],'right')
    C4_point<-c((G_point[1]+E_point[1])/2,(G_point[2]+E_point[2])/2)
    C7_point<-
      as.numeric(circle_intersect(0,0,60,
                                  C4_point[1],C4_point[2],dis2points(C4_point,E_point))[2,])
    C5_point<-C2_point
    J_point<-as.numeric(BigStarPoint[2,])
    H_point<-as.numeric(BigStarPoint[3,])
    I_point<-as.numeric(BigStarPoint[7,])
    C6_point<-getLineIntersection(H_point,I_point,J_point,c(J_point[2],0))$point

    C5StarPoint<-star_construction_point(
      C5_point[1],C5_point[2],
      0.35*dis2points(A_point,C5_point),
      getRoatingAngel(
        C5_point,
        as.numeric(
          star_construction_point(
          C5_point[1],C5_point[2],0.35*dis2points(A_point,C5_point),0)[3,]),
        c(C5_point[1]+0.35*dis2points(A_point,C5_point),0)
        )
      )

    # C7不在C6圆心C6C5半径的圆上
    # 实际这个右弧段下端点是两个圆的交点之一
    # 圆1：C4圆心EC4为半径
    # 圆2：C6圆心C6C5为半径
    C7_real_right_bottom<-as.numeric(
      circle_intersect(
        C4_point[1],C4_point[2],dis2points(C4_point,E_point),
        C6_point[1],C6_point[2],dis2points(C6_point,C5_point)
      )[2,])
    C7_real_left_bottom<-as.numeric(
      circle_intersect(
        0,0,60,
        C6_point[1],C6_point[2],(dis2points(C6_point,C5_point)+1)
      )[2,])
    C7Arc_right_top<-as.numeric(
      circle_intersect(
        C6_point[1],C6_point[2],dis2points(C6_point,C5_point),
        C5_point[1],C5_point[2],0.35*dis2points(A_point,C5_point)
      )[2,]
    )
    C7Arc_left_top<-as.numeric(
      circle_intersect(
        C6_point[1],C6_point[2],(dis2points(C6_point,C5_point)+1),
        C5_point[1],C5_point[2],0.35*dis2points(A_point,C5_point)
      )[2,]
    )
    # 到这里所有的点到齐了
    #总结点
    df_points<-data.frame(
      x=c(A_point[1],B_point[1],C_point[1],D_point[1],E_point[1],F_point[1],G_point[1],
          H_point[1],I_point[1],J_point[1],P_point[1],
          C1_point[1],C2_point[1],C3_point[1],C4_point[1],
          C5_point[1],C6_point[1],C7_point[1],
          C7_real_right_bottom[1],C7_real_left_bottom[1],
          C7Arc_left_top[1],C7Arc_right_top[1]),
      y=c(A_point[2],B_point[2],C_point[2],D_point[2],E_point[2],F_point[2],G_point[2],
          H_point[2],I_point[2],J_point[2],P_point[2],
          C1_point[2],C2_point[2],C3_point[2],C4_point[2],
          C5_point[2],C6_point[2],C7_point[2],
          C7_real_right_bottom[2],C7_real_left_bottom[2],
          C7Arc_left_top[2],C7Arc_right_top[2]),
      label=c('A','B','C','D','E','F',
              'G','H','I','J','P','C1',
              'C2','C3','C4','C5','C6','C7',
              'C7_real_right_bottom','C7_real_left_bottom',
              'C7Arc_left_top','C7Arc_right_top')
    )
    #自定义小函数：根据点名称取坐标
    getLocsByDF<-function(label){
      selectDF<-df_points[which(df_points$label==label),][,c(1,2)]
      return(c(
        as.numeric(selectDF)[1],
        as.numeric(selectDF)[2]
      ))
    }


    ## --- 计算基准角：把 C7->P 摆到正上方所需的顺时针度数 ---
    rotationCenter <- getLocsByDF('C7')
    P_now          <- getLocsByDF('P')
    nowAngle <- atan2(P_now[2] - rotationCenter[2],
                      P_now[1] - rotationCenter[1]) * 180/pi   # ≈ 106.64°
    PPreDefaultAngle <- nowAngle - 90                          # ≈ 16.64°，顺时针为正
    totalAngle <- PPreDefaultAngle + RotationAngle             # 单位：度

    ## --- 平移量（旋转中心不动，直接用原始 C7）---
    MoveVolumes <- c(CenterLoc[1] - rotationCenter[1],
                     CenterLoc[2] - rotationCenter[2])

    ## --- 变换 df_points（rotate_point 向量化，无需循环）---
    tmp <- rotate_point(df_points$x, df_points$y,
                        rotationCenter[1], rotationCenter[2], totalAngle)
    df_points$x <- tmp$x + MoveVolumes[1]
    df_points$y <- tmp$y + MoveVolumes[2]

    ## --- 同样变换 C5StarPoint（关键！）---
    tmp <- rotate_point(C5StarPoint$x, C5StarPoint$y,
                        rotationCenter[1], rotationCenter[2], totalAngle)
    C5StarPoint <- data.frame(x = tmp$x + MoveVolumes[1],
                              y = tmp$y + MoveVolumes[2])

    ## --- 弧采样工具 ---
    # 沿以 O 为圆心的圆，从 from点 到 to点、且经过 via点 的那一段弧
    arc_via <- function(O, from, to, via, n = 400) {
      ang <- function(p) atan2(p[2] - O[2], p[1] - O[1])
      a0 <- ang(from); a1 <- ang(to); av <- ang(via)
      sweep_ccw <- (a1 - a0) %% (2*pi)
      via_ccw   <- (av - a0) %% (2*pi)
      if (via_ccw <= sweep_ccw)                      # 逆时针能覆盖 via
        angs <- a0 + seq(0, sweep_ccw,        length.out = n)
      else                                           # 否则顺时针
        angs <- a0 - seq(0, 2*pi - sweep_ccw, length.out = n)
      r <- sqrt(sum((from - O)^2))
      cbind(x = O[1] + r*cos(angs), y = O[2] + r*sin(angs))
    }
    # 沿圆从 from 到 to 的劣弧（较短一段）
    arc_minor <- function(O, from, to, n = 400) {
      ang <- function(p) atan2(p[2] - O[2], p[1] - O[1])
      a0 <- ang(from); a1 <- ang(to)
      d <- (a1 - a0) %% (2*pi)
      angs <- if (d <= pi) a0 + seq(0, d, length.out = n)
      else         a0 - seq(0, 2*pi - d, length.out = n)
      r <- sqrt(sum((from - O)^2))
      cbind(x = O[1] + r*cos(angs), y = O[2] + r*sin(angs))
    }

    ## --- 按边界顺序拼接四段弧 ---
    edge_section1 <- arc_via  (getLocsByDF('C1'), getLocsByDF('P'),  getLocsByDF('C7_real_left_bottom'), getLocsByDF('A'))
    edge_section2 <- arc_minor(getLocsByDF('C6'), getLocsByDF('C7_real_left_bottom'), getLocsByDF('C7Arc_left_top'))
    edge_section3 <- data.frame(x=c(getLocsByDF('C7Arc_left_top')[1],getLocsByDF('C7Arc_right_top')[1]),
                                y=c(getLocsByDF('C7Arc_left_top')[2],getLocsByDF('C7Arc_right_top')[2]))
    edge_section4 <- arc_minor(getLocsByDF('C6'), getLocsByDF('C7_real_right_bottom'), getLocsByDF('C7Arc_right_top'))
    edge_section5 <- arc_minor(getLocsByDF('C4'), getLocsByDF('C7_real_right_bottom'), getLocsByDF('G'))
    edge_section6 <- arc_via  (getLocsByDF('C2'), getLocsByDF('G'),  getLocsByDF('F'), getLocsByDF('C1'))
    edge_section7 <- arc_minor(getLocsByDF('C3'), getLocsByDF('F'),  getLocsByDF('P'))
    edge_poly_df <- as.data.frame(rbind(edge_section1, edge_section2,
                                        edge_section3, edge_section4,
                                        edge_section5, edge_section6,
                                        edge_section7))
    #输出
    BigestCircleR<-dis2points(getLocsByDF('C7'),getLocsByDF('P'))
    p<-
    list(
      #白色花瓣
      ggplot2::geom_polygon(data=edge_poly_df,
                   mapping=ggplot2::aes(x, y),
                   fill="white",colour=NA),
      #红色五角星
      ggplot2::geom_polygon(data=C5StarPoint,
                   mapping=ggplot2::aes(x, y),
                   fill="red",colour=NA))
    return(list(p=p,r=BigestCircleR))
  }

  #背景旗帜长度
  RinPlot<-singlePetalPlot(RotationAngle = 14+0/5*360)$r
  FlagWidthLength<-15*(RinPlot)/3
  FlagHeightLength<-10*(RinPlot)/3
  #组合绘图
  TerminalOutputPlot<-
    ggplot2::ggplot()+
    ggplot2::geom_rect(ggplot2::aes(xmin = -(FlagWidthLength/2), xmax = FlagWidthLength/2,
                  ymin = -(FlagHeightLength/2), ymax = FlagHeightLength/2),
              fill = "red") +
    singlePetalPlot(RotationAngle = 14+0/5*360)$p+
    singlePetalPlot(RotationAngle = 14+1/5*360)$p+
    singlePetalPlot(RotationAngle = 14+2/5*360)$p+
    singlePetalPlot(RotationAngle = 14+3/5*360)$p+
    singlePetalPlot(RotationAngle = 14+4/5*360)$p+
    ggplot2::coord_fixed()+
    ggplot2::scale_x_continuous(limits = c(-(FlagWidthLength/2),FlagWidthLength/2))+
    ggplot2::scale_y_continuous(limits = c(-(FlagHeightLength/2),FlagHeightLength/2))+
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
  return(TerminalOutputPlot)
}



#' Draw the Regional Flag of the Macao Special Administrative Region
#'
#' Constructs the flag of the Macao Special Administrative Region of the
#' People's Republic of China entirely from analytic geometry and renders it as
#' a \pkg{ggplot2} object. The emblem is assembled from three components, each
#' derived from the specification by computing every construction point (circle
#' intersections, line intersections, and rotation angles): a white lotus flower
#' of three petals, a cluster of five yellow five-pointed stars, and a stylised
#' white bridge with water below. The central petal is constructed explicitly
#' and the two lateral petals follow by mirroring across the vertical axis; the
#' star cluster is positioned by rotating a base location about the lotus anchor
#' point.
#'
#' @details
#' The geometry follows the Chinese national standard GB 17654-1999. The lotus
#' is bounded by a sequence of circular arcs drawn on numerous distinct
#' construction circles (centres \code{O2}-\code{O12}, radii such as 649/2,
#' 260/2, and 763/2); each arc is sampled into a dense polyline (400 points by
#' default) so that \code{\link[ggplot2]{geom_polygon}} can fill the enclosed
#' region. At this sampling density the chordal deviation from the true arc is
#' several orders of magnitude below any practical display or print resolution.
#'
#' The five stars comprise one larger central star flanked by two pairs of
#' smaller stars, all seated on an arc of fixed radius about the emblem anchor
#' \code{O1} at angular offsets of \eqn{\pm 35^{\circ}} and
#' \eqn{\pm 65^{\circ}}. The bridge and water are rendered as five white bands
#' beneath the lotus, each band bounded above and below by arcs of the field's
#' large construction circles and mirrored across the vertical axis.
#'
#' All construction is performed in a local coordinate system anchored near the
#' emblem base (\code{O1} centred at \eqn{(0, 38)}). The green field is a fixed
#' rectangle spanning \eqn{\pm 1440} horizontally and \eqn{\pm 960} vertically,
#' giving a 3:2 width-to-height ratio. The field colour is derived from the spot
#' colour Pantone 355 C (hex \code{#007A4D}); the lotus and bridge are white and
#' the stars are yellow (\code{#ffff00}). The function is self-contained: it
#' defines its own geometric helpers internally and depends only on
#' \pkg{ggplot2} for rendering and \pkg{showtext} for CJK glyph support.
#'
#' @param label Logical scalar. If \code{TRUE} (the default), the plot is
#'   annotated with a title and axis captions identifying the standard and
#'   drawing reference. If \code{FALSE}, all text annotations are suppressed,
#'   yielding a bare flag suitable for embedding or export.
#'
#' @return A \code{ggplot} object. The object can be printed, further modified
#'   with additional \pkg{ggplot2} layers, or written to disk with
#'   \code{\link[ggplot2]{ggsave}}.
#'
#' @section Rendering notes:
#' Correct display of the Chinese title and caption requires a font providing
#' CJK glyphs. The function calls \code{\link[showtext]{showtext_auto}}, but the
#' user must register a suitable font beforehand, for example via
#' \code{\link[sysfonts]{font_add}} or \code{\link[sysfonts]{font_add_google}}.
#' Without a registered CJK font the non-ASCII labels may render as blank boxes.
#'
#' The plot uses \code{\link[ggplot2]{coord_fixed}} to enforce a 1:1 aspect
#' ratio; altering the coordinate system will distort the circular arcs.
#'
#' @references
#' GB 17654-1999. \emph{Regional flag of the Macao Special Administrative
#' Region of the People's Republic of China.} Standardization Administration of
#' China.
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{ggsave}},
#'   \code{\link{plot_HK_SAR_flag}}
#'
#' @examples
#' \donttest{
#' # Register a CJK font before plotting annotated output
#' if (requireNamespace("sysfonts", quietly = TRUE)) {
#'   sysfonts::font_add_google("Noto Sans SC", "notosans")
#'   showtext::showtext_auto()
#' }
#'
#' # Annotated flag
#' plot_Macao_SAR_flag()
#'
#' # Bare flag, no text
#' plot_Macao_SAR_flag(label = FALSE)
#'
#' }
#'
#' @export
plot_Macao_SAR_flag<-function(label=TRUE){

  # ------------------------------------------------------------
  # 标题和坐标轴标签
  # ------------------------------------------------------------
  if(label==TRUE){
    labels<-list(x='\u53c2\u8003\uff1aGB 17654-1999',  # 参考：GB 17654-1999
                 y='Regional flag of Macao Special Administrative Region',
                 title='\u6fb3\u95e8\u7279\u522b\u884c\u653f\u533a\u533a\u65d7')  # 澳门特别行政区区旗
  }else{
    labels<-list(x='',
                 y='',
                 title='')
  }

  #定义需要的函数
  {
    # ------------------------------------------------------------
    # 函数1：求旋转后点的坐标
    # x1, y1 : 被旋转点的坐标
    # x0, y0 : 旋转中心点坐标
    # x2, y2 : 旋转后点的坐标
    # angle  : 旋转角度数
    # 点A(x1,y1)绕着点O(x0,y0)顺时针旋转angle度角（angle的范围为实数）后为点B(x2,y2)
    # ------------------------------------------------------------
    rotate_point <- function(x1, y1, x0, y0, angle){
      a  <- angle * pi / 180           # 角度转弧度
      dx <- x1 - x0
      dy <- y1 - y0
      x2 <- x0 + dx * cos(a) + dy * sin(a)
      y2 <- y0 - dx * sin(a) + dy * cos(a)
      return(list(x=x2,y=y2))
    }
    # ------------------------------------------------------------
    # 函数2：计算两条由两点确定的直线的交点
    # (x1,y1)-(x2,y2) 与 (x3,y3)-(x4,y4)
    # ------------------------------------------------------------
    line_line_point <- function(x1,y1,x2,y2,x3,y3,x4,y4, tol = 1e-10){
      denom <- (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
      if (abs(denom) < tol) {
        warning("No Cross Point!")
        return(c(NA, NA))
      }
      d12 <- x1*y2 - y1*x2
      d34 <- x3*y4 - y3*x4
      x0 <- (d12*(x3 - x4) - (x1 - x2)*d34) / denom
      y0 <- (d12*(y3 - y4) - (y1 - y2)*d34) / denom
      c(x0, y0)
    }
    # ------------------------------------------------------------
    # 函数3：构造五角星的 10 个顶点（指向顶点在内）
    # 这个函数源自于绘制五星红旗时，旋转中心不是星星中心而是(0,0)
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
      result<-data.frame(
        x=c(x1,x6,x2,x7,x3,x8,x4,x9,x5,x10),
        y=c(y1,y6,y2,y7,y3,y8,y4,y9,y5,y10))
      result
    }
    # ------------------------------------------------------------
    # 函数4：根据圆心和圆上两点计算对应圆心夹角度数的函数
    # ------------------------------------------------------------
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
    # ------------------------------------------------------------
    # 函数5：圆弧与直线交点坐标
    # 以A点(xp,yp)为圆心，B点(x1,y1)为圆弧起点画圆，与直线CD：C(x2,y2),
    # D(x3,y3)交与E点(x4,y4)和F(x5,y5)。side为X轴方向上靠左还是靠右。
    # ------------------------------------------------------------
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
    # ------------------------------------------------------------
    # 函数6：计算两点之间距离
    # ------------------------------------------------------------
    dis2points<-function(p1,p2){
      return(
        sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)
      )
    }
    # ------------------------------------------------------------
    # 函数7：计算直线 AB 与直线 CD 的交点
    # ------------------------------------------------------------
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
    # ------------------------------------------------------------
    # 函数8：计算两圆交点
    # 圆1圆心O1(x01,y01)半径r1，圆2圆心O2(x02,y02)半径r2
    # tol 参数用来吸收浮点误差,尤其是相切时 r1^2 - a^2 理论上为 0 但计算可能得到极小负数
    # 代码里已经把负数截断为 0,避免 sqrt 产生 NaN。
    # ------------------------------------------------------------
    circle_intersect <- function(x01, y01, r1, x02, y02, r2, tol = 1e-10){
      #两圆点坐标距离
      dx <- abs(x02 - x01)
      dy <- abs(y02 - y01)
      d  <- sqrt(dx^2 + dy^2) #圆心连线长度

      # 特殊情况判断
      if (d < tol && abs(r1 - r2) < tol) {
        warning("The two circles coincide; there are infinitely many intersection points") # 两圆重合，交点有无穷多个
        return(NULL)
      }else if (d > r1 + r2 + tol) {
        warning("The two circles are separate; no intersection") # 两圆相离，无交点
        return(NULL)
      }else if (d < abs(r1 - r2) - tol) {
        warning("One circle lies inside the other; no intersection") # 一圆在另一圆内部，无交点
        return(NULL)
      }else if(d == abs(r1 - r2)){ # 两圆相切时交点为两圆心
        cross1_point1<-getPointCrossLineAndCircle(x01,y01,x01,y01-r1,x01,y01,x02,y02,'left')
        cross1_point2<-getPointCrossLineAndCircle(x01,y01,x01,y01-r1,x01,y01,x02,y02,'right')
        cross2_point1<-getPointCrossLineAndCircle(x02,y02,x02,y02-r2,x01,y01,x02,y02,'left')
        cross2_point2<-getPointCrossLineAndCircle(x02,y02,x02,y02-r2,x01,y01,x02,y02,'right')
        crosspoint_df<-
          data.frame(
            x=c(cross1_point1[1],cross1_point2[1],cross2_point1[1],cross2_point2[1]),
            y=c(cross1_point1[2],cross1_point2[2],cross2_point1[2],cross2_point2[2])
          )
        outputDF<-crosspoint_df[duplicated(crosspoint_df),]
        return(list(x=outputDF$x,y=outputDF$y))
      }else{
        # 两连线斜率
        k_o<-(y02-y01)/(x02-x01)

        # a: 从 O1 沿连心线到弦中点的距离
        a <- (d^2 + r1^2 - r2^2) / (2 * d)
        h2 <- r1^2 - a^2
        h  <- if (h2 < 0) 0 else sqrt(h2)   # 防止相切时浮点误差导致负数

        # 弦中点坐标
        if(x01<=x02){
          xm <- x01 + a * dx / d
        }else if(x01>x02){
          xm <- x01 - a * dx / d
        }
        if(y01<=y02){
          ym <- y01 + a * dy / d
        }else if(y01>y02){
          ym <- y01 - a * dy / d
        }
        # 两个交点（相切时两点重合）
        if(k_o>=0){
          x_1 <- xm + h * dy / d
          y_1 <- ym - h * dx / d
          x_2 <- xm - h * dy / d
          y_2 <- ym + h * dx / d
        }else if(k_o<0){
          x_1 <- xm + h * dy / d
          y_1 <- ym + h * dx / d
          x_2 <- xm - h * dy / d
          y_2 <- ym - h * dx / d
        }
        return(data.frame(x=c(x_1,x_2),y=c(y_1,y_2)))
      }
    }
    # ------------------------------------------------------------
    # 函数9：计算点旋转的角度（小于180度的那个角）
    # 旋转中心坐标O(x0,y0)，旋转前坐标A(x1,y1)，旋转后坐标B(x2,y2)
    # ------------------------------------------------------------
    getRoatingAngel<-function(O,A,B,tol = 1e-10){
      #计算弦长
      stringLength<-dis2points(A,B)
      #计算半径
      rA<-dis2points(O,A)
      rB<-dis2points(O,B)
      if(abs(rA - rB) > tol * max(rA, rB)){
        warning('OA is not equal OB!')
        stop()
      }else if(rA==rB){
        r<-(rA+rB)/2
      }
      #计算半弦对应的半旋转角度
      halfAngel<-asin((stringLength/2)/r)
      Angel<-halfAngel*2
      #输出
      return(Angel)
    }

    # ------------------------------------------------------------
    # 函数10-11：弧采样工具
    # ------------------------------------------------------------
    # 函数10:沿以 O 为圆心的圆，从 from点 到 to点、且经过 via点 的那一段弧
    arc_via <- function(O, from, to, via, n = 400) {
      ang <- function(p) atan2(p[2] - O[2], p[1] - O[1])
      a0 <- ang(from); a1 <- ang(to); av <- ang(via)
      sweep_ccw <- (a1 - a0) %% (2*pi)
      via_ccw   <- (av - a0) %% (2*pi)
      if (via_ccw <= sweep_ccw)                      # 逆时针能覆盖 via
        angs <- a0 + seq(0, sweep_ccw,        length.out = n)
      else                                           # 否则顺时针
        angs <- a0 - seq(0, 2*pi - sweep_ccw, length.out = n)
      r <- sqrt(sum((from - O)^2))
      cbind(x = O[1] + r*cos(angs), y = O[2] + r*sin(angs))
    }
    # 函数11:沿圆从 from 到 to 的劣弧（较短一段）
    arc_minor <- function(O, from, to, n = 400) {
      ang <- function(p) atan2(p[2] - O[2], p[1] - O[1])
      a0 <- ang(from); a1 <- ang(to)
      d <- (a1 - a0) %% (2*pi)
      angs <- if (d <= pi) a0 + seq(0, d, length.out = n)
      else         a0 - seq(0, 2*pi - d, length.out = n)
      r <- sqrt(sum((from - O)^2))
      cbind(x = O[1] + r*cos(angs), y = O[2] + r*sin(angs))
    }

    # ------------------------------------------------------------
    # 函数12-13：求圆上点坐标的函数
    # center: 圆心坐标，例如 c(0, 0)
    # radius: 半径，正数
    # ------------------------------------------------------------
    #函数12:求纵坐标
    circle_getY <- function(center, radius, x) {
      ox <- center[1]
      oy <- center[2]

      delta <- radius^2 - (x - ox)^2

      # 处理浮点误差导致的微小负数
      if (delta < 0 && delta > -1e-12) {
        delta <- 0
      }

      if (delta < 0) {
        warning("Point (", x, ", y) is not on the circle: x-coordinate out of range.")
        return(NA_real_)
      }

      sqrt_delta <- sqrt(delta)
      y1 <- oy + sqrt_delta
      y2 <- oy - sqrt_delta

      return(c(y1, y2))
    }
    #函数13:求横坐标
    circle_getX <- function(center, radius, y) {
      ox <- center[1]
      oy <- center[2]

      delta <- radius^2 - (y - oy)^2

      # 处理浮点误差导致的微小负数
      if (delta < 0 && delta > -1e-12) {
        delta <- 0
      }

      if (delta < 0) {
        warning("Point (x, ", y, ") is not on the circle: y-coordinate out of range.")
        return(NA_real_)
      }

      sqrt_delta <- sqrt(delta)
      x1 <- ox + sqrt_delta
      x2 <- ox - sqrt_delta

      return(c(x1, x2))
    }


    # ------------------------------------------------------------
    # 函数14：构造五角星的 10 个顶点
    # x0, y0 : 星星中心坐标
    # r      : 外接圆半径
    # w      : 整体旋转角度（弧度）
    # 先以(0,0)构建0度旋转的五角星，然后以(0,0)为中心旋转，最后平移
    # ------------------------------------------------------------
    star_construction_point2<-function(x0,y0,r,w)#中心坐标，半径，旋转角度
    {
      onefifth2pi=2*pi/5 # 五角星相邻顶点夹角
      # 外圈五个顶点坐标
      x1=r*sin(w)
      y1=r*cos(w)
      x2=r*sin((w+onefifth2pi))
      x3=r*sin((w+2*onefifth2pi))
      x4=r*sin((w+3*onefifth2pi))
      x5=r*sin((w+4*onefifth2pi))
      y2=r*cos((w+onefifth2pi))
      y3=r*cos((w+2*onefifth2pi))
      y4=r*cos((w+3*onefifth2pi))
      y5=r*cos((w+4*onefifth2pi))
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
      for (i in 1:10) {
        temp_x<-get(paste0('x',i))
        temp_y<-get(paste0('y',i))
        rotated<-rotate_point(x0=0,y0=0,x1=temp_x,y1=temp_y,angle=w)
        assign(paste0('x',i),as.numeric(rotated$x)+x0)
        assign(paste0('y',i),as.numeric(rotated$y)+y0)
      }
      # 返回顺序排列的 10 个点（用于 geom_polygon）
      result<-data.frame(
        x=c(x1,x6,x2,x7,x3,x8,x4,x9,x5,x10),
        y=c(y1,y6,y2,y7,y3,y8,y4,y9,y5,y10))
      result
    }

  }

  # ------------------------------------------------------------
  # 绘图：背景
  # ------------------------------------------------------------
  # 区旗对应的专色 Pantone 355 C 作为基准，并衍生出用于屏幕的 RGB/十六进制色值。
  FlagBG<-list(ggplot2::geom_rect(ggplot2::aes(xmin=-1440,xmax=1440,ymin=-960,ymax=960),
                                  color=NULL,fill='#007A4D'))

  # ------------------------------------------------------------
  # 绘图函数：莲花瓣
  # ------------------------------------------------------------
  # 中央花瓣
  PetalCenter<-function(){
    #一些数据
    O1_point<-c(0,38);r0<-1152/2
    O2_point<-c(212,100);O3_point<-c(199,118);O4_point<-c(158,149);O5_point<-c(0,346)
    r1<-649/2;r2<-260/2
    #X轴左半一些关键点
    ##圆O5截断圆O3和O4的交点
    cross_o4o5<-
      as.numeric(circle_intersect(O5_point[1],O5_point[2],r2,
                                  O4_point[1],O4_point[2],r1)[2,])
    cross_o3o5<-
      as.numeric(circle_intersect(O5_point[1],O5_point[2],r2,
                                  O3_point[1],O3_point[2],r1)[2,])
    #官方说明里，花瓣下面的点没有说明，很模糊，所以这里采用的方案是：
    #最外侧上半段是圆O4，圆O4和圆O3交点后取O3到X轴
    cross_o3o4<-
      as.numeric(circle_intersect(O3_point[1],O3_point[2],r1,
                                  O4_point[1],O4_point[2],r1)[2,])
    ##圆O2与圆O3交点
    cross_o2o3<-
      as.numeric(circle_intersect(O3_point[1],O3_point[2],r1,
                                  O2_point[1],O2_point[2],r1)[2,])
    #X轴右半镜像
    cross_o4o5_mirror<-c(-cross_o4o5[1],cross_o4o5[2])
    cross_o3o5_mirror<-c(-cross_o3o5[1],cross_o3o5[2])
    cross_o3o4_mirror<-c(-cross_o3o4[1],cross_o3o4[2])
    cross_o2o3_mirror<-c(-cross_o2o3[1],cross_o2o3[2])
    O2_point_mirror<-c(-O2_point[1],O2_point[2])
    O3_point_mirror<-c(-O3_point[1],O3_point[2])
    O4_point_mirror<-c(-O4_point[1],O4_point[2])
    O5_point_mirror<-c(-O5_point[1],O5_point[2])

    #弧采样
    Arc_points<-rbind(
      #左半边
      arc_minor(O2_point,
                from = c(0,circle_getY(O2_point,r1,0)[1]),
                to = cross_o2o3),
      arc_minor(O3_point,from = cross_o2o3,to = cross_o3o5),
      arc_minor(O5_point,from = cross_o3o5,to = cross_o4o5),
      arc_minor(O4_point, from = cross_o4o5, to = cross_o3o4),
      arc_minor(O3_point, from = cross_o3o4,
                to = c(0,circle_getY(O3_point,r1,0)[2])),
      #右半边
      arc_minor(O3_point_mirror,
                from = c(0,circle_getY(O3_point,r1,0)[2]),
                to = cross_o3o4_mirror),
      arc_minor(O4_point_mirror,
                from = cross_o3o4_mirror,
                to = cross_o4o5_mirror),
      arc_minor(O5_point_mirror,
                from = cross_o4o5_mirror,
                to = cross_o3o5_mirror),
      arc_minor(O3_point_mirror,
                from = cross_o3o5_mirror,
                to = cross_o2o3_mirror),
      arc_minor(O2_point_mirror,
                from = cross_o2o3_mirror,
                to = c(0,circle_getY(O2_point_mirror,r1,0)[1]))
    )
    Plot<-
      list(
        ggplot2::geom_polygon(data=Arc_points,
                              mapping=ggplot2::aes(x, y),
                              fill="white",colour=NA)
      )
    PointDF<-as.data.frame(rbind(O2_point,O3_point,O4_point,O5_point,
                   cross_o2o3,cross_o3o4,cross_o3o5,cross_o4o5))
    colnames(PointDF)<-c('x','y')
    PointDF$point<-rownames(PointDF)
    return(
      list(
        Plot=Plot,
        PointDF=PointDF,
        Arc_points=Arc_points
      )
    )
  }
  # 左侧花瓣
  PetalLeft<-function(){
    PreviousDF<-PetalCenter()$PointDF
    # 上半部分点、下半部分图案
    O6_point<-c(-182,96);O7_point<-c(-213,91);O8_point<-c(-214,64);O12_point<-c(-485,-21)
    r3<-649/2;r5<-260/2
    cross_o8o12<-
      as.numeric(circle_intersect(O12_point[1],O12_point[2],r5,
                                  O8_point[1],O8_point[2],r3)[1,])
    cross_o7o12<-
      as.numeric(circle_intersect(O12_point[1],O12_point[2],r5,
                                  O7_point[1],O7_point[2],r3)[1,])
    cross_o7o8<-
      as.numeric(circle_intersect(O7_point[1],O7_point[2],r3,
                                  O8_point[1],O8_point[2],r3)[1,])
    cross_o6o7<-
      as.numeric(circle_intersect(O7_point[1],O7_point[2],r3,
                                  O6_point[1],O6_point[2],r3)[1,])
    # 下半部分点、上半部分图案
    O9_point<-c(-339,-372);O10_point<-c(-370,-343);O11_point<-c(-379,-307)
    r4<-763/2
    cross_o11o12<-
      as.numeric(circle_intersect(O12_point[1],O12_point[2],r5,
                                  O11_point[1],O11_point[2],r4)[1,])
    cross_o10o12<-
      as.numeric(circle_intersect(O12_point[1],O12_point[2],r5,
                                  O10_point[1],O10_point[2],r4)[1,])
    cross_o10o11<-
      as.numeric(circle_intersect(O10_point[1],O10_point[2],r4,
                                  O11_point[1],O11_point[2],r4)[1,])
    cross_o9o10<-
      as.numeric(circle_intersect(O10_point[1],O10_point[2],r4,
                                  O9_point[1],O9_point[2],r4)[1,])

    cross_o4o11<-as.numeric(
      circle_intersect(O11_point[1],O11_point[2],r4,
                       PreviousDF[which(PreviousDF$point=='O4_point'),]$x,
                       PreviousDF[which(PreviousDF$point=='O4_point'),]$y,
                       r3)[2,])

    #弧采样
    Arc_points<-rbind(
      #起点是中间花瓣和左边花瓣的交接，这里没明写到底去哪里作为左边花瓣的右端，
      #所以先选圆O8与X轴交点，也就是左边花瓣底端的弧与X轴交点，
      #做直线与曲线作为起点。如果有空出来部分，到时候在多加一个图层覆盖填充
      data.frame(
        x=c(0,cross_o4o11[1]),
        y=c(circle_getY(O8_point,r3,0)[2],
            cross_o4o11[2])),
      #上半边图案
      arc_minor(O11_point,
                from = cross_o4o11,
                to = cross_o11o12),
      arc_minor(O12_point,from = cross_o11o12,to = cross_o10o12),
      arc_minor(O10_point,from = cross_o10o12,to = cross_o9o10),
      arc_minor(O9_point, from = cross_o9o10, to = O12_point),
      #下半边图案
      arc_minor(O6_point,
                from = O12_point,
                to = cross_o6o7),
      arc_minor(O7_point,
                from = cross_o6o7,
                to = cross_o7o12),
      arc_minor(O12_point,
                from = cross_o7o12,
                to = cross_o8o12),
      arc_minor(O8_point,
                from = cross_o8o12,
                to = c(0,circle_getY(O8_point,r3,0)[2]))
    )
    Plot<-
      list(
        ggplot2::geom_polygon(data=Arc_points,
                              mapping=ggplot2::aes(x, y),
                              fill="white",colour=NA)
      )
    PointDF1<-as.data.frame(rbind(O6_point,O7_point,O8_point,O9_point,O10_point,
                                  O11_point,O12_point,
                                  cross_o6o7,cross_o7o8,cross_o8o12,cross_o7o12,
                                  cross_o9o10,cross_o10o11,cross_o11o12,cross_o10o12,
                                  cross_o4o11))
    colnames(PointDF1)<-c('x','y')
    PointDF1$point<-rownames(PointDF1)
    return(
      list(
        Plot=Plot,
        PointDF=PointDF1,
        Arc_points=Arc_points,
        ToFill=data.frame(
          x=c(0,cross_o4o11[1],(-cross_o4o11[1]),0),
          y=c(circle_getY(O8_point,r3,0)[2],
              cross_o4o11[2],
              cross_o4o11[2],
              circle_getY(O8_point,r3,0)[2]))
      )
    )
  }
  # 右侧花瓣：根据左侧花瓣镜像
  PetalRight<-function(){
    Arc_points<-PetalLeft()$Arc_points
    Plot<-
      list(
        ggplot2::geom_polygon(data=Arc_points,
                              mapping=ggplot2::aes(-x, y),
                              fill="white",colour=NA)
      )
    return(
      list(
        Plot=Plot,
        PointDF=NULL
      )
    )
  }
  # 填充图层
  PetalFill<-function(){
    Arc_points<-PetalLeft()$ToFill
    Plot<-
      list(
        ggplot2::geom_polygon(data=Arc_points,
                              mapping=ggplot2::aes(x, y),
                              fill="white",colour=NA)
      )
    return(
      list(
        Plot=Plot,
        PointDF=NULL
      )
    )
  }

  # ------------------------------------------------------------
  # 绘图函数：五星
  # ------------------------------------------------------------
  FiveStars<-function(){
    #一些数据
    O1_point<-c(0,38)
    r6<-909/2;r7<-211/2;r8<-146/2
    alpha1<-70;alpha2<-130;w1<-alpha1/2;w2<-alpha2/2

    #五角星中心
    StarsCenter_Big<-c(0,r6)
    StarsCenter_Left1<-
      as.numeric(unlist(rotate_point(x0=O1_point[1],y0=O1_point[2],
                                     x1=0,y1=r6,
                                     angle=-w1)))
    StarsCenter_Left2<-
      as.numeric(unlist(rotate_point(x0=O1_point[1],y0=O1_point[2],
                                     x1=0,y1=r6,
                                     angle=-w2)))
    StarsCenter_Right1<-
      as.numeric(unlist(rotate_point(x0=O1_point[1],y0=O1_point[2],
                                     x1=0,y1=r6,
                                     angle=w1)))
    StarsCenter_Right2<-
      as.numeric(unlist(rotate_point(x0=O1_point[1],y0=O1_point[2],
                                     x1=0,y1=r6,
                                     angle=w2)))

    #计算顶点
    Stars_Big<-
      star_construction_point2(x0=StarsCenter_Big[1],y0=StarsCenter_Big[2],
                               r=r7,w=0)
    Stars_Left1<-
      star_construction_point2(x0=StarsCenter_Left1[1],y0=StarsCenter_Left1[2],
                               r=r8,w=-w1)
    Stars_Left2<-
      star_construction_point2(x0=StarsCenter_Left2[1],y0=StarsCenter_Left2[2],
                               r=r8,w=-w2)
    Stars_Right1<-
      star_construction_point2(x0=StarsCenter_Right1[1],y0=StarsCenter_Right1[2],
                               r=r8,w=w1)
    Stars_Right2<-
      star_construction_point2(x0=StarsCenter_Right2[1],y0=StarsCenter_Right2[2],
                               r=r8,w=w2)

    #构造图层
    '#ffff00'
    Stars_layer<-
      list(
        ggplot2::geom_polygon(data=Stars_Big,
                              mapping=ggplot2::aes(x, y),
                              fill='#ffff00',colour=NULL),
        ggplot2::geom_polygon(data=Stars_Left1,
                              mapping=ggplot2::aes(x, y),
                              fill='#ffff00',colour=NULL),
        ggplot2::geom_polygon(data=Stars_Left2,
                              mapping=ggplot2::aes(x, y),
                              fill='#ffff00',colour=NULL),
        ggplot2::geom_polygon(data=Stars_Right1,
                              mapping=ggplot2::aes(x, y),
                              fill='#ffff00',colour=NULL),
        ggplot2::geom_polygon(data=Stars_Right2,
                              mapping=ggplot2::aes(x, y),
                              fill='#ffff00',colour=NULL)
      )

    #输出
    return(Stars_layer)
  }

  # ------------------------------------------------------------
  # 绘图函数：桥梁和海浪
  # ------------------------------------------------------------
  BridgeAndOcean<-function(){
    #一些数据
    PointDF2<-rbind(
      PetalCenter()$PointDF,
      PetalLeft()$PointDF
    )
    O1_point<-c(0,38);r0<-1152/2
    O12_point2<-c(-224,39);O13_point<-c(-209,7)
    r3<-649/2;r9<-649/2
    Dis_A1<-25;Dis_A2<-33;Dis_A3<-33;Dis_A4<-25;Dis_A5<-16
    Dis_B1<-32;Dis_B2<-16;Dis_B3<-25;Dis_B4<-32;Dis_B5<-40
    #A1的顶端线是切于莲花底端，也就是圆O8
    #所以第一条白线的上部直线是A1的y轴坐标是
    B1_line_y1<-PointDF2$y[which(PointDF2$point=='O8_point')]-r3-Dis_A1
    B1_line_y2<-B1_line_y1-Dis_B1
    #第二条白线
    B2_line_y1<-B1_line_y2-Dis_A2
    B2_line_y2<-B2_line_y1-Dis_B2
    #第三条白线
    B3_line_y1<-B2_line_y2-Dis_A3
    B3_line_y2<-B3_line_y1-Dis_B3
    #第四条白线
    B4_line_y1<-B3_line_y2-Dis_A4
    B4_line_y2<-B4_line_y1-Dis_B4
    #第五条白线（白色区域最低端）
    B5_line_y1<-B4_line_y2-Dis_A5
    B5_line_y2<-B5_line_y1-Dis_B5

    #圆O12和x轴交点
    cross_Xo12_2<-c(0,circle_getY(O12_point2,r9,x = 0)[2])
    cross_Xo13<-c(0,circle_getY(O13_point,r9,x = 0)[2])

    #圆O1和多个白线的左侧纵坐标
    cross_o1B1_1<-c(circle_getX(O1_point,r0,B1_line_y1)[2],B1_line_y1)
    cross_o1B1_2<-c(circle_getX(O1_point,r0,B1_line_y2)[2],B1_line_y2)
    cross_o1B2_1<-c(circle_getX(O1_point,r0,B2_line_y1)[2],B2_line_y1)
    cross_o1B2_2<-c(circle_getX(O1_point,r0,B2_line_y2)[2],B2_line_y2)
    cross_o1B3_1<-c(circle_getX(O1_point,r0,B3_line_y1)[2],B3_line_y1)
    cross_o1B3_2<-c(circle_getX(O1_point,r0,B3_line_y2)[2],B3_line_y2)
    cross_o1B4_1<-c(circle_getX(O1_point,r0,B4_line_y1)[2],B4_line_y1)
    cross_o1B4_2<-c(circle_getX(O1_point,r0,B4_line_y2)[2],B4_line_y2)
    cross_o1B5_1<-c(circle_getX(O1_point,r0,B5_line_y1)[2],B5_line_y1)
    cross_o1B5_2<-c(circle_getX(O1_point,r0,B5_line_y2)[2],B5_line_y2)


    #构造图层数据框
    B1_DF<-rbind(
      data.frame(x=c(cross_o1B1_1[1],O12_point2[1]),
                 y=c(cross_o1B1_1[2],O12_point2[2]-r9)),
      arc_minor(O12_point2,
                from = c(O12_point2[1],O12_point2[2]-r9),
                to = cross_Xo12_2),
      data.frame(x=c(cross_Xo12_2[1],cross_Xo13[1]),
                 y=c(cross_Xo12_2[2],cross_Xo13[2])),
      arc_minor(O13_point,
                from = cross_Xo13,
                to = c(O13_point[1],O13_point[2]-r9)),
      data.frame(x=c(O13_point[1],cross_o1B1_2[1]),
                 y=c(O13_point[2]-r9,cross_o1B1_2[2])),
      arc_minor(O1_point,
                from = cross_o1B1_2,
                to = cross_o1B1_1)
    )
    B2_DF<-rbind(
      data.frame(x=c(cross_o1B2_1[1],0,0,cross_o1B2_2[1]),
                 y=c(cross_o1B2_1[2],cross_o1B2_1[2],cross_o1B2_2[2],cross_o1B2_2[2])),
      arc_minor(O1_point,
                from = cross_o1B2_2,
                to = cross_o1B2_1)
    )
    B3_DF<-rbind(
      data.frame(x=c(cross_o1B3_1[1],0,0,cross_o1B3_2[1]),
                 y=c(cross_o1B3_1[2],cross_o1B3_1[2],cross_o1B3_2[2],cross_o1B3_2[2])),
      arc_minor(O1_point,
                from = cross_o1B3_2,
                to = cross_o1B3_1)
    )
    B4_DF<-rbind(
      data.frame(x=c(cross_o1B4_1[1],0,0,cross_o1B4_2[1]),
                 y=c(cross_o1B4_1[2],cross_o1B4_1[2],cross_o1B4_2[2],cross_o1B4_2[2])),
      arc_minor(O1_point,
                from = cross_o1B4_2,
                to = cross_o1B4_1)
    )
    B5_DF<-rbind(
      data.frame(x=c(cross_o1B5_1[1],0,0,cross_o1B5_2[1]),
                 y=c(cross_o1B5_1[2],cross_o1B5_1[2],cross_o1B5_2[2],cross_o1B5_2[2])),
      arc_minor(O1_point,
                from = cross_o1B5_2,
                to = cross_o1B5_1)
    )
    #构造图层
    BridgeAndOcean_Layer<-list(
      #左侧
      ggplot2::geom_polygon(data=B1_DF,
                            mapping=ggplot2::aes(x, y),
                            fill='white',colour=NULL),
      ggplot2::geom_polygon(data=B2_DF,
                            mapping=ggplot2::aes(x, y),
                            fill='white',colour=NULL),
      ggplot2::geom_polygon(data=B3_DF,
                            mapping=ggplot2::aes(x, y),
                            fill='white',colour=NULL),
      ggplot2::geom_polygon(data=B4_DF,
                            mapping=ggplot2::aes(x, y),
                            fill='white',colour=NULL),
      ggplot2::geom_polygon(data=B5_DF,
                            mapping=ggplot2::aes(x, y),
                            fill='white',colour=NULL),
      #右侧
      ggplot2::geom_polygon(data=B1_DF,
                            mapping=ggplot2::aes(-x, y),
                            fill='white',colour=NULL),
      ggplot2::geom_polygon(data=B2_DF,
                            mapping=ggplot2::aes(-x, y),
                            fill='white',colour=NULL),
      ggplot2::geom_polygon(data=B3_DF,
                            mapping=ggplot2::aes(-x, y),
                            fill='white',colour=NULL),
      ggplot2::geom_polygon(data=B4_DF,
                            mapping=ggplot2::aes(-x, y),
                            fill='white',colour=NULL),
      ggplot2::geom_polygon(data=B5_DF,
                            mapping=ggplot2::aes(-x, y),
                            fill='white',colour=NULL)
    )
    #输出
    return(BridgeAndOcean_Layer)
  }


  # ------------------------------------------------------------
  # 组合绘图
  # ------------------------------------------------------------
  TerminalOutputPlot<-
  ggplot2::ggplot()+
    FlagBG +
    PetalCenter()$Plot+
    PetalLeft()$Plot+
    PetalRight()$Plot+
    PetalFill()$Plot+
    FiveStars()+
    BridgeAndOcean()+
    ggplot2::coord_fixed()+
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
  return(TerminalOutputPlot)
}
