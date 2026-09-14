#' Plot the flag of the People's Liberation Army (PLA) or its service branches
#'
#' This function programmatically renders the flag of the People's Liberation
#' Army of China, as well as the flags of its various service branches and
#' branches, using analytic geometry and ggplot2-based vector graphics. The
#' symbols are constructed entirely from geometric primitives (rectangles,
#' polygons, and stars), without relying on any external image files.
#'
#' The function supports both the general PLA flag and a range of service
#' branch flags. The desired flag is selected via the \code{subtype} argument,
#' which accepts English names or abbreviations. The matching is performed
#' through a cleaned search table with fuzzy matching to tolerate common
#' prefixes such as "PLA" or "People's Liberation Army".
#'
#' @param subtype Character string specifying the service branch or flag type.
#'   Accepts English names (e.g., \code{"PLA Navy"}) or abbreviations (e.g.,
#'   \code{"Navy"}). Use \code{"general"} (default) for the general PLA flag.
#' @param label Logical value indicating whether to display textual annotations
#'   (title and axis labels). Default is \code{TRUE}.
#'
#' @return A \code{ggplot} object representing the requested PLA flag.
#'
#' @details
#' The geometric construction of the general PLA flag follows the design
#' published by the People's Revolutionary Military Commission on 15 June 1949.
#' The flag has a red background with a golden five-pointed star and the
#' characters "8-1" in the upper left corner. Service branch flags
#' retain the upper 5/8 of the general flag and replace the lower 3/8 with
#' branch-specific colors and patterns.
#'
#' Supported \code{subtype} values include:
#' \itemize{
#'   \item \code{"general"} -- general PLA flag
#'   \item \code{"PLA Ground Force"} / \code{"Groud"} -- Army
#'   \item \code{"PLA Navy"} / \code{"Navy"} -- Navy
#'   \item \code{"PLA Air Force"} / \code{"Air"} -- Air Force
#'   \item \code{"PLA Rocket Force"} / \code{"Rocket"} -- Rocket Force
#'   \item \code{"PLA Aerospace Force"} / \code{"Aerospace"} -- Aerospace Force
#'   \item \code{"PLA Cyberspace Force"} / \code{"Cyber"} -- Cyberspace Force
#'   \item \code{"PLA Information Support Force"} / \code{"Information"} -- Information Support Force
#'   \item \code{"PLA Joint Logistics Support Force"} / \code{"Support"} -- Joint Logistics Support Force
#' }
#'
#' @examples
#' \donttest{
#' plot_PLA()                          # general PLA flag
#' plot_PLA("Navy")                    # PLA Navy flag
#' plot_PLA("PLA Air Force")           # PLA Air Force flag
#' plot_PLA("Rocket")                  # PLA Rocket Force flag
#' plot_PLA(subtype = "Support", label = FALSE)
#' }
#'
#' @author Per the regulations on the flag and emblem of the People's Liberation Army.
#'
#' @seealso \code{\link{plot_CCP}} for the CCP emblem/flag plotting interface,
#'   \code{\link{plot_CYLC}} for the CYLC flag plotting interface.
#'
#' @references
#' People's Revolutionary Military Commission. Order on the Flag and Emblem of
#' the Chinese People's Liberation Army. People's Daily, 1949-06-15(1).
#'
#' @export
plot_PLA<-function(subtype='general',label=TRUE){

  #子类绘图函数
  plot_PLA_general<-function(label=TRUE){
    # ------------------------------------------------------------
    # 设计参考：
    # 中国人民革命军事委员会. 中国人民革命军事委员会发布命令 公布中国人民解放军军旗及军徽样式[N]. 人民日报, 1949-06-15(1).
    # https://cn.govopendata.com/renminribao/1949/06/15/1/#25347
    # https://www.saac.gov.cn/daj/1949nda/202006/1365199f12b24efcaa5f7d3748dc58b9.shtml
    # ------------------------------------------------------------

    # ------------------------------------------------------------
    # 标题和坐标轴标签
    # ------------------------------------------------------------
    if(label==TRUE){
      labels <- list(
        x = '\u8bbe\u8ba1\u6765\u6e90\uff1a\u4e2d\u56fd\u4eba\u6c11\u9769\u547d\u519b\u4e8b\u59d4\u5458\u4f1a',  # 设计来源：中国人民革命军事委员会
        y = '(1921-)',  # (1921-)
        title = '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u519b\u65d7'  # 中国人民解放军军旗
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
      ggplot2::geom_rect(mapping = ggplot2::aes(xmin=(-20),xmax=20,ymin=(-16),ymax=16),
                         fill=std_cols$red,color=NA)

    #图层Layer1：金色五角星
    # 绘图数据
    star0_x<-c(star_construction_point(x0 = (-14), y0 = (8), r = 4, w = 0)[1:10])
    star0_y<-c(star_construction_point(x0 = (-14), y0 = (8), r = 4, w = 0)[11:20])
    star0<-data.frame(star0_x,star0_y)#构建数据框
    p_star_layer1<-
      ggplot2::geom_polygon(data = star0,ggplot2::aes(x=star0_x,y=star0_y),
                            color=NA,
                            fill=std_cols$gold)
    #图层Layer2：“八”字
    left_edge_x<-max(star0$star0_x) #最右角横坐标为“八”字小方块最左端
    # “八”字左半边是一个宽1长3的矩形且：
    # 1.其倾斜角度使其在y轴的投影高度为3；
    # 2.过矩形中心和短边中点且平行于长边的直线斜率为正数；
    # 3.left_edge_x是这个矩形最左边点的横坐标；
    # 4.矩形在y轴投影的范围是[4,7]。
    # 求得坐标为
    Point_BA_left <- data.frame( #从最左边点开始顺时针
      x = c(left_edge_x,
            left_edge_x + 9/5,
            left_edge_x + 13/5,
            left_edge_x + 4/5),
      y = c(23/5,
            7,
            32/5,
            4)
    )
    # “八”字右半边是一个宽1长3的矩形且：
    # 1.其倾斜角度使其在y轴的投影高度为3；
    # 2.过矩形中心和短边中点且平行于长边的直线斜率为负数；
    # 3.-4.5是这个矩形最右边点的横坐标；
    # 4.矩形在y轴投影的范围是[4,7]。
    Point_BA_right <- data.frame( #顺时针从最右点开始排列
      x = c(-9/2, -63/10, -71/10, -53/10),
      y = c(23/5, 7, 32/5, 4)
    )
    #验证两部分间距是不是约为0.5
    # min(Point_BA_right$x)-max(Point_BA_left$x)
    # [1] 0.4957739
    p_BA<-list(
      ggplot2::geom_polygon(data = Point_BA_left,
                            ggplot2::aes(x=x,y=y),
                            color=std_cols$gold,
                            fill=std_cols$gold),
      ggplot2::geom_polygon(data = Point_BA_right,
                            ggplot2::aes(x=x,y=y),
                            color=std_cols$gold,
                            fill=std_cols$gold)
    )
    #图层Layer3：“一”字
    p_YI<-list(
      ggplot2::geom_polygon(
        data = data.frame(
          x=c(0,-4,-4,0),
          y=c(6,6,5,5)
        ),
        ggplot2::aes(x=x,y=y),
        color=std_cols$gold,
        fill=std_cols$gold)
    )

    p_PLA_geneal<-
      p_bg_layer0+ #背景图层
      p_star_layer1+ #星星图层
      p_BA+p_YI+ #“八一”字图层
      ggplot2::coord_quickmap()+#调整为1：1比例显示
      ggplot2::theme(legend.key = ggplot2::element_blank(),
                     panel.grid.major=ggplot2::element_line(color=NA),
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
    return(p_PLA_geneal)
  }
  plot_PLA_Navy<-function(label=TRUE){
    # ------------------------------------------------------------
    # 标题和坐标轴标签
    # ------------------------------------------------------------
    if(label==TRUE){
      labels <- list(
        x = "People's Liberation Army Navy",  # 横轴：中国人民解放军海军
        y = '(1949-)',                         # 纵轴：(1949-)
        title = '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u6d77\u519b\u519b\u65d7'  # 标题：中国人民解放军海军军旗
      )
    } else {
      labels <- list(
        x = '',
        y = '',
        title = ''
      )
    }
    p_subtype<-list(
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-4),ymin=(-16)),
                         fill='#000080',color=NA),
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-11.2),ymin=(-13.6)),
                         fill='white',color=NA),
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-6.4),ymin=(-8.8)),
                         fill='white',color=NA)
    )
    p_combined<-
      plot_PLA_general(label=label)+p_subtype+
      ggplot2::labs(x=labels$x,
                    y=labels$y,
                    title=labels$title)
    return(p_combined)
  }
  plot_PLA_Groud<-function(label=TRUE){
    # ------------------------------------------------------------
    # 标题和坐标轴标签
    # ------------------------------------------------------------
    if(label==TRUE){
      labels <- list(
        x = "People's Liberation Army Ground Force",  # 横轴：中国人民解放军陆军
        y = '(1927-)',                         # 纵轴：(1927-)
        title = '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u9646\u519b\u519b\u65d7'  # 标题：中国人民解放军陆军军旗
      )
    } else {
      labels <- list(
        x = '',
        y = '',
        title = ''
      )
    }
    p_subtype<-ggplot2::geom_rect(
      ggplot2::aes(xmin=(-20),xmax=20,ymin=(-16),ymax=(-4)),fill="#39B54A"
    )
    p_combined<-
      plot_PLA_general(label=label)+p_subtype+
      ggplot2::labs(x=labels$x,
                    y=labels$y,
                    title=labels$title)
    return(p_combined)
  }
  plot_PLA_Air<-function(label=TRUE){
    # ------------------------------------------------------------
    # 标题和坐标轴标签
    # ------------------------------------------------------------
    if(label==TRUE){
      labels <- list(
        x = "People's Liberation Army Air Force",  # 横轴：中国人民解放军空军
        y = '(1949-)',                         # 纵轴：(1949-)
        title = '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u7a7a\u519b\u519b\u65d7'  # 标题：中国人民解放军空军军旗
      )
    } else {
      labels <- list(
        x = '',
        y = '',
        title = ''
      )
    }
    p_subtype<-ggplot2::geom_rect(
      ggplot2::aes(xmin=(-20),xmax=20,ymin=(-16),ymax=(-4)),fill="#87CEEB",color=NA
    )
    p_combined<-
      plot_PLA_general(label=label)+p_subtype+
      ggplot2::labs(x=labels$x,
                    y=labels$y,
                    title=labels$title)
    return(p_combined)
  }
  plot_PLA_Rocket<-function(label=TRUE){
    # ------------------------------------------------------------
    # 标题和坐标轴标签
    # ------------------------------------------------------------
    if(label==TRUE){
      labels <- list(
        x = "People's Liberation Army Rocket Force",  # 横轴：中国人民解放军空军
        y = '(2015-)',                         # 纵轴：(2015-)
        title = '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u706b\u7bad\u519b\u519b\u65d7'  # 标题：中国人民解放军火箭军军旗
      )
    } else {
      labels <- list(
        x = '',
        y = '',
        title = ''
      )
    }
    p_subtype<-ggplot2::geom_rect(
      ggplot2::aes(xmin=(-20),xmax=20,ymin=(-16),ymax=(-4)),fill="#F2A826",color=NA
    )
    p_combined<-
      plot_PLA_general(label=label)+p_subtype+
      ggplot2::labs(x=labels$x,
                    y=labels$y,
                    title=labels$title)
    return(p_combined)
  }
  plot_PLA_Aerospace<-function(label=TRUE){
    # ------------------------------------------------------------
    # 标题和坐标轴标签
    # ------------------------------------------------------------
    if(label==TRUE){
      labels <- list(
        x = "People's Liberation Army Aerospace Force",
        y = '(2015-)',                         # 纵轴：(2015-)
        title = '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u519b\u4e8b\u822a\u5929\u90e8\u961f\u519b\u65d7'  # 标题：中国人民解放军军事航天部队军旗
      )
    } else {
      labels <- list(
        x = '',
        y = '',
        title = ''
      )
    }
    p_subtype<-list(
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-4),ymin=(-16)),
                         fill='#1D3C7B',color=NA),#底色
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-10.5),ymin=(-12)),
                         fill='#ffff00',color=NA),
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-7.5),ymin=(-9)),
                         fill='#ffff00',color=NA)
    )
    p_combined<-
      plot_PLA_general(label=label)+p_subtype+
      ggplot2::labs(x=labels$x,
                    y=labels$y,
                    title=labels$title)
    return(p_combined)
  }
  plot_PLA_Cyberspace<-function(label=TRUE){
    # ------------------------------------------------------------
    # 标题和坐标轴标签
    # ------------------------------------------------------------
    if(label==TRUE){
      labels <- list(
        x = "People's Liberation Army Cyberspace Force",
        y = '(2024-)',                         # 纵轴：(2024-)
        title = '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u7f51\u7edc\u7a7a\u95f4\u90e8\u961f\u519b\u65d7'  # 标题：中国人民解放军网络空间部队军旗
      )
    } else {
      labels <- list(
        x = '',
        y = '',
        title = ''
      )
    }
    p_subtype<-list(
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-4),ymin=(-16)),
                         fill='#65676B',color=NA),#底色
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-10.5),ymin=(-12)),
                         fill='#ffff00',color=NA),
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-7.5),ymin=(-9)),
                         fill='#ffff00',color=NA)
    )
    p_combined<-
      plot_PLA_general(label=label)+p_subtype+
      ggplot2::labs(x=labels$x,
                    y=labels$y,
                    title=labels$title)
    return(p_combined)
  }
  plot_PLA_Information<-function(label=TRUE){
    # ------------------------------------------------------------
    # 标题和坐标轴标签
    # ------------------------------------------------------------
    if(label==TRUE){
      labels <- list(
        x = "People's Liberation Army Information Support Force",
        y = '(2024-)',                         # 纵轴：(2024-)
        title = '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u4fe1\u606f\u652f\u63f4\u90e8\u961f\u519b\u65d7'  # 标题：中国人民解放军信息支援部队军旗
      )
    } else {
      labels <- list(
        x = '',
        y = '',
        title = ''
      )
    }
    p_subtype<-list(
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-4),ymin=(-16)),
                         fill='#640872',color=NA),#底色
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-10.5),ymin=(-12)),
                         fill='#ffff00',color=NA),
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-7.5),ymin=(-9)),
                         fill='#ffff00',color=NA)
    )
    p_combined<-
      plot_PLA_general(label=label)+p_subtype+
      ggplot2::labs(x=labels$x,
                    y=labels$y,
                    title=labels$title)
    return(p_combined)
  }
  plot_PLA_Support<-function(label=TRUE){
    # ------------------------------------------------------------
    # 标题和坐标轴标签
    # ------------------------------------------------------------
    if(label==TRUE){
      labels <- list(
        x = "People's Liberation Army Joint Logistics Support Force",
        y = '(2016-)',                         # 纵轴
        title = '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u8054\u52e4\u4fdd\u969c\u90e8\u961f\u519b\u65d7'  # 标题：中国人民解放军联勤保障部队军旗
      )
    } else {
      labels <- list(
        x = '',
        y = '',
        title = ''
      )
    }
    p_subtype<-list(
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-4),ymin=(-16)),
                         fill='#296E40',color=NA),#底色
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-10.5),ymin=(-12)),
                         fill='#ffff00',color=NA),
      ggplot2::geom_rect(ggplot2::aes(xmin=(-20),xmax=20,ymax=(-7.5),ymin=(-9)),
                         fill='#ffff00',color=NA)
    )
    p_combined<-
      plot_PLA_general(label=label)+p_subtype+
      ggplot2::labs(x=labels$x,
                    y=labels$y,
                    title=labels$title)
    return(p_combined)
  }

  #检索数据基础数据框
  search_table <- data.frame(
    Chinese = c("\u9646\u519b", "\u6d77\u519b",
                "\u7a7a\u519b", "\u706b\u7bad\u519b", "\u519b\u4e8b\u822a\u5929\u90e8\u961f", "\u7f51\u7edc\u7a7a\u95f4\u90e8\u961f",
                "\u4fe1\u606f\u652f\u63f4\u90e8\u961f", "\u8054\u52e4\u4fdd\u969c\u90e8\u961f"),
    English = c("PLA Ground Force", "PLA Navy (PLAN)",
                "PLA Air Force", "PLA Rocket Force",
                "PLA Aerospace Force", "PLA Cyberspace Force",
                "PLA Information Support Force",
                "PLA Joint Logistics Support Force"),
    Abbr = c('Groud', 'Navy',
             'Air','Rocket','Aerospace','Cyber','Information','Support'),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # 如果 label 缺失，默认 TRUE
  if (missing(label)) label <- TRUE

  # 清洗函数：去除“中国人民解放军”、“解放军”、“PLA”等前缀，并去除空格标点
  clean_text <- function(x) {
    if (is.null(x)) return("")
    x <- as.character(x)
    x <- tolower(x)
    # 移除中文常见前缀
    x <- gsub("\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b", "", x)
    x <- gsub("\u89e3\u653e\u519b", "", x)
    x <- gsub("\u4e2d\u56fd", "", x)
    # 移除英文常见前缀
    x <- gsub("people's liberation army", "", x, ignore.case = TRUE)
    x <- gsub("pla", "", x, ignore.case = TRUE)
    x <- gsub("the", "", x, ignore.case = TRUE)
    # 去除空格、标点、括号
    x <- gsub("[[:space:][:punct:]]", "", x)
    x <- trimws(x)
    return(x)
  }

  # 清洗 search_table 的各列
  search_table$Chinese_clean <- sapply(search_table$Chinese, clean_text)
  search_table$English_clean <- sapply(search_table$English, clean_text)
  search_table$Abbr_clean <- sapply(search_table$Abbr, clean_text)
  # 合并所有清洗后的文本用于搜索
  search_table$all_clean <- paste(search_table$Chinese_clean,
                                  search_table$English_clean,
                                  search_table$Abbr_clean,
                                  sep = "|")

  # 清洗输入的 subtype
  subtype_clean <- clean_text(subtype)

  # 查找匹配
  matched_idx <- integer(0)
  if (nzchar(subtype_clean)) {
    # 先尝试精确包含匹配
    matched_idx <- which(grepl(subtype_clean, search_table$all_clean, fixed = TRUE))
    if (length(matched_idx) == 0) {
      # 尝试模糊匹配（允许编辑距离）
      matched_idx <- agrep(subtype_clean, search_table$all_clean,
                           ignore.case = TRUE, max.distance = 0.2)
    }
  }

  # 如果没找到，检查是否是 general 或空
  if (length(matched_idx) == 0) {
    if (tolower(subtype) %in% c("general", "general flag", "\u519b\u65d7", "")) {
      return(plot_PLA_general(label = label))
    } else {
      warning("\u672a\u627e\u5230\u5339\u914d\u7684 subtype: ", subtype, "\uff0c\u5c06\u4f7f\u7528\u9ed8\u8ba4 general \u519b\u65d7\u3002")
      return(plot_PLA_general(label = label))
    }
  }

  # 取第一个匹配
  idx <- matched_idx[1]
  abbr <- search_table$Abbr[idx]

  # 映射到对应的绘图函数
  func_map <- list(
    Groud = plot_PLA_Groud,
    Navy = plot_PLA_Navy,
    Air = plot_PLA_Air,
    Rocket = plot_PLA_Rocket,
    Aerospace = plot_PLA_Aerospace,
    Cyber = plot_PLA_Cyberspace,
    Information = plot_PLA_Information,
    Support = plot_PLA_Support
  )

  # 根据缩写调用对应的子函数
  if (abbr %in% names(func_map)) {
    return(func_map[[abbr]](label = label))
  } else {
    warning("\u672a\u8bc6\u522b\u7684\u519b\u79cd\u7f29\u5199: ", abbr, "\uff0c\u5c06\u4f7f\u7528\u9ed8\u8ba4 general \u519b\u65d7\u3002")
    return(plot_PLA_general(label = label))
  }

}
