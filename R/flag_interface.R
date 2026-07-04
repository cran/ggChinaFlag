#' Built-in Chinese and English names of the flags
#'
#' Provides a standardised list of flag names used by \code{\link{plotCNFlag}}
#' and related functions. It contains the names of the national flag of the
#' People's Republic of China, several historical flags of the Republic of
#' China, and the party flags of the Communist Party of China and the
#' Kuomintang, in both Chinese and English.
#'
#' @param lang Character string giving the language of the returned names.
#'   Either \code{"Chinese"} (default) or \code{"English"}.
#'
#' @return A \code{list} of two named character vectors. With
#'   \code{lang = "Chinese"} the elements hold the Chinese national-flag and
#'   political-party names; with \code{lang = "English"} they are named
#'   \code{National Flags} and \code{Political Parties}. The strings in each
#'   vector correspond one-to-one to the underlying plotting functions.
#'
#' @examples
#' # Chinese names
#' FlagStorage()
#'
#' # English names
#' FlagStorage(lang = "English")
#'
#' @seealso \code{\link{plotCNFlag}}, which matches against these names.
#'
#' @export
FlagStorage<-function(lang=c('Chinese','English')){
  lang <- match.arg(lang)
  if(lang=='Chinese'){
    return(
      list(
        "\u56fd\u65d7"=c('\u4e2d\u534e\u4eba\u6c11\u5171\u548c\u56fd\u56fd\u65d7','\u4e2d\u534e\u6c11\u56fd\u9752\u5929\u767d\u65e5\u65d7',  # 中华人民共和国国旗 / 中华民国青天白日旗
                         '\u4e2d\u534e\u6c11\u56fd\u5317\u6d0b\u653f\u5e9c\u4e94\u8272\u65d7','\u6b66\u660c\u8d77\u4e49\u94c1\u8840\u5341\u516b\u661f\u65d7'),  # 中华民国北洋政府五色旗 / 武昌起义铁血十八星旗
        "\u653f\u515a"=c('\u4e2d\u56fd\u5171\u4ea7\u515a\u515a\u65d7','\u4e2d\u56fd\u56fd\u6c11\u515a\u515a\u65d7')  # 中国共产党党旗 / 中国国民党党旗
      )
    )
  }else{
    return(
      list(
        `National Flags` = c(
          "Flag of the People's Republic of China",
          "Flag of the Republic of China (Blue Sky, White Sun, and Red Earth)",
          "Five-Color Flag of the Beiyang Government of the Republic of China",
          "Iron-Blood 18-Star Flag of the Wuchang Uprising"
        ),
        `Political Parties` = c(
          "Flag of the Communist Party of China",
          "Flag of the Kuomintang (Blue Sky and White Sun flag)"
        )
      )
    )
  }
}




#' Plot a Chinese national or historical flag by name
#'
#' A unified interface that dispatches to the appropriate low-level plotting
#' function based on the supplied flag name (Chinese or English) and returns a
#' \code{ggplot} object.
#'
#' @param input Character string giving the flag to plot, either a Chinese or
#'   an English name; see \code{\link{FlagStorage}} for the supported names.
#' @param label Logical; whether to display the title and text annotations.
#'   Default is \code{TRUE}.
#'
#' @return A \code{ggplot} object, which can be printed directly or saved with
#'   \code{ggsave()}.
#'
#' @details
#' The function obtains the built-in name list via \code{\link{FlagStorage}},
#' detects the input language, matches the name, and forwards to one of
#' \code{\link{plot_P.R.CHINA_flag}}, \code{\link{plot_ROC_KMT_flag}},
#' \code{\link{plot_ROC_Beiyang_flag}}, \code{\link{plot_Han18Star}},
#' \code{\link{plot_CCP}} or \code{\link{plot_KMT}}. An unrecognised name
#' raises an error.
#'
#' @examples
#' \donttest{
#' plotCNFlag("Flag of the People's Republic of China")
#' plotCNFlag("Flag of the Kuomintang (Blue Sky and White Sun flag)", label = FALSE)
#' plotCNFlag("Five-Color Flag of the Beiyang Government of the Republic of China")
#' plotCNFlag("Iron-Blood 18-Star Flag of the Wuchang Uprising")
#' }
#'
#' @seealso \code{\link{FlagStorage}} for the name list, and the underlying
#'   plotting functions such as \code{\link{plot_P.R.CHINA_flag}}.
#'
#' @export
plotCNFlag <- function(input, label = TRUE) {
  # 获取中英文名称列表
  cn_list <- FlagStorage(lang = "Chinese")
  en_list <- FlagStorage(lang = "English")

  # 拼接成扁平字符向量（保持顺序一致）
  names_cn <- c(cn_list[[1]], cn_list[[2]])
  names_en <- c(en_list$`National Flags`, en_list$`Political Parties`)

  # 尝试匹配中文或英文名称
  idx <- match(input, names_cn)
  if (is.na(idx)) {
    idx <- match(input, names_en)
  }
  if (is.na(idx)) {
    stop("\u8f93\u5165\u540d\u79f0\u65e0\u6cd5\u8bc6\u522b\uff0c\u8bf7\u4f7f\u7528\u5185\u7f6e\u6807\u51c6\u540d\u79f0\uff08\u4e2d\u82f1\u6587\u5747\u53ef\uff09\u3002")  # 输入名称无法识别，请使用内置标准名称（中英文均可）。
  }

  # 绘图函数列表，顺序与名称列表完全对应
  flag_funcs <- list(
    plot_P.R.CHINA_flag,   # 1. 中华人民共和国国旗
    plot_ROC_KMT_flag,     # 2. 中华民国青天白日满地红旗
    plot_ROC_Beiyang_flag, # 3. 北洋政府五色旗
    plot_Han18Star,        # 4. 武昌起义铁血十八星旗
    plot_CCP,              # 5. 中国共产党党旗
    plot_KMT               # 6. 中国国民党党旗
  )

  # 调用对应函数，传递 label 参数
  flag_funcs[[idx]](label = label)
}
