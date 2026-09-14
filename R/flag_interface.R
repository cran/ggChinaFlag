#' Built-in Chinese and English names of the flags
#'
#' Provides a standardised list of flag names used by \code{\link{plotCNFlag}}
#' and related functions. It contains the names of the national flag of the
#' People's Republic of China, several historical flags of the Republic of
#' China, the party flags of the Communist Party of China and the Kuomintang,
#' the regional flags of the Hong Kong and Macao Special Administrative
#' Regions, the flag of the Communist Youth League of China, and the flag of
#' the People's Liberation Army and its service branches, in both Chinese and
#' English.
#'
#' @param lang Character string giving the language of the returned names.
#'   Either \code{"Chinese"} (default) or \code{"English"}.
#'
#' @return A \code{list} of named character vectors. With
#'   \code{lang = "Chinese"} the elements hold the Chinese national-flag,
#'   political-party, regional-flag, organisation, and military flag names;
#'   with \code{lang = "English"} they are named \code{National Flags},
#'   \code{Political Parties}, \code{Regional Flags}, \code{Organizations},
#'   and \code{Military}. The strings in each vector correspond one-to-one to
#'   the underlying plotting functions.
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
FlagStorage <- function(lang = c('Chinese', 'English')) {
  lang <- match.arg(lang)
  if (lang == 'Chinese') {
    return(list(
      "\u56fd\u65d7" = c(
        '\u4e2d\u534e\u4eba\u6c11\u5171\u548c\u56fd\u56fd\u65d7',
        '\u4e2d\u534e\u6c11\u56fd\u9752\u5929\u767d\u65e5\u65d7',
        '\u4e2d\u534e\u6c11\u56fd\u5317\u6d0b\u653f\u5e9c\u4e94\u8272\u65d7',
        '\u6b66\u660c\u8d77\u4e49\u94c1\u8840\u5341\u516b\u661f\u65d7'
      ),
      "\u653f\u515a" = c(
        '\u4e2d\u56fd\u5171\u4ea7\u515a\u515a\u65d7',
        '\u4e2d\u56fd\u56fd\u6c11\u515a\u515a\u65d7'
      ),
      "\u533a\u65d7" = c(
        '\u9999\u6e2f\u7279\u522b\u884c\u653f\u533a\u533a\u65d7',
        '\u6fb3\u95e8\u7279\u522b\u884c\u653f\u533a\u533a\u65d7'
      ),
      "\u7ec4\u7ec7" = c(
        '\u4e2d\u56fd\u5171\u4ea7\u4e3b\u4e49\u9752\u5e74\u56e2\u56e2\u65d7'
      ),
      "\u519b\u4e8b" = c(
        '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u519b\u65d7',
        '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u9646\u519b\u519b\u65d7',
        '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u6d77\u519b\u519b\u65d7',
        '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u7a7a\u519b\u519b\u65d7',
        '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u706b\u7bad\u519b\u519b\u65d7',
        '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u519b\u4e8b\u822a\u5929\u90e8\u961f\u519b\u65d7',
        '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u7f51\u7edc\u7a7a\u95f4\u90e8\u961f\u519b\u65d7',
        '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u4fe1\u606f\u652f\u63f4\u90e8\u961f\u519b\u65d7',
        '\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u8054\u52e4\u4fdd\u969c\u90e8\u961f\u519b\u65d7'
      )
    ))
  } else {
    return(list(
      `National Flags` = c(
        "Flag of the People's Republic of China",
        "Flag of the Republic of China (Blue Sky, White Sun, and Red Earth)",
        "Five-Color Flag of the Beiyang Government of the Republic of China",
        "Iron-Blood 18-Star Flag of the Wuchang Uprising"
      ),
      `Political Parties` = c(
        "Flag of the Communist Party of China",
        "Flag of the Kuomintang (Blue Sky and White Sun flag)"
      ),
      `Regional Flags` = c(
        "Regional Flag of the Hong Kong Special Administrative Region",
        "Regional Flag of the Macao Special Administrative Region"
      ),
      `Organizations` = c(
        "Flag of the Communist Youth League of China"
      ),
      `Military` = c(
        "General PLA Flag",
        "PLA Ground Force Flag",
        "PLA Navy Flag",
        "PLA Air Force Flag",
        "PLA Rocket Force Flag",
        "PLA Aerospace Force Flag",
        "PLA Cyberspace Force Flag",
        "PLA Information Support Force Flag",
        "PLA Joint Logistics Support Force Flag"
      )
    ))
  }
}


#' Plot a Chinese national, historical, party, regional, organisation, or
#' military flag by name
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
#' detects the input language, matches the name, and forwards to the
#' appropriate plotting function. Supported functions include
#' \code{\link{plot_P.R.CHINA_flag}}, \code{\link{plot_ROC_KMT_flag}},
#' \code{\link{plot_ROC_Beiyang_flag}}, \code{\link{plot_Han18Star}},
#' \code{\link{plot_CCP}}, \code{\link{plot_KMT}},
#' \code{\link{plot_HK_SAR_flag}}, \code{\link{plot_Macao_SAR_flag}},
#' \code{\link{plot_CYLC}}, and \code{\link{plot_PLA}} (for the general PLA
#' flag and its service branches). An unrecognised name raises an error.
#'
#' @examples
#' \donttest{
#' plotCNFlag("Flag of the People's Republic of China")
#' plotCNFlag("Flag of the Kuomintang (Blue Sky and White Sun flag)", label = FALSE)
#' plotCNFlag("Five-Color Flag of the Beiyang Government of the Republic of China")
#' plotCNFlag("Iron-Blood 18-Star Flag of the Wuchang Uprising")
#' plotCNFlag("Regional Flag of the Hong Kong Special Administrative Region")
#' plotCNFlag("Regional Flag of the Macao Special Administrative Region", label = FALSE)
#' plotCNFlag("\u4e2d\u56fd\u5171\u4ea7\u4e3b\u4e49\u9752\u5e74\u56e2\u56e2\u65d7")
#' plotCNFlag("\u4e2d\u56fd\u4eba\u6c11\u89e3\u653e\u519b\u6d77\u519b\u519b\u65d7")
#' plotCNFlag("PLA Rocket Force Flag", label = FALSE)
#' }
#'
#' @seealso \code{\link{FlagStorage}} for the name list, and the underlying
#'   plotting functions such as \code{\link{plot_P.R.CHINA_flag}},
#'   \code{\link{plot_CYLC}}, and \code{\link{plot_PLA}}.
#'
#' @export
plotCNFlag <- function(input, label = TRUE) {
  # 获取中英文名称列表
  cn_list <- FlagStorage(lang = "Chinese")
  en_list <- FlagStorage(lang = "English")
  # 展平为字符向量（保持顺序一致）
  names_cn <- unlist(cn_list, use.names = FALSE)
  names_en <- unlist(en_list, use.names = FALSE)
  # 尝试匹配中文或英文名称
  idx <- match(input, names_cn)
  if (is.na(idx)) {
    idx <- match(input, names_en)
  }
  if (is.na(idx)) {
    stop("\u8f93\u5165\u540d\u79f0\u65e0\u6cd5\u8bc6\u522b\uff0c\u8bf7\u4f7f\u7528\u5185\u7f6e\u6807\u51c6\u540d\u79f0\uff08\u4e2d\u82f1\u6587\u5747\u53ef\uff09\u3002")
  }
  # 绘图函数列表，顺序与名称列表完全对应
  # 共 4 + 2 + 2 + 1 + 9 = 18 个
  flag_funcs <- list(
    function(label) plot_P.R.CHINA_flag(label = label),      # 1
    function(label) plot_ROC_KMT_flag(label = label),        # 2
    function(label) plot_ROC_Beiyang_flag(label = label),    # 3
    function(label) plot_Han18Star(label = label),           # 4
    function(label) plot_CCP(label = label),                 # 5
    function(label) plot_KMT(label = label),                 # 6
    function(label) plot_HK_SAR_flag(label = label),         # 7
    function(label) plot_Macao_SAR_flag(label = label),      # 8
    function(label) plot_CYLC(label = label),                # 9
    function(label) plot_PLA(subtype = "general", label = label),  # 10
    function(label) plot_PLA(subtype = "\u9646\u519b", label = label),      # 11
    function(label) plot_PLA(subtype = "\u6d77\u519b", label = label),      # 12
    function(label) plot_PLA(subtype = "\u7a7a\u519b", label = label),      # 13
    function(label) plot_PLA(subtype = "\u706b\u7bad\u519b", label = label),    # 14
    function(label) plot_PLA(subtype = "\u519b\u4e8b\u822a\u5929\u90e8\u961f", label = label), # 15
    function(label) plot_PLA(subtype = "\u7f51\u7edc\u7a7a\u95f4\u90e8\u961f", label = label), # 16
    function(label) plot_PLA(subtype = "\u4fe1\u606f\u652f\u63f4\u90e8\u961f", label = label), # 17
    function(label) plot_PLA(subtype = "\u8054\u52e4\u4fdd\u969c\u90e8\u961f", label = label)  # 18
  )
  # 调用对应函数，传递 label 参数
  flag_funcs[[idx]](label = label)
}
