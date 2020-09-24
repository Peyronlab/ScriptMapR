#' @export
#'
#' @import formatR
#' @import RCy3
#' @import dplyr
#' @import stringr
#' @import utils
#' @import grDevices
#'
#' @title Display content of input script in 'Cytoscape'
#' @description
#' This function allows the user to represent the content of a given script in 'Cytoscape' (<https://cytoscape.org/>).
#' Therefore it requires to have a functioning version of 'Cytoscape' 3.6.1 or greater.
#'
#' Each variable is represented as a node and edges represent commands that call the variable.
#' Functions can also be represented.
#'
#' A color code is associated to each nodes:
#'
#' - white: intermediate variables (that are created and used to create new ones),
#'
#' - pink: subsets of a variable (ex: var2 in var1$var2),
#'
#' - green: final variables (that are created and not used afterwards),
#'
#' - red: warnings, message and stop functions,
#'
#' - yellow: if, else if, else and ifelse functions,
#'
#' - orange: for, foreach and while functions,
#'
#' - blue: *print, cat or other functions at beginning of line,
#'
#' - gray: packages import and session info (wd)
#'
#' Edge color reports the sequence of command on a blue scale (light: early in the script -to- dark: late in the script)
#' Node color code extend to edges in case of loops or tests (orange and yellow)
#'
#' Edges of type sinewave represent the connection between a node and itself when used as an indice.
#' Edges of type dots represent the commands within a if, else, else if, ifelse or loop condition.
#'
#' User created functions are represented and pooled as a group, collapsed and extracted to a subnetwork to avoid latency.
#'
#' @name scriptmapr
#' @rdname scriptmapr
#' @aliases scriptmapr
#' @usage
#' scriptmapr(path)
#' @param path path of the R file to plot in Cytoscape
#'
#' @examples
#' # load example script path
#' file.path <- system.file("extdata", "example.R", package = "ScriptMapR")
#'
#' scriptmapr(path=file.path)
#'
#' @return Cytoscape network visualization


scriptmapr = function(path) {
  path_tmp=path
  if (!file.exists(path)) {
    stop("File not found, please check current working directory")
  }


  chkpnt=readline(prompt=paste("File is going to be copied at ",paste(tempdir(),basename(path),sep='/'), " Continue? (Y/n)"))
  if (chkpnt%in%c("",'Y',"y","Yes","yes")){
    b=file.copy(path,tempdir())
    script_name=path
    path=paste(tempdir(),basename(path),sep='/')
  } else {
    message('Aborting')
    return(NULL)
  }

  if (b==TRUE){
    message('tmp file created at ',path)
  }

  log <- vector('character')
  con    <- textConnection('log', 'wr', local = TRUE)
  sink(con,type = 'message',append = FALSE)
  tidy_file(path, width.cutoff = 500)
  sink(type = 'message')
  close(con)

  # log.path=paste(tempdir(),"/test.log",sep="")
  # con <- file(log.path)
  # sink(con, append=TRUE)
  # sink(con, append=TRUE, type="message")
  # tidy_file(path, width.cutoff = 500)

  # log=readLines(log.path)
  if (sum(grepl("InLiNe_IdEnTiFiEr",log))>0){
    # cat(readLines(log.path), sep="\n")
    cat(log,sep = '\n')
    message("Tidying failed:")
    stop("inline comment detected, correct specified line and retry \n see https://yihui.org/formatr/#the-pipe-operator section6")
  }

  p_data = parse(path)

  text = unlist(strsplit(as.character(p_data), "\n"))
  check_err = lapply(seq_along(text), function(x) {
    if (nchar(text[x]) > 500) {
      stop("Some lines in the script are > 500 The script graph cannot be rendered ")
    }
  })

  #checking Cytoscape connection
  result = tryCatch({
    cytoscapePing()
  }, error = function(e) {
    message('You must open Cytoscape before running this function')
    return(1)

  })


  make_edge = function(target, attr = "", source, interaction = "interacts_with", weight = 1, cmd, id = NULL, extra = "", edge_lgth = 1, all_groups = NULL, edgtype = "SOLID", reverse.edg = FALSE, indices = "") {
    #function to create edges given specific arguments
    group_attr = ""
    if (length(id) > 1 || as.numeric(id) > 0) {
      # debug
      id = as.numeric(gsub("[.].*", "", id))
      group_attr = paste(unlist(groups[id]), collapse = ",")
    }
    id = as.character(id)
    data.frame(target = target, attr = attr, source = source, interaction = interaction, weight = weight, cmd = cmd, id = id, extra, edge_lgth = edge_lgth, group_attr = group_attr, edgtype = edgtype, reverse.edg = reverse.edg, indices = indices,stringsAsFactors = FALSE)

  }
  # parse script
  parsed = getParseData(p_data)
  ex=identical(path_tmp,system.file("extdata", "example.R", package = "ScriptMapR"))
  if (!is.null(result)){
    if (result==1){
      #quit function
      return(NULL)
    }
  }
  if (is.null(parsed) & ex==TRUE){
    message("CRANcheck fails to get an output from getParseData - parsing example file")
    parsed=readRDS(system.file("extdata", "parsed.RDS", package = "ScriptMapR"))

  }
  # removing empty lines
  parsed = parsed[-c(which(parsed$token %in% c("expr", "equal_assign", "forcond", "expr_or_assign_or_help"))), ]
  xy.list <- split(parsed, f = parsed$line1)

  # extracting comments could be used in tooltips
  xy.list.comments = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "COMMENT")]
  xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 7), `[[`, 1) == "COMMENT") == FALSE]


  # extracting packages imports
  xy.list.packages = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) == "library")]
  packages = as.character(lapply(lapply(xy.list.packages, `[[`, 9), `[[`, 3))
  xy.list = xy.list[as.logical(lapply(lapply(xy.list, `[[`, 9), `[[`, 1) == "library") == FALSE]
  # Checking for package::function
  matched.pkgs = as.character(p_data[grep("\\w+(?=[:]{2})", p_data, perl = TRUE)])
  if (length(matched.pkgs) > 0) {
    m = gregexpr("\\w+(?=[\\:]{2})", paste(p_data, collapse = ";"), perl = TRUE)
    extra = unique(unlist(regmatches(paste(p_data, collapse = ";"), m)))
    if (sum(!extra %in% packages)>0) {
      packages = unique(c(packages, extra))
    }
  }

  #indices of ( ) in the script
  openpar = lapply(seq_along(xy.list), FUN = function(x) {
    ct = 1
    ids = matrix(ncol = 2)
    for (i in seq_len(length(xy.list[[x]]$text))) {
      patt = xy.list[[x]]$text[i]
      if (patt == "(") {
        ids = rbind(ids, c(i, NA))
      } else if (patt == ")") {
        ids[tail(which(is.na(ids[, 2])), 1), 2] = i
      }
    }
    ids[-c(1), ]
  })


  #indices of [ ] in the script
  openbrk = lapply(seq_along(xy.list), FUN = function(x) {
    ct = 1
    ids = matrix(ncol = 2)
    for (i in seq_len(length(xy.list[[x]]$text))) {
      patt = xy.list[[x]]$text[i]
      if (patt == "[") {
        ids = rbind(ids, c(i, NA))
      } else if (patt == "]") {
        ids[tail(which(is.na(ids[, 2])), 1), 2] = i
      }
    }
    ids[-c(1), ]
  })



  #indices of {} in the script
  grp = ""
  c = 1
  groups = lapply(seq_along(xy.list), FUN = function(x) {
    # print(x)
    if (tail(xy.list[[x]]$token, 1) == "'{'" & xy.list[[x]]$token[1] != "'}'") {
      grp <<- c(grp, x)
      grp
    } else if (xy.list[[x]]$token[1] == "'}'" & length(xy.list[[x]]$token) == 1) {
      grp <<- grp[-c(length(grp))]
      grp
    }
    grp
  })

  groups = lapply(groups, "[", -1)
  length(unique(unlist(groups)))
  # add to group

  ### find and load packages function to check for pattern arguments
  #that might conflict with [ brackets detection
  #source https://stackoverflow.com/users/1017276/benjamin

  locate_function <- function(fun, find_in = "library") {
    find_in <- match.arg(arg = find_in, choices = c("searchpath", "library"))

    # Find all libraries that have a function of this name.
    h <- help.search(pattern = paste0("^", fun, "$"), agrep = FALSE)
    h <- h$matches[, "Package"]

    if (find_in == "library")
      return(h)

    # List packages in the search path
    sp <- search()
    sp <- sp[grepl("^package", sp)]
    sp <- sub("^package[:]", "", sp)

    # List the packages on the search path with a function named `fun` in the order they appear on the search path.
    h <- h[h %in% sp]
    h[order(match(h, sp, NULL))]
  }

  ############################ PACKAGE LOAD
  bool_pkg=lapply(seq_along(packages) ,FUN = function(x){
    res=tryCatch(require(packages[x], character.only = TRUE), error = function(e) {
      warning('Some packages required in the script are not installed yet')}
    )

  })

  packages = packages[unlist(bool_pkg)]

  R_func = lapply(seq_along(xy.list), FUN = function(x) {
    if ("SYMBOL_FUNCTION_CALL" %in% xy.list[[x]]$token) {
      xy.list[[x]]$text[which(xy.list[[x]]$token == "SYMBOL_FUNCTION_CALL")]
    }
  })

  all_func_names = unique(unlist(R_func))

  usr_func = lapply(seq_along(xy.list), FUN = function(x) {
    m = which(xy.list[[x]]$token == "FUNCTION")
    if (length(m) > 0) {
      if (m[1] == 3) {
        fun_name = paste("f(", xy.list[[x]]$text[1], ")", sep = "")
        paste(fun_name, xy.list[[x]]$text[which(xy.list[[x]]$token == "SYMBOL_FORMALS")], sep = ".")
      }
    }
  })
  all_func_names_tmp = all_func_names
  all_func_names = all_func_names[-c(which(unlist(lapply(R_func, "[", 1)) %in% gsub("f\\(|\\).*", "", unlist(lapply(usr_func, "[", 1)), perl = TRUE)))]

  symb_package = lapply(seq_along(xy.list), FUN = function(x) {
    if ("NS_GET" %in% xy.list[[x]]$token) {
      return(x)
    }
  })


  # to_add = lapply(seq_along(all_func_names), FUN = function(x) {
  #   rep = tryCatch({
  #     formals(all_func_names[x])
  #     NULL
  #   }, error = function(e) {
  #     pkgs = locate_function(all_func_names[x])[1]
  #     if (!(pkgs %in% packages) && length(pkgs) > 0) {
  #       require(pkgs, character.only = TRUE)
  #       pkgs
  #     }
  #   })
  #   rep
  # })
  #
  # if ((!is.null(unique(unlist(to_add))))) {
  #   packages = rbind(packages, unlist(to_add))
  # }

  all_func_names = all_func_names_tmp

  # regexp_found = lapply(seq_along(xy.list), FUN = function(x) {
  #   # checks all functions and trycatch accessing parameters
  #   if ("SYMBOL_FUNCTION_CALL" %in% xy.list[[x]]$token) {
  #     if (sum(xy.list[[x]]$text[which(xy.list[[x]]$token == "SYMBOL_FUNCTION_CALL")] %in% all_func_names) > 0) {
  #       function_to_check = unique(xy.list[[x]]$text[which(xy.list[[x]]$token == "SYMBOL_FUNCTION_CALL")])
  #       args = tryCatch({
  #         names(unlist(lapply(function_to_check, formals)))
  #       }, error = function(e) {
  #         message(paste("Function ", paste(function_to_check, collapse = " or "), " could not be found and has not been referenced in the script you privided.\nCould not check for arguments.", sep = ""))
  #         NULL
  #       })
  #       # if sucess, checks for patterns (spit and patterns) to be in 1st or 2nd parameters of function
  #       if ((!is.null(args))) {
  #         df.ls = lapply(function_to_check, formals)
  #         l1 <- lapply(seq_along(df.ls), function(x) {
  #           if ("pattern" %in% names(unlist(df.ls[[x]]))[seq_len(2)] | "split" %in% names(unlist(df.ls[[x]]))[seq_len(2)])
  #             return(TRUE)
  #         })
  #         if (sum(unlist(l1)) > 0 & "STR_CONST" %in% xy.list[[x]]$token) {
  #           return(TRUE)
  #         }
  #         # if true and str const as one of parameters then it might be a grep like function with patterns
  #       }
  #     }
  #   }
  #
  # })


  #seting up environment information, packages etc
  pkg_edges = NULL
  # packages = c(packages, unlist(to_add))
  if (length(packages) > 0) {
    l.packages = rep("library", length(packages))
    pkg_edges = rbind(make_edge(target = "Packages", attr = "", source = "Session", interaction = "env.setup", weight = 1, id = 0, cmd = "", all_groups = groups), make_edge(target = packages, attr = "", source = "Packages", interaction = "wd.init", weight = 1, id = 0, cmd = "library()", all_groups = groups))
    packages = cbind(l.packages, packages)
  }


  # extracts the names of all variables found in given line

  all_nodes = lapply(seq_along(xy.list), FUN = function(x) {
    xy.list[[x]]$text[which(xy.list[[x]]$token == "SYMBOL")]
  })


  # stop message in condition link to ifquery[max(groups[[x]])] $ condition ?

  #check for indices of for foreach and while commands
  opn = NULL
  for.id = lapply(seq_along(xy.list), FUN = function(x) {
    m = which(xy.list[[x]]$token %in% c("FOR", "WHILE"))
    n_tmp = which(xy.list[[x]]$token == "SYMBOL_FUNCTION_CALL")
    n = which(xy.list[[x]]$text[n_tmp] == "foreach")
    n = c(n, which(grepl("apply", xy.list[[x]]$text[n_tmp])))
    # print(x)
    if (length(groups[[x]]) == 0) {
      opn <<- NULL
    } else if (!is.null(opn) & length(groups[[x]]) > 0) {
      if (x > 1) {
        if (length(groups[[x - 1]]) > 0) {
          if (sum(grepl(pattern = tail(groups[[x - 1]], 1), groups[[x]])) == 0) {
            opn <<- NULL
          }
        } else {
          opn <<- NULL
        }
      }

    }
    if (length(m) > 0 | length(n) > 0) {
      opn <<- x
      return(opn)
    } else if (!is.null(opn)) {
      return(opn)
    }
  })


  #check for indices of else commands
  opn = NULL
  else.id = lapply(seq_along(xy.list), FUN = function(x) {
    m = which(xy.list[[x]]$token %in% c("ELSE"))
    n = which(xy.list[[x]]$token %in% c("IF"))
    if (length(groups[[x]]) == 0) {
      opn <<- NULL
    } else if (!is.null(opn) & length(groups[[x]]) > 0) {
      if (x > 1) {
        if (length(groups[[x - 1]]) > 0) {
          if (sum(grepl(pattern = tail(groups[[x - 1]], 1), groups[[x]])) == 0) {
            opn <<- NULL
          }
        } else {
          opn <<- NULL
        }
      }

    }
    if (length(m) > 0 & length(n) == 0) {
      opn <<- x
      return(x)
    } else if (!is.null(opn)) {
      return(x)
    }
  })


  #check for indices of elseif commands
  opn = NULL
  elseif.id = lapply(seq_along(xy.list), FUN = function(x) {
    if (!is.null(opn) & length(groups[[x]]) > 0) {
      if (x > 1) {
        if (length(groups[[x - 1]]) > 0) {
          if (sum(grepl(pattern = tail(groups[[x - 1]], 1), groups[[x]]) == 0) | !is.null(else.id[[x]])) {
            opn <<- NULL
          }
        } else {
          opn <<- NULL
        }
      }
    }
    m = which(xy.list[[x]]$token %in% c("IF"))
    if (length(m) > 0) {

      if (m[1] == 3) {
        opn <<- x
        return(x)
      }
    } else if (!is.null(opn)) {
      return(x)
    }

  })

  #check for indices of if commands
  opn = NULL
  if.id = lapply(seq_along(xy.list), FUN = function(x) {
    # print(x)
    if (!is.null(opn) & length(groups[[x]]) > 0) {
      if (sum(grepl(pattern = as.character(opn), groups[[x]]) == 0) | !is.null(else.id[[x]]) | !is.null(elseif.id[[x]])) {
        opn <<- NULL
      }
    }
    m = which(xy.list[[x]]$token %in% c("IF"))
    if (length(m) > 0) {

      if (m[1] %in% seq(1)) {
        opn <<- x
        return(x)
      }
    } else if (!is.null(opn)) {
      return(x)
    }
  })

  # extract brackets [,1], [1,2], [1, var] double check maybe/ check if pattern is in function argument
  #return edges
  brackets = lapply(seq_along(xy.list), FUN = function(x) {
    str = 1.5
    n = which(xy.list[[x]]$token == "'['")
    imb = FALSE
    if (length(n) > 1) {
      i1 = gregexpr("\\[", paste(xy.list[[x]]$text, collapse = ""))
      i2 = gregexpr("\\]", paste(xy.list[[x]]$text, collapse = ""))
      # print(x)
      final = as.numeric(sort(c(i1[[1]], i2[[1]])))
      if (!identical(final, as.numeric(strsplit(paste(sort(i1[[1]]), sort(i2[[1]]), sep = ",", collapse = ","), split = ",")[[1]]))) {
        imb = TRUE
      }
    }

    id_rm=which(xy.list[[x]]$text=="in")
    if( length(id_rm)>0){xy.list[[x]]$text[id_rm]="="}
    patt = str_detect(paste(xy.list[[x]]$text, collapse = ""), "\\[")
    text = paste(xy.list[[x]]$text, collapse = "")

    if ("LBB" %in% xy.list[[x]]$token) {
      i1 = gregexpr("\\[{2}", paste(xy.list[[x]]$text, collapse = ""))
      i2 = gregexpr("\\]{2}", paste(xy.list[[x]]$text, collapse = ""))
      trg = substring(paste(xy.list[[x]]$text, collapse = ""), i1[[1]], i2[[1]] + 1)
      src = xy.list[[x]]$text[which(xy.list[[x]]$token == "LBB") - 1]
      src = ifelse(src == ")", paste(xy.list[[x]]$text[which(xy.list[[x]]$token == "'('")[max(which(which(xy.list[[x]]$token == "'('") < which(xy.list[[x]]$token == "LBB")))]:which(xy.list[[x]]$token == "LBB") - 1], collapse = ""), src)
      cmd = paste(src, trg, sep = "")
      text = str_replace(paste(xy.list[[x]]$text, collapse = ""), "\\[\\[.*?\\]\\]", "")
      left = str_count(text, "\\[")
      if (left > 0) {
        num = numeric(str_count(text, "\\["))
        return(make_edge(target = trg, attr = "", source = src, interaction = c("subset"), weight = 1, cmd = cmd, id = x, edge_lgth = str, all_groups = groups, extra = src))

      } else {
        return(make_edge(target = trg, attr = "", source = src, interaction = c("subset"), weight = 1, cmd = cmd, id = x, edge_lgth = str, all_groups = groups, extra = src))
      }
    }

    if (length(n) > 0 && length(n) < 2) {
      m = regexpr("\\[.*\\]", text[1], perl = TRUE)
      m = regexpr("\\[[^\\s]{1,}\\]", text[1], perl = TRUE)
      src = regexpr("(\\w{0,}[.]{0,})+\\w+\\){0,}(?=\\[[^\\s]{1,}\\])", text[1], perl = TRUE)
      if (src[[1]][1] == -1) {
        return(NULL)
      }
      r = regmatches(text, m)
      r.src = regmatches(text, src)
      src.id = which(xy.list[[x]]$text == r.src)
      cmd = paste(c(r.src, r), collapse = "")
      attr = ""
      eq.id = which(xy.list[[x]]$token %in% c("EQ_ASSIGN", "LEFT_ASSIGN"))
      if (length(eq.id) > 0) {
        if (eq.id > n) {
          # print(x)
          return(make_edge(target = r, attr = attr, source = r.src, interaction = c("subset"), weight = 1, cmd = cmd, id = x, edge_lgth = str, all_groups = groups, extra = r.src))
        }
      }
      if (str_detect(r.src, "\\)")) {
        # subset of function results
        r.src = gsub("\\)", "", r.src)
        f.id = which(xy.list[[x]]$token == "SYMBOL_FUNCTION_CALL")
        src.id = which(xy.list[[x]]$text == r.src)
        fnc = xy.list[[x]]$text[max(f.id[f.id < src.id])]

        cmd = paste(c(xy.list[[x]]$text[max(f.id[f.id < src.id]):src.id], ")", r), collapse = "")
        make_edge(target = r, attr = fnc, source = r.src, interaction = c("subset"), weight = 1, cmd = cmd, id = x, edge_lgth = str, all_groups = groups, extra = paste(fnc, r.src, sep = "-"))

      } else {
        make_edge(target = r, attr = attr, source = r.src, interaction = c("subset"), weight = 1, cmd = cmd, id = x, edge_lgth = str, all_groups = groups, extra = r.src)

      }

    } else if (patt & length(n) == 0) {
      return(NULL)
    } else if (length(n) >= 2 && (!"LBB" %in% xy.list[[x]]$token) & (!imb)) {
      m = gregexpr("\\[.*?\\]", text)
      r = regmatches(text, m)[[1]]
      src1 = gregexpr("\\w+\\){0,}(?=\\[[^\\s]{1,}\\])", text, perl = TRUE)
      r.src = regmatches(text, src1)[[1]]
      if (length(r) != length(r.src)) {
        fir = which(r %in% str_extract(text, "(\\[.*?\\])(?=\\[.*?\\])"))
        sec = which(r %in% str_extract(text, "(?<=\\])(\\[.*?\\])"))[1]
        r[fir] = paste(r[fir], r[sec], sep = "")
        r = r[-sec]
      }
      cmd = paste(r.src, r, sep = "")
      if (any(str_detect(r.src, "\\)"))) {
        wh = str_which(r.src, "\\)")
        r.src.f = gsub("\\)", "", r.src[wh])
        f.id = which(xy.list[[x]]$token == "SYMBOL_FUNCTION_CALL")
        src.id = which(xy.list[[x]]$text %in% r.src.f)
        f.ids = which((f.id + 2) %in% src.id)
        fnc = xy.list[[x]]$text[f.id[f.ids]]
        cmd[wh] = paste(c(fnc, "(", r.src.f, ")", r[wh]), collapse = "")
        r.src[wh] = r.src.f
      }

      # substring(text,i1[[1]],i2[[1]])
      make_edge(target = r, attr = "", source = r.src, interaction = c("subset"), weight = 1, cmd = cmd, id = x, edge_lgth = str, all_groups = groups, extra = r.src)
    } else if (imb) {
      for (ind in 1:(length(i1[[1]]) - 1)) {
        if (i2[[1]][ind] < i1[[1]][ind + 1]) {
          trg1 = gregexpr("\\[.*?\\]", text, perl = TRUE)
          src1 = gregexpr("\\w+\\){0,}(?=\\[[^\\s]{1,}\\])", text, perl = TRUE)
          src = regmatches(text, src1)[[1]][ind]
          trg = regmatches(text, trg1)[[1]][ind]
          cmd = paste(src, trg, sep = "")
          return(make_edge(target = trg, attr = "", source = src, interaction = c("subset"), weight = 1, cmd = cmd, id = x, edge_lgth = str, all_groups = groups, extra = src))
        }
      }
    }
  })


  ##check if pattern is in function argument extract dollar assignations:
  # car$breaks links car to breaks

  dollars = lapply(seq_along(xy.list), FUN = function(x, inter) {
    str = 1.5
    m = which(xy.list[[x]]$token == "'$'" | xy.list[[x]]$token == "'@'")
    if (length(m) > 0) {
      if (sum(xy.list[[x]]$text[m - 1] == "]") == 0) {
        cmd = paste(xy.list[[x]]$text[m - 1], "$", xy.list[[x]]$text[m + 1], sep = "")
        make_edge(target = xy.list[[x]]$text[m + 1], attr = "$", source = xy.list[[x]]$text[m - 1], interaction = c("holds"), weight = 1, cmd = cmd, id = make.unique(rep(as.character(x), length(m))), edge_lgth = str, all_groups = groups)
      } else {
        opn = which(xy.list[[x]]$token == "'['" | xy.list[[x]]$token == "LBB")
        l = list()
        for (i in seq_len(length(m))) {
          id = make.unique(rep(as.character(x), length(m)))[i]
          cmd = paste(paste(xy.list[[x]]$text[opn[i]:(m[i] - 1)], collapse = ""), "$", xy.list[[x]]$text[m[i] + 1], sep = "")
          l[[i]] = make_edge(target = paste(xy.list[[x]]$text[opn[i]:(m[i] - 1)], collapse = ""), attr = "$", source = xy.list[[x]]$text[m[i] + 1], interaction = c("holds"), weight = 1, cmd = cmd, id = id, edge_lgth = str, all_groups = groups, extra = paste(xy.list[[x]]$text[opn[i] - 1]))
        }
        return(bind_rows(l))
      }
    }
  })


  # extract name of function called in first instance ex setwd() or print() or plot()
  strtfunc = lapply(seq_along(xy.list), FUN = function(x) {
    m = which(xy.list[[x]]$token == "SYMBOL_FUNCTION_CALL")
    attr = ""
    edgtype = "SOLID"
    edg2 = NULL
    if (length(m) > 0) {
      if (m[1] == 1) {
        n = which(xy.list[[x]]$token == "SYMBOL")
        cmd = paste(xy.list[[x]]$text, collapse = "")
        if (length(n) > 0) {
          target1 = xy.list[[x]]$text[n]
          if (!is.null(for.id[[x]])) {

            if (xy.list[[x]]$text[1] == "return") {
              if (sum(nchar(xy.list[[x]]$text[n]) == 1) > 0) {
                id = which(nchar(xy.list[[x]]$text[n]) == 1)
                target1 = paste(target1[id], for.id[[x]], sep = ".")

              }
            }

            if (!is.null(brackets[[x]]$target)) {
              target1 = paste(brackets[[x]]$target, for.id[[x]], sep = ".")

            }
            attr = cmd
            edgtype = "LONG_DASH"

          }
          edg = make_edge(target = target1, attr = attr, source = paste(xy.list[[x]]$text[1], x, sep = "."), interaction = "called", weight = 1, id = x, cmd = cmd, all_groups = groups, edgtype = edgtype)

          return(rbind(edg, edg2))
        }
        if (xy.list[[x]]$text[1] == "setwd")
        {
          edgtype = "SOLID"

          rbind(make_edge(target = paste(xy.list[[x]]$text[1],x,sep="."), attr = "", source = "WorkingDir", interaction = "wd.init", weight = 1, id = 0, cmd = cmd, all_groups = groups), make_edge(target = paste(xy.list[[x]]$text[(m + 2):(length(xy.list[[x]]$text) - 1)], collapse = ""), attr = "=", source = paste(xy.list[[x]]$text[1], x, sep = "."), interaction = "wd.init", weight = 1, id = 0, cmd = cmd, all_groups = groups), make_edge(target = "WorkingDir", attr = "", source = "Session", interaction = "env.setup",
                                                                                                                                                                                                                                                                                                                                                                                                                                                       weight = 1, id = 0, cmd = cmd, all_groups = groups))
        }  #
      } else if (m[1] == 3) {
      }
    }
  })


  #gives info on what's next to = or <-
  right2eq = lapply(seq_along(xy.list), FUN = function(x) {
    eq = which(xy.list[[x]]$token %in% c("EQ_ASSIGN", "LEFT_ASSIGN"))
    if (length(eq) > 0) {
      return(xy.list[[x]]$token[eq + 1])
    }
  })

  # ??returns last var of sequence var1$var2[var3] : var3 following $ [ but not (
  target_vars = lapply(seq_along(dollars), FUN = function(x) {
    # print(x)
    if (!is.null(right2eq[[x]])) {
      # type is either 1 for dollars only , 2 for brackets only and 3 for dollars and brackets
      done = FALSE
      vars = c()
      cmd = paste(xy.list[[x]]$text, collapse = "")
      m = which(xy.list[[x]]$token %in% c("EQ_ASSIGN", "LEFT_ASSIGN"))
      cmd_left = paste(xy.list[[x]]$text[(m + 1):length(xy.list[[x]]$text)], collapse = "")
      cmd_left = gsub("\\s{0,}[#].*", "", cmd_left, perl = TRUE)
      if (!is.null(dollars[[x]]) & !is.null(brackets[[x]])) {
        opt1 = c(dollars[[x]]$source, dollars[[x]]$attr, dollars[[x]]$target, brackets[[x]]$source, brackets[[x]]$attr, brackets[[x]]$target)
        opt2 = c(brackets[[x]]$source, brackets[[x]]$attr, brackets[[x]]$target, dollars[[x]]$source, dollars[[x]]$attr, dollars[[x]]$target)
        opt1 = ifelse(sum(duplicated(opt1)) > 0, paste(opt1[-c(which(duplicated(opt1) | opt1 == ""))], collapse = ","), paste(opt1, collapse = ","))
        opt2 = ifelse(sum(duplicated(opt2)) > 0, paste(opt2[-c(which(duplicated(opt2) | opt2 == ""))], collapse = ","), paste(opt2, collapse = ","))
        if (grepl(gsub(",", "", opt1), cmd_left, fixed = TRUE)) {

          vars = brackets[[x]]$target

        } else if (grepl(gsub(",", "", opt2), cmd_left, fixed = TRUE)) {
          vars = dollars[[x]]$target

        }
        done = TRUE
      }
      if (!is.null(dollars[[x]]) & isFALSE(done)) {
        vars = c()
        for (y in seq_len(dim(dollars[[x]])[1])) {
          opt1 = c(dollars[[x]][y, ]$source, dollars[[x]][y, ]$attr, dollars[[x]][y, ]$target)
          opt1 = paste(opt1, collapse = "")
          if (grepl(opt1, cmd_left, fixed = TRUE)) {
            vars = c(vars, dollars[[x]][y, ]$target)
          }
        }

      } else if (!is.null(brackets[[x]]) & isFALSE(done)) {
        vars = c()
        opt2 = c(brackets[[x]]$source, brackets[[x]]$attr, brackets[[x]]$target)
        opt2 = paste(opt2, collapse = "")
        opt3 = c(brackets[[x]]$attr, "(", brackets[[x]]$source, ")", brackets[[x]]$target)
        opt3 = paste(opt3, collapse = "")
        opt4 = c(brackets[[x]]$attr, "(", brackets[[x]]$source, brackets[[x]]$target, ")")
        opt4 = paste(opt4, collapse = "")
        for (y in seq_len(dim(brackets[[x]])[1])) {

          if (grepl(opt2, cmd_left, fixed = TRUE)) {
            vars = c(vars, brackets[[x]][y, ]$target)
          } else if (right2eq[[x]] %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL_PACKAGE") & (grepl(opt3, cmd_left, fixed = TRUE) | grepl(opt4, cmd_left, fixed = TRUE))) {
            vars = c(vars, brackets[[x]][y, ]$target)

          }
        }
      }
      if (!is.null(vars)) {
        # print(x) print(vars)
        if (sum(grepl("\\(", vars, perl = TRUE)) > 0) {

          # make list with edges containing vars from functions or with node fn1(fn2(var1)) /// fn3(fn4(var2)) return var 1 var 2
          if (is.integer(openpar[[x]])) {
            id1 = openpar[[x]][1]
            id2 = openpar[[x]][2]
            to_keep = which(xy.list[[x]]$token[id1:id2] == "SYMBOL")
            vars2 = xy.list[[x]]$text[id1:id2][to_keep]
          } else {
            ids = openpar[[x]][which(openpar[[x]][, 1][-c(1)] > openpar[[x]][, 2][-c(dim(openpar[[x]])[1])]), ]
            if (nrow(ids) > 0) {
              vars2 = unlist(lapply(seq_along(dim(ids)[1]), function(f) {
                id1 = ids[f, 1] + 1
                id2 = ids[f, 2] - 1
                to_keep = which(xy.list[[x]]$token[id1:id2] == "SYMBOL")
                xy.list[[x]]$text[id1:id2][to_keep]
              }))
            } else {
              id1 = tail(openpar[[x]], 1)[1]
              id2 = tail(openpar[[x]], 1)[2]
              to_keep = which(xy.list[[x]]$token[id1:id2] == "SYMBOL")
              vars2 = xy.list[[x]]$text[id1:id2][to_keep]
            }
          }
          return(list(vars, vars2))
        } else {
          return(list(vars, NULL))
        }
      }

    }
    return(list(NULL, NULL))

  })

  if (length(target_vars) > 1) {
    target_vars2 = sapply(target_vars, `[[`, 2)
    target_vars = sapply(target_vars, `[[`, 1)
  } else {
    target_vars2 = sapply(target_vars, `[`, 2)
    target_vars = sapply(target_vars, `[`, 1)
  }


  trg = lapply(seq_along(target_vars2), function(x) {
    if (!is.null(target_vars2)) {
      a = match(brackets[[x]]$source, target_vars[[x]])
      b = match(dollars[[x]]$source, target_vars[[x]])
      if (length(which(!is.na(a))) > 0) {
        to_select = 1
      } else if (length(which(!is.na(b))) > 0) {
        to_select = 2
      } else {
        to_select = 0
      }
    }
  })


  #creates links where = is found
  eqquery = lapply(seq_along(xy.list), FUN = function(x) {
    #print(x)
    str = 1.5
    m = which(xy.list[[x]]$token %in% c("EQ_ASSIGN", "LEFT_ASSIGN"))
    n_tmp = which(xy.list[[x]]$token == "SYMBOL_FUNCTION_CALL")
    n = which(xy.list[[x]]$text[n_tmp] == "foreach" | xy.list[[x]]$text[n_tmp] == "ifelse")
    n = c(n, which(grepl("apply", xy.list[[x]]$text[n_tmp])))
    edge = NULL
    edge2 = NULL
    src = NULL
    attr = " = "
    extra_tmp = c()
    cmd = paste(xy.list[[x]]$text, collapse = "")
    if (length(m) == 0){
      return(list(NULL,NULL))
    } else if (length(m) > 0) {
      edg = xy.list[[x]]$text[1]
      if (m > 2) {
        edg = paste(xy.list[[x]]$text[1:(m - 1)], collapse = "")
        cmd = edg
        edg = unlist(str_split(edg, "(?=\\[{1,})(?<!\\[)|(?<=\\$)|(?=\\$)"))

        if (TRUE) {
          for (i in seq_len(length(edg))) {

            if (grepl("^\\$", edg[i], perl = TRUE)) {
              edg[i] = gsub("\\$", "", edg[i])
              edg = edg[-c(i)]
            }
          }
        }

      } else if (m == 2 & right2eq[[x]] %in% c("SYMBOL_FUNCTION_CALL")) {
        edg = xy.list[[x]]$text[1]
        a = match(brackets[[x]]$source, xy.list[[x]]$text[max(which(xy.list[[x]]$token == "SYMBOL"))])
        b = match(dollars[[x]]$source, xy.list[[x]]$text[max(which(xy.list[[x]]$token == "SYMBOL"))])
        if (length(which(!is.na(a))) > 0) {
          to_select = 1
        } else if (length(which(!is.na(b))) > 0) {
          to_select = 2
        } else {
          to_select = 0
        }
        if (to_select == 1) {
          src = brackets[[x]]$target
        } else if (to_select == 2) {
          src = dollars[[x]]$target

        }
        attr = paste(paste(xy.list[[x]]$text[m + 1:(length(xy.list[[x]]$text) - m)]), collapse = "")
        variables = c(xy.list[[x]]$text[which(xy.list[[x]]$token %in% c("SYMBOL", target_vars[[x]]))], "\\$")
        attr = gsub(paste("\\b", paste(variables, collapse = "\\b|\\b"), "\\b", sep = ""), ".", attr)
        if (!is.null(brackets[[x]]) & !is.null(brackets[[x]]$extra)) {
          if (sum(unique(brackets[[x]]$extra) != "") > 0) {

            extra_tmp = brackets[[x]]$extra
          }
        }
      } else if (m == 2 & right2eq[[x]] %in% c("SYMBOL_PACKAGE")) {
        edg = xy.list[[x]]$text[1]
        attr = paste(xy.list[[x]]$text[5], "(", ")", sep = "")
        a = match(brackets[[x]]$source, xy.list[[x]]$text[max(which(xy.list[[x]]$token == "SYMBOL"))])
        b = match(dollars[[x]]$source, xy.list[[x]]$text[max(which(xy.list[[x]]$token == "SYMBOL"))])
        if (length(which(!is.na(a))) > 0) {
          to_select = 1
        } else if (length(which(!is.na(b))) > 0) {
          to_select = 2
        } else {
          to_select = 0
        }

        if (to_select == 1) {
          src = brackets[[x]]$target
        } else if (to_select == 2) {
          src = dollars[[x]]$target
        }
        attr = paste(paste(xy.list[[x]]$text[m + 1:(length(xy.list[[x]]$text) - m)]), collapse = "")
        variables = c(xy.list[[x]]$text[which(xy.list[[x]]$token %in% c("SYMBOL", target_vars[[x]]))], "\\$")
        attr = gsub(paste("\\b", paste(variables, collapse = "\\b|\\b"), "\\b", sep = ""), ".", attr)
      } else if (m == 2 & !is.null(dollars[[x]]) & !is.null(brackets[[x]])) {
        a = match(brackets[[x]]$source, xy.list[[x]]$text[max(which(xy.list[[x]]$token == "SYMBOL"))])
        b = match(dollars[[x]]$source, xy.list[[x]]$text[max(which(xy.list[[x]]$token == "SYMBOL"))])
        to_select = which(!is.na(c(a, b)))
        to_select = ifelse(length(to_select) == 0, 0, to_select)
        if (to_select == 1) {
          src = brackets[[x]]$target
        } else if (to_select == 2) {
          src = dollars[[x]]$target
        }
        edg = paste(xy.list[[x]]$text[m - 1], collapse = "")

        cmd = paste(xy.list[[x]]$text, collapse = "")
      } else if (m == 2 & (!is.null(dollars[[x]]))) {
        edg = xy.list[[x]]$text[1]
        cmd = paste(xy.list[[x]]$text, collapse = "")
        if (dollars[[x]]$target == edg) {
          src = dollars[[x]]$target
          attr = dollars[[x]]$cmd
        } else if (xy.list[[x]]$text[m + 1] == dollars[[x]]$source) {
          src = dollars[[x]]$target
        }
      } else if (m == 2 & (!is.null(brackets[[x]]))) {
        edg = xy.list[[x]]$text[1]
        cmd = paste(xy.list[[x]]$text, collapse = "")
        if (brackets[[x]]$target == edg) {
          src = brackets[[x]]$target
          attr = brackets[[x]]$cmd
        } else if (xy.list[[x]]$text[m + 1] == brackets[[x]]$source) {
          src = brackets[[x]]$target
        }
      } else if (m == 2 & is.null(dollars[[x]]) & is.null(brackets[[x]])) {
        edg = paste(xy.list[[x]]$text[m - 1], collapse = "")
        cmd = paste(xy.list[[x]]$text, collapse = "")
      }

      ################# make edges case var1[var2]=var3 edge=make_edge(target = edg[2],source = edg[1],interaction = 'holds',weight = 1,attr = '<-',id=x,cmd = cmd) extra=xy.list[[x]]$text[max(which(xy.list[[x]]$token=='SYMBOL'))] #keep trace of bypassed source
      if (length(edg) == 2 && right2eq[[x]] %in% c("NUM_CONST", "STR_CONST", "SYMBOL") & length(xy.list[[x]]$token) == (m + 1)) {
        edge = make_edge(target = xy.list[[x]]$text[m + 1], source = edg[2], interaction = "holds", weight = 1, attr = attr, id = x, cmd = cmd, all_groups = groups, extra = edg[1])

      } else if (length(edg) == 2 && right2eq[[x]] %in% c("NUM_CONST", "STR_CONST", "SYMBOL") & length(xy.list[[x]]$token) > (m + 1)) {
        # case var1[var2]=var3
        trg = xy.list[[x]]$text[(m + 1):length(xy.list[[x]]$text)][which(xy.list[[x]]$token[(m + 1):length(xy.list[[x]]$text)] == "SYMBOL")]
        attr = paste(xy.list[[x]]$text[m + 1:(length(xy.list[[x]]$text) - (m))], collapse = "")
        edge = make_edge(target = trg, source = edg[2], interaction = "holds", weight = 1, attr = attr, id = x, cmd = cmd, all_groups = groups, extra = edg[length(edg)])

      } else if (length(edg) == 1 && right2eq[[x]] %in% c("NUM_CONST", "STR_CONST", "SYMBOL")) {
        # case var1='NUM_STR_SYMBOL'...  print(x) case var1=... and ... contains $ or []
        if (!is.null(src)) {
          shifted = 2
          sym = which(xy.list[[x]]$token == "SYMBOL")[-c(1)]
          sym_brk = which(xy.list[[x]]$token[sym + 1] %in% c("'['", "LBB"))
          if (length(sym_brk) == 0)
          {
            sym_brk = which(xy.list[[x]]$token[sym + 2] %in% c("'['", "LBB"))
            shifted = 3
          }  #case var)[indice] parenthesis
          src_id = which(xy.list[[x]]$text %in% gsub("\\[|\\]", "", src, perl = TRUE))
          extra = rep("", length(src))
          if (length(src_id) > 0) {
            extra[match(sym[sym_brk] + shifted, src_id)] = xy.list[[x]]$text[sym][sym_brk]
          } else {
            extra = xy.list[[x]]$text[sym][sym_brk]
          }
          edge2 = make_edge(target = xy.list[[x]]$text[1], source = src, interaction = "holds", weight = 1, attr = attr, id = x, cmd = cmd, all_groups = groups, extra = extra)

        } else if (length(xy.list[[x]]$token) == 3) {
          # case var1=var2 / no bypass

          edge2 = make_edge(target = xy.list[[x]]$text[1], source = xy.list[[x]]$text[m + 1], interaction = "holds", weight = 1, attr = attr, id = x, cmd = cmd, all_groups = groups)

        } else {
          # case var1=NUM + ...
          ids = which(xy.list[[x]]$token == "SYMBOL")[-c(1)]
          vars = xy.list[[x]]$text[ids]

          if (length(ids) > 0) {
            if (length(ids) > 1) {
              vars_names = paste(vars[which(vars != ".")], collapse = "|")
            } else {
              vars_names = vars
            }
            attr_tmp = paste(xy.list[[x]]$text[(m):length(xy.list[[x]]$text)], collapse = "")
            attr = gsub(vars_names, ".", attr_tmp)
          }
          edge2 = make_edge(target = xy.list[[x]]$text[1], source = vars, interaction = "holds", weight = 1, attr = attr, id = x, cmd = cmd, all_groups = groups)

        }
      } else if (length(edg) == 1 && right2eq[[x]] %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL_PACKAGE") & length(n) == 0) {
        if (!is.null(src)) {
          shifted = 2
          sym = which(xy.list[[x]]$token == "SYMBOL")[-c(1)]
          sym_brk = which(xy.list[[x]]$token[sym + 1] %in% c("'['", "LBB"))
          if (length(sym_brk) == 0)
          {
            sym_brk = which(xy.list[[x]]$token[sym + 2] %in% c("'['", "LBB"))
            shifted = 3
          }  #case var)[indice] parenthesis
          src_id = which(xy.list[[x]]$text %in% gsub("\\[|\\]", "", src, perl = TRUE))
          extra = rep("", length(src))
          if (length(src_id) > 0) {
            extra[match(sym[sym_brk] + shifted, src_id)] = xy.list[[x]]$text[sym][sym_brk]
          } else {
            extra = xy.list[[x]]$text[sym][sym_brk]
          }
          if (!is.null(extra_tmp)) {
            extra[which(extra_tmp != "")] = extra_tmp[which(extra_tmp != "")]
          }
          edge2 = make_edge(target = xy.list[[x]]$text[1], source = src, interaction = "holds", weight = 1, attr = attr, id = x, cmd = cmd, all_groups = groups, extra = extra)

        } else {
          # case var1=function(...) no $ []
          dup_nodes = all_nodes[[x]]
          nodes = unique(all_nodes[[x]][-c(which(all_nodes[[x]] == xy.list[[x]]$text[1]))])
          nd.bk = nodes
          if (length(nodes) > 0) {
            # case variable in function
            extra = ""
            if (!is.null(target_vars[[x]])) {
              # case sequence seen var1$var2 link to var2 only
              nodes = nodes[-c(which(nodes %in% unique(c(brackets[[x]]$source, dollars[[x]]$source))))]
              # nodes=target_vars[[x]] print(x)
              if (sum(grepl("\\[", target_vars[[x]]), perl = TRUE) > 0) {
                extra = nd.bk[-c(match(nodes, nd.bk))]
              }
            }
            edge2 = make_edge(target = xy.list[[x]]$text[1], source = nodes, interaction = "holds", weight = 1, attr = attr, id = x, cmd = cmd, all_groups = groups, extra = extra)

          } else if (length(dup_nodes) == 2) {
            edge2 = make_edge(target = xy.list[[x]]$text[1], source = xy.list[[x]]$text[1], interaction = "holds", weight = 1, attr = attr, id = x, cmd = cmd, all_groups = groups)
          } else {
            edge2 = make_edge(target = xy.list[[x]]$text[1], source = paste("init", x, sep = "."), interaction = "holds", weight = 1, attr = attr, id = x, cmd = cmd, all_groups = groups)

          }

        }
      } else if (length(edg) > 2 && right2eq[[x]] %in% c("NUM_CONST", "STR_CONST", "SYMBOL")) {
        attr = ""
        if (grepl("\\]\\[", paste(edg, collapse = ""), perl = TRUE)) {
          extra = edg[1]
        } else if (grepl("\\[", paste(edg, collapse = ""), perl = TRUE)) {
          extra = edg[(grep(pattern = "\\[", edg, perl = TRUE)) - 1]
        }
        edge2 = make_edge(target = xy.list[[x]]$text[m + 1], source = edg[length(edg)], interaction = "holds", weight = 1, attr = attr, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, extra = extra)
        edge2 = rbind(edge2, make_edge(target = edg[length(edg)], source = edg[(length(edg) - 1)], interaction = "holds", weight = 1, attr = attr, id = x, cmd = cmd, all_groups = groups, extra = extra, edge_lgth = str))

        for (i in (length(edg) - 1):2) {
          edge2 = rbind(edge2, make_edge(target = edg[i], source = edg[i - 1], interaction = "holds", weight = 1, attr = attr, id = x, cmd = cmd, edge_lgth = str, all_groups = groups, extra = extra))
        }

      }


      if (!is.null(edge2)) {
        if (dim(edge2)[1] > 1 & right2eq[[x]] %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL_PACKAGE")) {
          for (y in seq_len(dim(edge2)[1])) {
            edge2$attr[y] = paste(edge2$attr[y], paste(rep(".", y), collapse = ""), sep = paste(" : arg", y, sep = ""))
            # modify attr variables to . for first .. for second etc.. for sake of lisibility

          }
        }
      }

      return(list(edge, edge2))
    } else {
      return(list(edge, edge2))
    }

  })
  eq.id = unlist(eqquery)
  if (length(eqquery) > 1) {
    simple_assign = sapply(eqquery, `[[`, 2)
    complex_assign = sapply(eqquery, `[[`, 1)
  } else {
    simple_assign = sapply(eqquery, `[`, 2)
    complex_assign = sapply(eqquery, `[`, 1)
  }

  # create edges for if group
  trg = c()
  conditionifs = lapply(seq_along(xy.list), FUN = function(x) {
    # print(x)
    m = which(xy.list[[x]]$token %in% c("IF"))
    n = which(xy.list[[x]]$token %in% c("ELSE"))
    if (length(m) > 0 && length(n) == 0) {
      # if found only 'if' print(x)
      condtk = xy.list[[x]]$token[(m + 2):(length(xy.list[[x]]$token) - 2)]
      condtxt = xy.list[[x]]$text[(m + 2):(length(xy.list[[x]]$text) - 2)]
      x1 = brackets[[x]]$cmd
      x2 = dollars[[x]]$cmd
      trg <<- paste("IF", x, sep = ".")  # save current if name
      # id=groups() case if (var1) condition is TRUE
      if (length(condtxt) == 1) {
        attr = "TRUE"
        return(make_edge(target = trg, attr = attr, source = condtxt, interaction = "IFcondition", weight = 1, id = x, cmd = condtxt, all_groups = groups))
      } else if ((!is.null(x1))) {
        trg1 = gregexpr("(\\b\\w+\\b)|\\W", x1, perl = TRUE)
        src = regmatches(x1, trg1)[[1]]
        source = brackets[[x]]$target
        found = match(src, condtxt)
        in_cnd = all_nodes[[x]][-c(which(all_nodes[[x]] %in% src))]

      } else if ((!is.null(x2))) {
        trg1 = gregexpr("(\\w{0,}[\\.]{0,}\\w{0,})|(\\W)(?<![\\.])+", x2, perl = TRUE)
        attr(trg1[[1]], "match.length") = ifelse(attr(trg1[[1]], "match.length") == 0, 1, attr(trg1[[1]], "match.length"))
        src = regmatches(x2, trg1)[[1]]
        source = dollars[[x]]$target
        found = match(src, condtxt)
        in_cnd = all_nodes[[x]][-c(which(all_nodes[[x]] %in% src))]
      } else if (length(condtxt) > 1) {
        in_cnd = c()
        found = c()
        source = unique(all_nodes[[x]])
      } else {
        return(NULL)
      }

      attr = paste(condtxt, collapse = "")
      extra = ifelse(is.null(brackets[[x]]$extra[1]), "", brackets[[x]]$extra[1])

      if (length(found > 0) & length(in_cnd) == 0) {
        attr = paste(condtxt, collapse = "")
        make_edge(target = trg, attr = attr, source = source, interaction = "IFcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra)
      } else if (length(found > 0) & length(in_cnd) >= 1) {
        make_edge(target = trg, attr = attr, source = c(source, in_cnd), interaction = "IFcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra)
      } else if (length(found) == 0) {
        make_edge(target = trg, attr = attr, source = source, interaction = "IFcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra)
      }
    } else if (!is.null(if.id[[x]]) & isFALSE(xy.list[[x]]$text[1] %in% c("'{'", "'}'"))) {
      if (!is.null(simple_assign[[x]])) {
        if (nrow(simple_assign[[x]]) == 1) {
          target.if = simple_assign[[x]]$source
          make_edge(target = target.if, attr = "", source = trg, interaction = "IFcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = simple_assign[[x]]$extra)

        } else {
          target.if = simple_assign[[x]]$target[1]
          make_edge(target = target.if, attr = "", source = trg, interaction = "IFcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = simple_assign[[x]]$extra[1])

        }
      } else if (!is.null(complex_assign[[x]])) {
        if (nrow(complex_assign[[x]]) == 1) {
          target.if = complex_assign[[x]]$target
          make_edge(target = target.if, attr = "", source = trg, interaction = "IFcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = complex_assign[[x]]$extra)

        } else {
          target.if = complex_assign[[x]]$target[1]
          make_edge(target = target.if, attr = "", source = trg, interaction = "IFcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = complex_assign[[x]]$extra[1])

        }

      }

    }
  })

  trg = c()
  # create edges for else if group
  conditionelseifs = lapply(seq_along(xy.list), FUN = function(x) {
    m = which(xy.list[[x]]$token %in% c("IF"))
    n = which(xy.list[[x]]$token %in% c("ELSE"))
    # print(x) case else if in command
    if (length(m) > 0 & length(n) > 0) {
      src.if = paste("IF", tail(groups[[x]], 1), sep = ".")
      # trg=paste('ELSE IF',x,sep='.')
      condtk = xy.list[[x]]$token[(m + 2):(length(xy.list[[x]]$token) - 2)]
      condtxt = xy.list[[x]]$text[(m + 2):(length(xy.list[[x]]$text) - 2)]
      x1 = brackets[[x]]$cmd
      x2 = dollars[[x]]$cmd
      # print(x)
      trg <<- paste("ELSE IF", x, sep = ".")
      if (length(condtxt) == 1) {
        # case if TRUE
        attr = "TRUE"
        edg1 = make_edge(target = trg, attr = "", source = src.if, interaction = "ELSEIFcondition", weight = 1, id = x, cmd = condtxt, all_groups = groups, edgtype = "LONG_DASH")
        edg2 = make_edge(target = trg, attr = attr, source = condtxt, interaction = "ELSEIFcondition", weight = 1, id = x, cmd = condtxt, all_groups = groups)
        return(rbind(edg1, edg2))
      } else if ((!is.null(x1))) {
        trg1 = gregexpr("(\\b\\w+\\b)|\\W", x1, perl = TRUE)
        src = regmatches(x1, trg1)[[1]]
        target = brackets[[x]]$target
        found = match(src, condtxt)
        in_cnd = all_nodes[[x]][-c(which(all_nodes[[x]] %in% src))]
      } else if ((!is.null(x2))) {
        trg1 = gregexpr("(\\w{0,}[\\.]{0,}\\w{0,})|(\\W)(?<![\\.])+", x2, perl = TRUE)
        attr(trg1[[1]], "match.length") = ifelse(attr(trg1[[1]], "match.length") == 0, 1, attr(trg1[[1]], "match.length"))
        src = regmatches(x2, trg1)[[1]]
        target = dollars[[x]]$target
        found = match(src, condtxt)
        in_cnd = all_nodes[[x]][-c(which(all_nodes[[x]] %in% src))]
      } else if (length(condtxt) > 1) {
        in_cnd = c()
        found = c()
        target = unique(all_nodes[[x]])
      } else {
        return(NULL)
      }
      extra = ifelse(is.null(brackets[[x]]$extra[1]), "", brackets[[x]]$extra[1])
      attr = paste(condtxt, collapse = "")
      if (length(found) > 0 & length(in_cnd) == 0) {
        edg1 = make_edge(target = trg, attr = attr, source = target, interaction = "ELSEIFcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra, edgtype = "LONG_DASH")
        edg2 = make_edge(target = trg, attr = "", source = src.if, interaction = "ELSEIFcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra)

      } else if (length(found) > 0 & length(in_cnd) >= 1) {
        edg1 = make_edge(target = trg, attr = attr, source = c(target, in_cnd), interaction = "ELSEIFcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra, edgtype = "LONG_DASH")
        edg2 = make_edge(target = trg, attr = "", source = src.if, interaction = "ELSEIFcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra)

      } else if (length(found) == 0) {
        edg1 = make_edge(target = trg, attr = attr, source = target, interaction = "ELSEIFcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra, edgtype = "LONG_DASH")
        edg2 = make_edge(target = trg, attr = "", source = src.if, interaction = "ELSEIFcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra)
      }
      return(rbind(edg1, edg2))
    } else if (!is.null(elseif.id[[x]]) & isFALSE(xy.list[[x]]$text[1] %in% c("'{'", "'}'"))) {
      if (!is.null(simple_assign[[x]])) {
        if (nrow(simple_assign[[x]]) == 1) {
          target.elseif = simple_assign[[x]]$source
          make_edge(target = target.elseif, attr = "", source = trg, interaction = "ELSEIFcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = simple_assign[[x]]$extra)

        } else {
          target.elseif = simple_assign[[x]]$target[1]
          make_edge(target = target.elseif, attr = "", source = trg, interaction = "ELSEIFcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = simple_assign[[x]]$extra[1])

        }
      } else if (!is.null(complex_assign[[x]])) {
        if (nrow(complex_assign[[x]]) == 1) {
          target.elseif = complex_assign[[x]]$target
          make_edge(target = target.elseif, attr = "", source = trg, interaction = "ELSEIFcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = complex_assign[[x]]$extra)

        } else {
          target.elseif = complex_assign[[x]]$target[1]
          make_edge(target = target.elseif, attr = "", source = trg, interaction = "ELSEIFcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = complex_assign[[x]]$extra[1])

        }

      }
    }
  })

  # create edges for else group

  trg = c()
  elsequery = lapply(seq_along(xy.list), FUN = function(x) {
    m = which(xy.list[[x]]$token %in% c("ELSE"))
    n = which(xy.list[[x]]$token %in% c("IF"))
    attr = ""
    if (length(m) > 0 & length(n) == 0) {
      trg <<- paste("ELSE", x, sep = ".")
      src.if = paste("IF", tail(groups[[x]], 1), sep = ".")
      inds = which(groups == tail(groups[[x]], 1))
      switch = c()
      for (i in inds) {
        if (!is.null(elseif.id[[i]]) & length(switch) == 0) {
          switch = i
        }
      }
      if (length(switch) > 0) {
        src.if = paste("ELSE IF", max(switch), sep = ".")
      }

      src.elseif = paste("ELSE IF", tail(groups[[x]], 1), sep = ".")

      edg1 = make_edge(target = trg, attr = attr, source = src.if, interaction = "ELSEcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups)
      return(edg1)
    } else if (!is.null(else.id[[x]]) & isFALSE(xy.list[[x]]$text[1] %in% c("'{'", "'}'"))) {
      if (!is.null(simple_assign[[x]])) {
        if (nrow(simple_assign[[x]]) == 1) {
          target.else = simple_assign[[x]]$source
          make_edge(target = target.else, attr = "", source = trg, interaction = "ELSEcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = simple_assign[[x]]$extra)

        } else {
          target.else = simple_assign[[x]]$target[1]
          make_edge(target = target.else, attr = "", source = trg, interaction = "ELSEcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = simple_assign[[x]]$extra[1])

        }
      } else if (!is.null(complex_assign[[x]])) {
        if (nrow(complex_assign[[x]]) == 1) {
          target.else = complex_assign[[x]]$target
          make_edge(target = target.else, attr = "", source = trg, interaction = "ELSEcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = complex_assign[[x]]$extra)

        } else {
          target.else = complex_assign[[x]]$target[1]
          make_edge(target = target.else, attr = "", source = trg, interaction = "ELSEcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = complex_assign[[x]]$extra[1])

        }

      }
    }
  })


  # create edges for for, foreach, while group
  trg = c()
  conditionloop = lapply(seq_along(xy.list), FUN = function(x) {
    indices = ""
    appl = ""
    m = which(xy.list[[x]]$token %in% c("FOR"))
    n_tmp = which(xy.list[[x]]$token == "SYMBOL_FUNCTION_CALL")
    n = which(xy.list[[x]]$text[n_tmp] == "foreach")
    o = which(grepl("apply", xy.list[[x]]$text[n_tmp]))
    p = which(xy.list[[x]]$token %in% c("WHILE"))
    shift = 2
    neg_shift = 3
    if (length(n) > 0) {
      if (n_tmp[n] > 1) {
        shift = 4
      }
    } else if (length(m) > 0) {
      neg_shift = 2

    } else if (length(o) > 0) {
      if (n_tmp[o] > 1) {
        neg_shift = 1
        shift = 4
        appl = "apply:"
      }

    }
    if (length(m) > 0 | length(n) > 0 | length(p) > 0 | length(o) > 0) {
      # if loop found print(x)
      m = c(n, m, o, p)
      condtk = xy.list[[x]]$token[(m + shift):(length(xy.list[[x]]$token) - neg_shift)]
      condtxt = xy.list[[x]]$text[(m + shift):(length(xy.list[[x]]$text) - neg_shift)]
      x1 = brackets[[x]]$cmd
      x2 = dollars[[x]]$cmd
      trg <<- paste("FOR", x, sep = ".")  # save current for name
      if (length(condtxt) == 1) {
        attr = "TRUE"
        return(make_edge(target = trg, attr = attr, source = condtxt, interaction = "LOOPcondition", weight = 1, id = x, cmd = condtxt, all_groups = groups))
      } else if ((!is.null(x1))) {
        trg1 = gregexpr("(\\b\\w+\\b)|\\W", x1, perl = TRUE)
        src = regmatches(x1, trg1)[[1]]
        source = brackets[[x]]$target
        found = match(src, condtxt)
        in_cnd = all_nodes[[x]][-c(which(all_nodes[[x]] %in% src))]

      } else if ((!is.null(x2))) {
        trg1 = gregexpr("(\\w{0,}[\\.]{0,}\\w{0,})|(\\W)(?<![\\.])+", x2, perl = TRUE)
        attr(trg1[[1]], "match.length") = ifelse(attr(trg1[[1]], "match.length") == 0, 1, attr(trg1[[1]], "match.length"))
        src = regmatches(x2, trg1)[[1]]
        source = dollars[[x]]$target
        found = match(src, condtxt)
        in_cnd = all_nodes[[x]][-c(which(all_nodes[[x]] %in% src))]
      } else if (length(condtxt) > 1) {
        in_cnd = c()
        found = 1
        ind = which(condtk %in% c("NUM_CONST", "STR_CONST", "SYMBOL"))[1]
        source = condtxt[ind]

      } else {
        return(NULL)
      }
      id_rm=which(condtxt=="in")
      if( length(id_rm)>0){condtxt[id_rm]="="}
      attr = paste(condtxt, collapse = "")
      if (!is.null(source)) {
        attr = ifelse(grepl(source, attr), gsub(paste("\\b",source,"\\b",sep=""), ".", attr), attr)
        attr = paste(appl, attr, sep = "")
        if (nchar(source) == 1) {
          source = paste(source, strsplit(trg, "[.]")[[1]][2], sep = ".")
          indices = source
        } else if (grepl("function\\(", paste(condtxt, collapse = ""), perl = TRUE)) {
          source = c(source, paste("x", strsplit(trg, "[.]")[[1]][2], sep = "."))
          indices = paste("x", strsplit(trg, "[.]")[[1]][2], sep = ".")
        }
      }
      extra = ifelse(is.null(brackets[[x]]$extra[1]), "", brackets[[x]]$extra[1])
      if (length(found > 0) & length(in_cnd) == 0) {
        edg = make_edge(target = trg, attr = attr, source = source, interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra, indices = indices)
      } else if (length(found > 0) & length(in_cnd) >= 1) {
        edg = make_edge(target = trg, attr = attr, source = c(source, in_cnd), interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra, indices = indices)
      } else if (length(found) == 0) {
        edg = make_edge(target = trg, attr = attr, source = source, interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra, indices = indices)
      }
      if (nrow(edg) > 1) {
        edg$reverse.edg[nrow(edg)] = TRUE
      }
      if (shift == 4) {
        edg2 = make_edge(target = xy.list[[x]]$text[1], attr = attr, source = trg, interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(condtxt, collapse = ""), all_groups = groups, extra = extra, indices = indices)

      } else {
        edg2 = NULL
      }
      return(rbind(edg, edg2))
    } else if (!is.null(for.id[[x]]) & isFALSE(xy.list[[x]]$text[1] %in% c("'{'", "'}'"))) {
      if (!is.null(simple_assign[[x]])) {
        if (nrow(simple_assign[[x]]) == 1) {
          target.for = simple_assign[[x]]$source
          edg = make_edge(target = target.for, attr = "", source = trg, interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = simple_assign[[x]]$extra)

        } else {
          target.for = simple_assign[[x]]$target[1]
          edg = make_edge(target = target.for, attr = "", source = trg, interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = simple_assign[[x]]$extra[1])

        }
      } else if (!is.null(complex_assign[[x]])) {
        if (nrow(complex_assign[[x]]) == 1) {
          target.for = complex_assign[[x]]$target
          edg = make_edge(target = target.for, attr = "", source = trg, interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = complex_assign[[x]]$extra)

        } else {
          target.for = complex_assign[[x]]$target[1]
          edg = make_edge(target = target.for, attr = "", source = trg, interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = complex_assign[[x]]$extra[1])

        }

      } else if (!is.null(brackets[[x]])) {
        if (nrow(brackets[[x]]) == 1) {
          target.for = brackets[[x]]$target
          edg = make_edge(target = target.for, attr = "", source = trg, interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = brackets[[x]]$extra)

        } else {
          target.for = brackets[[x]]$target[1]
          edg = make_edge(target = target.for, attr = "", source = trg, interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = brackets[[x]]$extra[1])

        }

      } else {
        edg = NULL
      }
      id = as.numeric(strsplit(trg, "[.]")[[1]][2])
      n_tmp = which(xy.list[[id]]$token == "SYMBOL_FUNCTION_CALL")
      n = which(xy.list[[id]]$text[n_tmp] == "foreach")
      n = c(n, which(grepl("apply", xy.list[[x]]$text[n_tmp])))
      if (length(n) > 0 & x != id) {
        if (sum((c("EQ_ASSIGN", "LEFT_ASSIGN") %in% xy.list[[x]]$token)) == 0 & sum((c("IF", "ELSE", "FOR") %in% xy.list[[x]]$token)) == 0 & xy.list[[x]]$token[1] != "SYMBOL_FUNCTION_CALL") {
          edg2 = make_edge(target = paste(xy.list[[x]]$text, collapse = ""), attr = "", source = paste("return", x, sep = "."), interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = "")
          edg3 = make_edge(target = paste("return", x, sep = "."), attr = "", source = trg, interaction = "LOOPcondition", weight = 1, id = x, cmd = paste(xy.list[[x]]$text, collapse = ""), all_groups = groups, edgtype = "LONG_DASH", extra = "")
          return(rbind(edg, edg2, edg3))
        }
        return(edg)
      } else {
        return(edg)
      }

    }
  })


  # make unique 1 character variable found in loop ( i in 1:10)
  res = lapply(seq_along(conditionloop), function(x) {
    if (!is.null(conditionloop[[x]]$target) & !is.null(conditionloop[[x]]$source)) {
      if (sum(nchar(conditionloop[[x]]$target) < 2)>0| sum(nchar(conditionloop[[x]]$source) < 2)>0) {
        len_char=nchar(conditionloop[[x]]$target) < 2
        if (sum(nchar(conditionloop[[x]]$target) < 2)>0) {
          id = as.numeric(strsplit(conditionloop[[x]]$source, "[.]")[[1]][2])
          i = which(nchar(conditionloop[[x]]$target) < 2)
          for (j in i) {
            if (conditionloop[[id]]$indices[j] == paste(conditionloop[[x]]$target[j], id, sep = ".")) {
              conditionloop[[x]]$target[j] <<- paste(conditionloop[[x]]$target[j], id, sep = ".")
            }
          }
        } else {
          id = as.numeric(strsplit(conditionloop[[x]]$target, "[.]")[[1]][2])
          i = which(nchar(conditionloop[[x]]$source) < 2)
          for (j in i) {
            if (conditionloop[[id]]$indices[j] == paste(conditionloop[[x]]$source[j], id, sep = ".")) {
              conditionloop[[x]]$source[j] <<- paste(conditionloop[[x]]$source[j], id, sep = ".")
            }
          }
        }


        if (!is.null(simple_assign[[x]]$target) & !is.null(simple_assign[[x]]$source)) {
          if (sum(nchar(simple_assign[[x]]$target) < 2 | nchar(simple_assign[[x]]$source) < 2) > 0) {
            if (sum(nchar(simple_assign[[x]]$target) < 2) > 0) {
              if (length(nchar(simple_assign[[x]]$target) < 2) > 1) {
                i = which(nchar(simple_assign[[x]]$target) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices[j] == paste(simple_assign[[x]]$target[j], id, sep = ".")) {
                  simple_assign[[x]]$target[j] <<- paste(simple_assign[[x]]$target[j], id, sep = ".")
                }
              }

            } else {
              if (length(nchar(simple_assign[[x]]$source) < 2) > 1) {
                i = which(nchar(simple_assign[[x]]$source) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices[j] == paste(simple_assign[[x]]$source[j], id, sep = ".")) {
                  simple_assign[[x]]$source[j] <<- paste(simple_assign[[x]]$source[j], id, sep = ".")
                }
              }
            }
          }
        }
        if (!is.null(complex_assign[[x]]$target) & !is.null(complex_assign[[x]]$source)) {
          if (sum(nchar(complex_assign[[x]]$target) < 2 | nchar(complex_assign[[x]]$source) < 2) > 0) {
            if (sum(nchar(complex_assign[[x]]$target) < 2) > 0) {
              if (length(nchar(complex_assign[[x]]$target) < 2) > 1) {
                i = which(nchar(complex_assign[[x]]$target) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices[j] == paste(complex_assign[[x]]$target[j], id, sep = ".")) {
                  complex_assign[[x]]$target[j] <<- paste(complex_assign[[x]]$target[j], id, sep = ".")
                }
              }

            } else {
              if (length(nchar(complex_assign[[x]]$source) < 2) > 1) {
                i = which(nchar(complex_assign[[x]]$source) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices[j] == paste(complex_assign[[x]]$source[j], id, sep = ".")) {
                  complex_assign[[x]]$source[j] <<- paste(complex_assign[[x]]$source[j], id, sep = ".")
                }
              }
            }
          }
        }
        if (!is.null(dollars[[x]]$target) & !is.null(dollars[[x]]$source)) {
          if (sum(nchar(dollars[[x]]$target) < 2 | nchar(dollars[[x]]$source) < 2) > 0) {
            if (sum(nchar(dollars[[x]]$target) < 2) > 0) {
              if (length(nchar(dollars[[x]]$target) < 2) > 1) {
                i = which(nchar(dollars[[x]]$target) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices[j] == paste(dollars[[x]]$target[j], id, sep = ".")) {
                  dollars[[x]]$target[j] <<- paste(dollars[[x]]$target[j], id, sep = ".")
                }
              }

            } else {
              if (length(nchar(dollars[[x]]$source) < 2) > 1) {
                i = which(nchar(dollars[[x]]$source) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices[j] == paste(dollars[[x]]$source[j], id, sep = ".")) {
                  dollars[[x]]$source[j] <<- paste(dollars[[x]]$source[j], id, sep = ".")
                }
              }
            }
          }
        }
        if (!is.null(brackets[[x]]$target) & !is.null(brackets[[x]]$source)) {
          if (sum(nchar(brackets[[x]]$target) < 2 | nchar(brackets[[x]]$source) < 2) > 0) {
            if (sum(nchar(brackets[[x]]$target) < 2) > 0) {
              if (length(nchar(brackets[[x]]$target) < 2) > 1) {
                i = which(nchar(brackets[[x]]$target) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices[j] == paste(brackets[[x]]$target[j], id, sep = ".")) {
                  brackets[[x]]$target[j] <<- paste(brackets[[x]]$target[j], id, sep = ".")
                }
              }

            } else {
              if (length(nchar(brackets[[x]]$source) < 2) > 1) {
                i = which(nchar(brackets[[x]]$source) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices[j] == paste(brackets[[x]]$source[j], id, sep = ".")) {
                  brackets[[x]]$source[j] <<- paste(brackets[[x]]$source[j], id, sep = ".")
                }
              }
            }
          }
        }
        if (!is.null(conditionifs[[x]]$target) & !is.null(conditionifs[[x]]$source)) {
          if (sum(nchar(conditionifs[[x]]$target) < 2 | nchar(conditionifs[[x]]$source) < 2) > 0) {
            if (sum(nchar(conditionifs[[x]]$target) < 2) > 0) {
              if (length(nchar(conditionifs[[x]]$target) < 2) > 1) {
                i = which(nchar(conditionifs[[x]]$target) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices[j] == paste(conditionifs[[x]]$target[j], id, sep = ".")) {
                  conditionifs[[x]]$target[j] <<- paste(conditionifs[[x]]$target[j], id, sep = ".")
                }
              }

            } else {
              if (length(nchar(conditionifs[[x]]$source) < 2) > 1) {
                i = which(nchar(conditionifs[[x]]$source) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices[j] == paste(conditionifs[[x]]$source[j], id, sep = ".")) {
                  conditionifs[[x]]$source[j] <<- paste(conditionifs[[x]]$source[j], id, sep = ".")
                }
              }
            }
          }
        }
        if (!is.null(conditionelseifs[[x]]$target) & !is.null(conditionelseifs[[x]]$source)) {
          if (sum(nchar(conditionelseifs[[x]]$target) < 2 | nchar(conditionelseifs[[x]]$source) < 2) > 0) {
            if (sum(nchar(conditionelseifs[[x]]$target) < 2) > 0) {
              if (length(nchar(conditionelseifs[[x]]$target) < 2) > 1) {
                i = which(nchar(conditionelseifs[[x]]$target) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices[j] == paste(conditionelseifs[[x]]$target[j], id, sep = ".")) {
                  conditionelseifs[[x]]$target[j] <<- paste(conditionelseifs[[x]]$target[j], id, sep = ".")
                }
              }

            } else {
              if (length(nchar(conditionelseifs[[x]]$source) < 2) > 1) {
                i = which(nchar(conditionelseifs[[x]]$source) < 2)
              } else {
                i = 1
              }
              for (j in i) {
                if (conditionloop[[id]]$indices == paste(conditionelseifs[[x]]$source[j], id, sep = ".")) {
                  conditionelseifs[[x]]$source[j] <<- paste(conditionelseifs[[x]]$source[j], id, sep = ".")
                }
              }
            }
          }
        }

      }
    }
  })

  # create edges for ifelse group

  conditionifelse = lapply(seq_along(xy.list), FUN = function(x) {
    n_tmp = which(xy.list[[x]]$token == "SYMBOL_FUNCTION_CALL")
    n = which(xy.list[[x]]$text[n_tmp] == "ifelse")
    inds_coma = which(xy.list[[x]]$token == "','")
    if (length(n) > 0 & n_tmp[1] == 3 & length(inds_coma) == 2) {
      cond.ind = c(5:(inds_coma[1] - 1))
      vars_cond = xy.list[[x]]$text[cond.ind][which(xy.list[[x]]$token[cond.ind] %in% c("SYMBOL", "NUM_CONST", "STR_CONST"))]

      attr_if = paste(xy.list[[x]]$text[cond.ind], collapse = "")

      src.if = paste("IF", tail(groups[[x]], 1), sep = ".")
      condtk = xy.list[[x]]$token[(n_tmp[n] + 2):(inds_coma[1] - 1)]
      condtxt = xy.list[[x]]$text[(n_tmp[n] + 2):(inds_coma[1] - 1)]
      x1 = brackets[[x]]$cmd
      x2 = dollars[[x]]$cmd
      trg <<- paste("IF", x, sep = ".")
      if ((!is.null(x1))) {
        trg1 = gregexpr("(\\b\\w+\\b)|\\W", x1, perl = TRUE)
        src = regmatches(x1, trg1)[[1]]
        target = brackets[[x]]$target
        found = match(src, condtxt)
        in_cnd = all_nodes[[x]][-c(which(all_nodes[[x]] %in% src))][-c(1)]
      } else if ((!is.null(x2))) {
        trg1 = gregexpr("(\\w{0,}[\\.]{0,}\\w{0,})|(\\W)(?<![\\.])+", x2, perl = TRUE)
        attr(trg1[[1]], "match.length") = ifelse(attr(trg1[[1]], "match.length") == 0, 1, attr(trg1[[1]], "match.length"))
        src = regmatches(x2, trg1)[[1]]
        target = dollars[[x]]$target
        found = match(src, condtxt)
        in_cnd = all_nodes[[x]][-c(which(all_nodes[[x]] %in% src))][-c(1)]
      } else if (length(condtxt) > 1) {
        in_cnd = c()
        found = c()
        target = unique(all_nodes[[x]])
      } else {
        return(NULL)
      }
      extra = ifelse(is.null(brackets[[x]]$extra[1]), "", brackets[[x]]$extra[1])
      attr = attr_if

      IF.node = paste("IF", x, sep = ".")
      ELSE.node = paste("ELSE", x, sep = ".")

      src1 = xy.list[[x]]$text[1]

      ind_par = sort(which(xy.list[[x]]$token %in% c("LBB", "'['", "']'")))
      if (length(ind_par) == 5) {
        i = match(which(xy.list[[x]]$token %in% c("LBB")), ind_par)
        j = ind_par[-c(i:(i + 2))]
        brck1 = paste(xy.list[[x]]$text[ind_par[i]:ind_par[i + 2]], collapse = "")
        brck1 = c(brck1, paste(xy.list[[x]]$text[j[1]:j[2]], collapse = ""))
        if (!is.null(target_vars)) {
          vars_cond = brck1[-c(which(brck1 %in% target_vars))]
        }

      } else if (length(ind_par) == 3) {
        brck1 = paste(xy.list[[x]]$text[ind_par[i]:ind_par[i + 2]], collapse = "")
      } else if (length(ind_par) == 2) {
        brck1 = paste(xy.list[[x]]$text[i[1]:i[2]], collapse = "")
      }



      if.inds = inds_coma[1]:(inds_coma[2] - 1)
      else.inds = inds_coma[2]:(length(xy.list[[x]]$text) - 1)
      res_if = xy.list[[x]]$text[if.inds][which(xy.list[[x]]$token[if.inds] %in% c("SYMBOL", "NUM_CONST", "STR_CONST"))]
      res_else = xy.list[[x]]$text[else.inds][which(xy.list[[x]]$token[else.inds] %in% c("SYMBOL", "NUM_CONST", "STR_CONST"))]

      edg0 = make_edge(target = ELSE.node, attr = "", source = IF.node, interaction = "IFELSEcondition", weight = 1, id = x, cmd = "", all_groups = groups)
      edg1 = make_edge(target = IF.node, attr = attr_if, source = vars_cond, interaction = "IFELSEcondition", weight = 1, id = x, cmd = "", all_groups = groups, extra = extra)
      edg1.pass = make_edge(target = res_if, attr = "", source = IF.node, interaction = "IFELSEcondition", weight = 1, id = x, cmd = "", all_groups = groups, edgtype = "LONG_DASH")
      edg1.pass.2 = make_edge(target = src1, attr = "=", source = res_if, interaction = "IFELSEcondition", weight = 1, id = x, cmd = "", all_groups = groups, edgtype = "LONG_DASH")

      edg2.pass = make_edge(target = res_else, attr = "", source = ELSE.node, interaction = "IFELSEcondition", weight = 1, id = x, cmd = "", all_groups = groups, edgtype = "LONG_DASH")
      edg2.pass.2 = make_edge(target = src1, attr = "=", source = res_else, interaction = "IFELSEcondition", weight = 1, id = x, cmd = "", all_groups = groups, edgtype = "LONG_DASH")


      return(rbind(edg0, edg1, edg1.pass, edg1.pass.2, edg2.pass, edg2.pass.2))
    }

  })


  # fin indices where command is special %in% %<%
  spequery = lapply(seq_along(xy.list), FUN = function(x) {
    m = which(xy.list[[x]]$token %in% c("SPECIAL"))
    if (length(m) > 0) {
      return(x)
    }
  })


  #check table is a list of various object describing some elements found for each line
  # check_table=cbind(all_nodes,#variable found in the command - variable name (char) should be nodes
  #                   strtfunc,#is there some function that start the command (stop(), print()) - function name (char)
  #                   right2eq,#what's right after the equal assign - token
  #                   brackets,#decomposed some_variable[whats in here] - edges
  #                   dollars,#decomposed some_variable$what_shere - edges
  #                   regexp_found,#are there some function that calls patterns arguments[] - Boolean
  #                   conditionelseifs,# condition of elseifs - edges
  #                   conditionifs,# conditions for ifs - edges
  #                   if.id, #for groups - edges
  #                   elseif.id,
  #                   else.id, #for groups - list ids
  #                   forquery, #for groups - list ids
  #                   spequery,#list special characters percentinpercent - Boolean
  #                   R_func,# transform to list extract agruments (eq_sub) - function names (char)
  #                   usr_func, #for groups - function names (char)
  #                   simple_assign,# var1 = ... - edges
  #                   complex_assign, # var1[someting] = ... - edges
  #                   openpar, #index of opening and closing () for each function, - df (indexes)
  #                   openbrk, #index of opening and closing [] for each command - df (indexes)
  #                   groups, #where are we in the script, within which {} group - char
  #                   target_vars, #if command is nested which variable is sought using $ or [ - char
  #                   target_vars2 #second level of nesting using ( - char
  # )
  # symb_package list index of ::


  #remove duplicate edges in dollars
  dollars[sapply(dollars, is.null)] <- NA
  d = unlist(lapply(dollars, "[", 6))
  ids_full = unlist(lapply(dollars, "[", 7))
  ids = ids_full[which(!(is.na(d)) & duplicated(d))]
  dollars = lapply(seq_along(dollars), function(x) {
    if (isTRUE(is.na(dollars[[x]]))) {
      return(NULL)
    } else {
      to_remove = match(ids, dollars[[x]]$id)
      to_remove = to_remove[!is.na(to_remove)]
      if (length(to_remove) > 0) {
        if (dim(dollars[[x]][-c(to_remove), ])[1] == 0) {
          return(NULL)
        } else {
          return(dollars[[x]][-c(to_remove), ])
        }


      } else {
        return(dollars[[x]])
      }
    }
  })

  message('Script processed')
  #assemble all edges processed
  all_edges_list = c(strtfunc, brackets, dollars, conditionifs, conditionelseifs, elsequery, conditionloop, conditionifelse, simple_assign, complex_assign)
  all_edges = bind_rows(all_edges_list)

  if (!is.null(pkg_edges)) {
    all_edges = rbind(all_edges, pkg_edges)
  }
  all_edges = all_edges[order(as.numeric(all_edges$id)), ]
  all_edges$id = make.unique(all_edges$id)

  i = which(duplicated(apply(all_edges[, seq_len(3)], 1, function(x) {
    paste(x, collapse = "")
  })))
  all_edges = all_edges[-c(i), ]




  #check all possible nodes that can be a duplicated subset to make.unique
  #ex: multiple [1] indice on different variable should produce multiple [1] nodes
  to_monitor_tmp1 = which(all_edges$interaction == "subset")
  to_monitor_tmp2 = which(all_edges$interaction == "holds")
  to_monitor_tmp3 = which(all_edges$interaction == "IFcondition")
  to_monitor_tmp4 = which(all_edges$interaction == "ELSEIFcondition")
  to_monitor_tmp5 = which(all_edges$interaction == "ELSEcondition")
  to_monitor_tmp6 = which(all_edges$interaction == "LOOPcondition")
  to_monitor_tmp7 = which(all_edges$interaction == "IFELSEcondition")


  to_monitor = c(to_monitor_tmp1, to_monitor_tmp2, to_monitor_tmp3, to_monitor_tmp4, to_monitor_tmp5, to_monitor_tmp6, to_monitor_tmp7)
  to_monitor_tmp = to_monitor
  to_change = unique(c(all_edges[to_monitor_tmp1, ]$source, all_edges[to_monitor_tmp1, ]$target, all_edges[to_monitor_tmp2, ]$target, all_edges[to_monitor_tmp2, ]$source, all_edges[to_monitor_tmp3, ]$target, all_edges[to_monitor_tmp3, ]$source, all_edges[to_monitor_tmp4, ]$target, all_edges[to_monitor_tmp4, ]$source, all_edges[to_monitor_tmp5, ]$target, all_edges[to_monitor_tmp5, ]$source, all_edges[to_monitor_tmp6, ]$target, all_edges[to_monitor_tmp6, ]$source, all_edges[to_monitor_tmp7, ]$target,
                       all_edges[to_monitor_tmp7, ]$source))
  to_change = to_change[c(which(startsWith(to_change, "[")))]

  verif = data.frame(pattern = character(), source = character(), new_name = character(), cmd = character(), nesting = character(),stringsAsFactors = FALSE)

  c = 1
  while (length(to_monitor) > 0) {
    verif[c - 1, ]
    i = to_monitor[1]
    target = all_edges[i, ]$target
    source = all_edges[i, ]$source
    extra = all_edges[i, ]$extra
    cm = all_edges[i, ]$cmd
    orig.cmd = all_edges[i, ]$cmd
    if (extra != "") {
      cmd = paste(extra, target, sep = ".")
    } else {
      cmd = paste(source, target, sep = ".")
    }
    nesting = paste(extra, source, sep = ".")
    nesting2 = paste(extra, target, sep = ".")

    if ((target %in% to_change) && (source %in% to_change)) {
      if (nesting %in% verif$nesting) {
        if (isFALSE(nesting2 %in% verif$nesting)) {
          new_name = paste(target, i, sep = ".")
        } else {
          id = which(verif$nesting %in% nesting2)
          new_name = verif$new_name[id]
        }

        id = which(verif$nesting %in% nesting)
        nesting = paste(nesting, target, sep = ".")
        verif[c, ] = c(target, source, new_name, paste(cmd, id, sep = ""), nesting)
        all_edges[i, ]$source = verif$new_name[id]
        all_edges[i, ]$target = new_name

        to_monitor = to_monitor[-c(1)]
        c = c + 1
        next
      } else {
        stop(c)
      }

    }
    if (target %in% to_change) {
      if (isFALSE(cmd %in% verif$cmd) & isFALSE(nesting %in% verif$cmd) & isFALSE(nesting %in% verif$nesting)) {
        new_name = paste(target, i, sep = ".")
        verif[c, ] = c(target, source, new_name, cmd, nesting)
        all_edges[i, ]$target = new_name
      } else if (nesting %in% verif$nesting) {
        id = which(verif$nesting %in% nesting)[1]
        if (verif$pattern[id] == target) {
          verif[c, ] = c(target, source, verif$new_name[id], paste(cmd, id, sep = ""), nesting)
          all_edges[i, ]$target = verif$new_name[id]
        } else {
          new_name = paste(target, i, sep = ".")
          verif[c, ] = c(target, source, new_name, cmd, nesting)
          all_edges[i, ]$target = new_name
        }
      } else {
        id = which(verif$cmd %in% cmd)
        if (length(id) == 0) {
          id = which(verif$cmd %in% nesting)
        }
        verif[c, ] = c(target, source, verif$new_name[id], paste(cmd, id, sep = ""), nesting)
        all_edges[i, ]$target = verif$new_name[id]

      }

    } else if (source %in% to_change) {
      if (isFALSE(cmd %in% verif$cmd) & isFALSE(nesting %in% verif$cmd) & isFALSE(nesting %in% verif$nesting)) {
        new_name = paste(source, i, sep = ".")
        verif[c, ] = c(source, target, paste(source, i, sep = "."), cmd, nesting)
        all_edges[i, ]$source = new_name
      } else if (nesting %in% verif$nesting) {
        id = which(verif$nesting %in% nesting)[1]
        verif[c, ] = c(source, target, verif$new_name[id], paste(cmd, id, sep = ""), nesting)
        all_edges[i, ]$source = verif$new_name[id]
      } else {
        id = which(verif$cmd %in% cmd)
        if (length(id) == 0) {
          id = which(verif$cmd %in% nesting)
        }
        verif[c, ] = c(source, target, verif$new_name[id], paste(cmd, id, sep = ""), nesting)
        all_edges[i, ]$source = verif$new_name[id]
      }
    }

    if (isFALSE(source %in% to_change) && isFALSE(target %in% to_change)) {
      verif[c, ] = c("", "", "", "", "")
    }


    to_monitor = to_monitor[-c(1)]
    c = c + 1

  }
  dim(verif)

  ### unique BOOLEAN
  i1 = which(grepl("FALSE|\\bF\\b|TRUE|\\bT\\b", c(all_edges$source), perl = TRUE))
  i2 = which(grepl("FALSE|\\bF\\b|TRUE|\\bT\\b", c(all_edges$target), perl = TRUE))
  all_edges$source[i1] = paste(all_edges$source[i1], floor(as.numeric(all_edges$id[i1])), sep = ".")
  all_edges$target[i2] = paste(all_edges$target[i2], floor(as.numeric(all_edges$id[i2])), sep = ".")

  ### unique indices not treated in conditionloop
  if (length(to_monitor_tmp) > 0) {
    for (i in seq_len(length(to_monitor_tmp))) {
      target = all_edges[to_monitor_tmp[i], ]$target
      source = all_edges[to_monitor_tmp[i], ]$source
      # print(floor(as.numeric(all_edges$id[[to_monitor_tmp[i]]])))
      i1 = if.id[[floor(as.numeric(all_edges$id[[to_monitor_tmp[i]]]))]]
      i2 = elseif.id[[floor(as.numeric(all_edges$id[[to_monitor_tmp[i]]]))]]
      i3 = else.id[[floor(as.numeric(all_edges$id[[to_monitor_tmp[i]]]))]]
      i4 = for.id[[floor(as.numeric(all_edges$id[[to_monitor_tmp[i]]]))]]

      if (!is.null(i1) | !is.null(i2) | !is.null(i3) | !is.null(i4)) {
        # case within if reverse edge direction print(i)

        if (sum(grepl("IF|ELSE|FOR", c(source, target), perl = TRUE)) < 1 & sum(c(target, source) %in% verif$new_name) > 0) {

          all_edges[to_monitor_tmp[i], ]$reverse.edg = TRUE
          all_edges[to_monitor_tmp[i], ]$edgtype = "LONG_DASH"

        } else if (sum(grepl("IF|ELSE|FOR", c(source, target), perl = TRUE)) < 1) {
          all_edges[to_monitor_tmp[i], ]$edgtype = "LONG_DASH"

        }
      }
    }

  }
  #identify user function to rename nodes accordingly
  fun_name = gsub("f\\(|\\)[.].*", "", unlist(lapply(usr_func, "[", 1)))

  res = lapply(seq_along(all_edges[, 1]), function(x) {
    if (as.numeric(all_edges[x, ]$id) > 0) {
      if (!is.na(as.numeric(strsplit(all_edges$group_attr[x], ",")[[1]][1])) & length(fun_name) > 0) {

        # print(x)
        trg = all_edges[x, ]$target
        src = all_edges[x, ]$source
        trg_tmp = trg
        src_tmp = src
        grp = all_edges$group_attr[x]
        if (trg %in% verif$new_name | src %in% verif$new_name) {
          trg = gsub("\\[|\\].*", "", trg)
          src = gsub("\\[|\\].*", "", src)
        }
        args_fun = usr_func[[as.numeric(strsplit(grp, ",")[[1]][1])]]
        args_fun_subbed = gsub("f\\(\\b.*\\b\\)[.]", "", args_fun)
        fun_name = gsub("f\\(|\\)[.].*", "", args_fun[1])
        if (grepl("^(\\bIF\\b|\\bELSE\\b)(?=[.]{0,}\\d{0,})", trg, perl = TRUE)) {
          all_edges[x, ]$target <<- paste("f(", fun_name, ").", trg, sep = "")
        }
        if (grepl("^(\\bIF\\b|\\bELSE\\b)(?=[.]{0,}\\d{0,})", src, perl = TRUE)) {
          all_edges[x, ]$source <<- paste("f(", fun_name, ").", src, sep = "")

        }
        if (!is.null(args_fun)) {
          if (trg %in% args_fun_subbed) {
            ind_trg = which(args_fun_subbed %in% trg)
            trg = trg_tmp
            all_edges[x, ]$target <<- args_fun[ind_trg]
          } else if (isFALSE(grepl("^(\\bIF\\b|\\bELSE\\b)(?=[.]{0,}\\d{0,})", trg, perl = TRUE))) {
            all_edges[x, ]$target <<- paste("f(", fun_name, ").", trg_tmp, sep = "")
          }
          if (src %in% args_fun_subbed) {
            ind_src = which(args_fun_subbed %in% src)
            src = src_tmp
            all_edges[x, ]$source <<- args_fun[ind_src]
          } else if (isFALSE(grepl("^(\\bIF\\b|\\bELSE\\b)(?=[.]{0,}\\d{0,})", src, perl = TRUE))) {
            all_edges[x, ]$source <<- paste("f(", fun_name, ").", src_tmp, sep = "")
          }

        }
      }
    }
  })
  #####
  #####

  #identifying final variables for node color coding
  c = 1
  l = length(unique(c(all_edges$target, all_edges$source)))
  name = unique(c(all_edges$target, all_edges$source))
  final_vars1 = unique(unlist(lapply(simple_assign, "[", 1))[which(!unlist(lapply(simple_assign, "[", 1)) %in% unique(c(unlist(lapply(brackets, "[", 3)), unlist(lapply(dollars, "[", 3)), unlist(lapply(simple_assign, "[", 3)))))])
  final_vars2 = unique(unlist(lapply(conditionloop, "[", 1))[which(!unlist(lapply(conditionloop, "[", 1)) %in% unique(c(unlist(lapply(brackets, "[", 3)), unlist(lapply(dollars, "[", 3)), unlist(lapply(simple_assign, "[", 3)), unlist(lapply(complex_assign, "[", 3)))) & grepl("FOR.*", unlist(lapply(conditionloop, "[", 1))) == FALSE)])
  final_vars3 = unique(unlist(lapply(conditionifelse, "[", 1))[which(!unlist(lapply(conditionifelse, "[", 1)) %in% unique(c(unlist(lapply(brackets, "[", 3)), unlist(lapply(dollars, "[", 3)), unlist(lapply(simple_assign, "[", 3)), unlist(lapply(complex_assign, "[", 3)), unlist(lapply(conditionifelse, "[", 3)))) & grepl("ELSE.*|IF.*", unlist(lapply(conditionifelse, "[", 1))) == FALSE)])
  final_vars = c(final_vars1, final_vars2, final_vars3)

  fnc = c(unique(unlist(lapply(strtfunc, "[", 3))), "return")  # can be implicit

  #node color coding
  nodecol = lapply(seq_along(name), function(x) {
    if (name[x] %in% unique(unlist(lapply(dollars, "[", 1))) | startsWith(name[x], "[")) {
      return("#FADADD")  #pink
    } else if (name[x] %in% c("Session", "Packages", "WorkingDir")) {
      return("#000000")  #black
    } else if (grepl("(\\bIF\\b|\\bELSE\\b)(?=[.]{0,}\\d{0,})", name[x], perl = TRUE)) {
      return("#FED000")  #yellow
    } else if (grepl("(\\bFOR\\b|\\bWHILE\\b)(?=[.]{0,}\\d{0,})", name[x], perl = TRUE)) {
      return("#ff6633")  #orange
    } else if (grepl("(\\bmessage\\b|\\bstop\\b|\\bwarning\\b)(?=[.]{0,}\\d{0,})", name[x], perl = TRUE)) {
      return("#FF3333")  #red
    } else if (grepl("(\\b[a-z]?print\\b|\\cat\\b)(?=[.]{0,}\\d{0,})", name[x], perl = TRUE)) {
      return("#89D0F5")  #blue
    } else if (grepl(paste("(", paste(paste("\\b", gsub("[.].*", "", fnc, perl = TRUE), "\\b", sep = ""), collapse = "|"), "(?=[.]{0,}\\d{0,}))", sep = ""), name[x], perl = TRUE)) {
      return("#89D0F5")  #blue
    } else if (name[x] %in% final_vars | grepl("^init(?=[.]{0,}\\d{0,})", name[x], perl = TRUE)) {
      return("#669999")  #green
    } else if (name[x] %in% pkg_edges$target & name[x] != "Packages") {
      return("#CCCCCC")  #grey
    } else if (name[x] %in% unlist(lapply(strtfunc, "[", 3))) {
      return("#8cd3ff")  #blue
    } else {
      return("#FFFFFF")
    }  #white
  })

  #specific node border width for white nodes
  nodecol = unlist(nodecol)
  nodes_border = ifelse(nodecol == "#FFFFFF", "#000000", nodecol)
  nodes_label_color = ifelse(nodecol == "#000000", "#FFFFFF", "#000000")
  nodes = data.frame(id = seq_len(l), name = unique(c(all_edges$target, all_edges$source)), colors = nodecol, borders = nodes_border, label_font_color = nodes_label_color,stringsAsFactors = FALSE)
  all_edges$extra = all_edges$target


  # links between indices
  nodes_names = gsub("(?<!\\))[.](?=[0-9]).*", "", nodes[, 2], perl = TRUE)
  rep_ids = which(grepl("\\[{1,2}|\\]{1,2}", nodes_names))
  ind_inds = which(gsub("\\[|\\]", "", nodes_names[rep_ids]) %in% nodes_names & nchar(gsub("\\[|\\]", "", nodes_names[rep_ids])) > 1)
  targets = nodes[rep_ids[ind_inds], 2]
  sources = gsub("\\[|\\]", "", nodes_names[rep_ids][ind_inds])
  if (length(c(targets, sources)) > 2) {
    edg = make_edge(target = targets, attr = "", source = sources, interaction = "linked", weight = "1", cmd = "", id = 0, all_groups = groups)
    edg$edgtype = "SINEWAVE"
    # [1] 'PARALLEL_LINES' 'LONG_DASH' 'VERTICAL_SLASH' 'DASH_DOT' 'DOT' 'ZIGZAG' [7] 'FORWARD_SLASH' 'EQUAL_DASH' 'SINEWAVE' 'MARQUEE_DASH_DOT' 'BACKWARD_SLASH' 'CONTIGUOUS_ARROW' [13] 'SEPARATE_ARROW' 'MARQUEE_EQUAL' 'SOLID' 'MARQUEE_DASH'

    all_edges = rbind(edg, all_edges)
  }
  c = 1

  #rename nodes as id to avoid duplicated node name error
  apply(all_edges, MARGIN = 1, function(x) {
    y = nodes[match(x[c(1, 3)], nodes$name), 1]
    if (sum(is.na(y)) > 0) {
      stop("node not in network .... crap")
    } else {
      all_edges[c, c(1, 3)] <<- y
    }
    c <<- c + 1

  })

  nodes = apply(nodes, 2, as.character)
  nodes[, 1] = gsub(" ", "", nodes[, 1])

  ########## Remove .digits in node attributes names that were placed for identification
  nodes[, 2] = gsub("(?<!\\))[.](?=[0-9]).*", "", nodes[, 2], perl = TRUE)


  edgecol <- colorRampPalette(c("lightgrey", "lightblue", "steelblue1", "steelblue2", "steelblue3", "steelblue4", "darkblue", "black"))(nrow(all_edges))

  #reformating edge table for Cytoscape
  all_edges = as.data.frame(all_edges)
  all_edges = all_edges[, c(1:4, 8, 5:7, 9:12)]
  all_edges$edge_lgth = as.integer(all_edges$edge_lgth)
  all_edges$edgecol = edgecol

  #preparing network
  collection = "ScriptMapR"
  title = paste(script_name, as.POSIXct(Sys.time()))
  style.name = gsub(".R", "", basename(script_name))
  style.name = "ScriptMapR"
  ids = getNetworkList()[which(grepl(script_name, getNetworkList(), perl = TRUE))]
  lapply(seq_along(ids), function(x) {
    deleteNetwork(ids[x])
  })

  #reset default style
  # commandsPOST("vizmap apply styles=\"default\"")
  #creating network
  diplayed = tryCatch({
    createNetworkFromDataFrames(nodes, all_edges, title = title, collection = collection)
    diplayed=0
  }, error = function(e) {
    message('Cytoscape connection error, please retry.')
    diplayed=1

  })
  if (diplayed==1){
    return(NULL)
  }
  message("Network created")

  nview=getNetworkViews(network = title)
  mappings = list(
    nodeLabels = mapVisualProperty(visual.prop = "NODE_LABEL", table.column = "name", mapping.type = "p"),
    nodecolor = mapVisualProperty(visual.prop = "NODE_FILL_COLOR", table.column = "colors", mapping.type = "p"),
    nodeborder = mapVisualProperty(visual.prop = "NODE_BORDER_PAINT", table.column = "borders", mapping.type = "p"),
    nodeftcol = mapVisualProperty(visual.prop = "NODE_LABEL_COLOR", table.column = "label_font_color", mapping.type = "p"),
    edgelabel = mapVisualProperty(visual.prop = "EDGE_LABEL", table.column = "attr", mapping.type = "p"),
    edgecolor = mapVisualProperty(visual.prop = "EDGE_STROKE_UNSELECTED_PAINT", table.column = "edgecol", mapping.type = "p"),
    edgtype = mapVisualProperty(visual.prop = "EDGE_LINE_TYPE", table.column = "edgtype", mapping.type = "p"))

  def = list(NODE_BORDER_WIDTH = 4, EDGE_LABEL_COLOR = "#6600CC", EDGE_TARGET_ARROW_SHAPE = "ARROW", NODE_SHAPE = "ROUND_RECTANGLE")
  message("Network mappings set")
  if (style.name %in% getVisualStyleNames()) {
    message("Network mappings style exists")
    setVisualStyle(style.name, network = title)
    res = lapply(seq_along(mappings), function(x) {
      updateStyleMapping(style.name, mappings[[x]])
    })
    updateStyleDefaults(style.name, def)
    message("Network mappings done")

  } else {
    message("Network mappings style create")
    l = length(getVisualPropertyNames())
    vp_names = getVisualPropertyNames()
    cy_defaults = lapply(seq_len(l), function(x) {
      getVisualPropertyDefault(property = vp_names[x], style.name = "default")
    })
    names(cy_defaults) = vp_names
    defaults = c(cy_defaults[which(!(names(cy_defaults) %in% names(def)))], def)
    createVisualStyle(style.name, defaults = defaults, mappings = mappings)
    lockNodeDimensions(FALSE, style.name)
    message("Network mappings done")

  }

  # layoutNetwork("kamada-kawai nodeList='all' edgeAttribute=edge_lgth m_disconnectedNodeDistanceSpringRestLength=1200 m_nodeDistanceStrengthConstant=150 m_averageIterationsPerNode=100")


  #preparing edge color coding
  fun_name = gsub("f\\(|\\)[.].*", "", unlist(lapply(usr_func, "[", 1)))
  inds = seq(1, 100, 100/length(fun_name))
  group_colors <- colorRampPalette(c("lightgrey", "black"))(nrow(all_edges))[inds]
  nodedata <- getTableColumns("node")
  edgedata <- getTableColumns("edge")
  message("Getting table data")
  # apply reverse ifs else if reorder HARDCODED RESHAPE OF ALLEDGES
  if (length(which(edgedata$reverse.edg == TRUE))>0){
    selectEdges(edgedata$SUID[which(edgedata$reverse.edg == TRUE)])
    setEdgeTargetArrowShapeBypass(edge.names = edgedata$`shared name`[which(edgedata$reverse.edg == TRUE)], new.shapes = "NONE", network = title)
    setEdgeSourceArrowShapeBypass(edge.names = edgedata$`shared name`[which(edgedata$reverse.edg == TRUE)], new.shapes = "ARROW", network = title)
    clearSelection()
  }
  message("Reversing edges")
  if (length(which(edgedata$edgtype == "LONG_DASH")) > 0) {
    dashed = selectEdges(edgedata$SUID[which(edgedata$edgtype == "LONG_DASH")])
  }
  if (length(which(edgedata$edgtype == "SINEWAVE")) > 0) {
    sinewave = selectEdges(edgedata$SUID[which(edgedata$edgtype == "SINEWAVE")])
  }

  #bypass if edges colors
  s=selectNodes(c("IF", "ELSE IF", "ELSE"), by.col = "name")
  conds.ifs = selectEdgesAdjacentToSelectedNodes()
  message("color bypass start")

  if (length(conds.ifs) > 0 & length(conds.ifs$edges) > 0) {
    conds.ifs$edges = conds.ifs$edges[-c(which(conds.ifs$edges %in% dashed$edges))]
    setEdgeColorBypass(conds.ifs$edges, "#FED000")
  }
  clearSelection()
  message("color bypass done")

  #bypass loop edges colors
  s=selectNodes(c("FOR", "WHILE"), by.col = "name")
  conds.for = selectEdgesAdjacentToSelectedNodes()
  if (length(conds.for) > 0 & length(conds.for$edges) > 0) {
    conds.for$edges = conds.for$edges[-c(which(conds.for$edges %in% dashed$edges))]
    setEdgeColorBypass(conds.for$edges, "#ff6633")
  }
  clearSelection()

  nodedata <- getTableColumns("node")
  edgedata <- getTableColumns("edge")

  #creating groups from functions & extracting
  fun_name=unique(fun_name)
  if (length(fun_name) > 0) {
    for (f in seq_len(length(fun_name))) {
      namesSUIDs <- nodedata[grepl(fun_name[f], nodedata$name), 1]
      s=selectNodes(namesSUIDs, preserve.current.selection = FALSE)  #check for sub groups
      s=selectEdgesAdjacentToSelectedNodes()
      s=selectNodesConnectedBySelectedEdges()
      grp_suid = getSelectedNodes(node.suids = TRUE)

      createSubnetwork(grp_suid, subnetwork.name = paste("function: ", fun_name[f], sep = ""))
      setCurrentNetwork(title)
      grp_suid = createGroup(group.name = as.character(fun_name[f]), nodes = "selected")
      collapseGroup(fun_name[f])
      setNodeColorBypass(fun_name[f], group_colors[f])

    }
    # collapseGroup(listGroups()$groups)
    # s=selectNodes(listGroups()$groups, "SUID")
    # deleteSelectedNodes()
  }
  clearSelection()

  #change layout of session nodes
  #keep selected to remove out of the way
  if ("Session" %in% nodedata$name) {
    nt0 = 1
    nt1 = 1
    s=selectNodes("Session", by.col = "name", preserve.current.selection = TRUE)  #check for sub groups

    while (!(nt0 == nt1) | sum(nt0 + nt1) == 2) {
      nt0 = nt1
      s = selectEdgesAdjacentToSelectedNodes()
      s = selectNodesConnectedBySelectedEdges()
      nodetable = getTableColumns("node")
      nt1 = sum(nodetable$selected)
    }

    layoutNetwork("degree-circle nodeList=selected")

  }

  if (length(listGroups()$groups) > 0) {
    s=selectNodes(listGroups()$groups, "SUID", preserve.current.selection = T)
  }
  setCurrentView(nview)

}

