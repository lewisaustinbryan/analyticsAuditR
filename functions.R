#### REGEX RELIENT FUNCTIONS ####

pii_regex <- function() {
  pii <- c("cheese.com/mapple?email=lewis@google.com&postcode=m36JG&cheese=wensleydale", "bloop/beep?phone=07568706194", 
           "u8.decluttr.com/my-account/forgotten-password/",	
           "u8.decluttr.com/checkout/order-complete/"
           )
  pii_checker <- function(regex) {
    print(str_detect(pii, regex,))
    print(str_extract(pii, regex))
  }
  
  
  name <- "((firstname=)|(lastname=)|(surname=))[^&\\/\\?]+|(j(im(my)?|ohn|ames)|robert|bob(by)?|michael|dav(id|e)|(d|r)ic(k|hard)|ch(arl(es|ie)|uck)|mary|pat(ty|ricia)|linda|barb(ara)?|e?liz(zy|abeth)|jenn?(ifer)?|mari(e|a)|su(e|san)|sarah?)"
  pii_checker(name)
  
  email <- "([a-zA-Z0-9_\\.-]+)@([\\da-zA-Z\\.-]+)\\.([a-zA-Z\\.]{2,6})"
  pii_checker(email)  
  
  password <- "((password=)|(passwd=)|(pass=))[^&\\/\\?]+"
  
  sse <- "(\\d{3}-?\\d{2}-?\\d{4})"
  pii_checker(sse)
  
  address <- "(drive|street|road|dr\\.|po box|rd\\.)"
  pii_checker(address)
  
  postcode <- "((postcode=)|(zipcode=)|(zip=))[^&\\/\\?]+"
  
  phone <- "((\\d{3}-?\\d{3}-?\\d{4})|((tel=)|(telephone=)|(phone=)|(mobile=)|(mob=))[\\d\\+\\s][^&\\/\\?]+)"
  #pii_checker(phone) 
  
  pii_regex <- paste("\\?.*(", paste(name, email, password, sse, address, postcode, phone, sep = "|"), ")", sep ="")
  
  return(pii_regex)
}


payment_gateway_regex <- function() {
  "paypal|worldpay|authorize|shopify|securepay|payments|pay|fondy|klarna|checkout"
}

##### GET Website  
get_hostnames_from_url <- function(url) {
  pg <- read_html(url) # Read source html
  hostname <- append(html_attr(html_nodes(pg, "a"), "href"), html_attr(html_nodes(pg, "link"), "href")) # get hrefs from a and link nodes
  hostname <- str_replace(hostname,"^((?!(http(s)?:)?\\/\\/).)*", "") #remove links without http
  hostname <- str_replace_all(hostname,"(((http(s)?:)?\\/\\/(www\\.)?)|(\\/$)|\\/.*)|(\\?.*)", "") #clean up related domains 
  hostname <- str_replace(hostname, "(.*\\.)?(youtube|trustpilot|facebook|twitter|pinterest|myhermes|linkedin|instagram|dailymail|express|finance.yahoo|plus.google)(.co.uk|.com)", "") #remove social links 
  hostname <- hostname[hostname != ""]
  hostname <- unique(as.character(na.omit(hostname)))
  '
  #url <- "cheese.com"
  #country_code_top_level_domains <- c("\\.ac","\\.ad","\\.ae","\\.af","\\.ag","\\.ai","\\.al","\\.am","\\.ao","\\.aq","\\.ar","\\.as","\\.at","\\.au","\\.aw","\\.ax","\\.az","\\.ba","\\.bb","\\.bd","\\.be","\\.bf","\\.bg","\\.bh","\\.bi","\\.bj","\\.bm","\\.bn","\\.bo","\\.bq","\\.br","\\.bs","\\.bt","\\.bw","\\.by","\\.bz","\\.ca","\\.cc","\\.cd","\\.cf","\\.cg","\\.ch","\\.ci","\\.ck","\\.cl","\\.cm","\\.cn","\\.co","\\.cr","\\.cu","\\.cv","\\.cw","\\.cx","\\.cy","\\.cz","\\.de","\\.dj","\\.dk","\\.dm","\\.do","\\.dz","\\.ec","\\.ee","\\.eg","\\.er","\\.es","\\.et","\\.eu","\\.fi","\\.fj","\\.fk","\\.fm","\\.fo","\\.fr","\\.ga","\\.gd","\\.ge","\\.gf","\\.gg","\\.gh","\\.gi","\\.gl","\\.gm","\\.gn","\\.gp","\\.gq","\\.gr","\\.gs","\\.gt","\\.gu","\\.gw","\\.gy","\\.hk","\\.hm","\\.hn","\\.hr","\\.ht","\\.hu","\\.id","\\.ie","\\.il","\\.im","\\.in","\\.io","\\.iq","\\.ir","\\.is","\\.it","\\.je","\\.jm","\\.jo","\\.jp","\\.ke","\\.kg","\\.kh","\\.ki","\\.km","\\.kn","\\.kp","\\.kr","\\.kw","\\.ky","\\.kz","\\.la","\\.lb","\\.lc","\\.li","\\.lk","\\.lr","\\.ls","\\.lt","\\.lu","\\.lv","\\.ly","\\.ma","\\.mc","\\.md","\\.me","\\.mg","\\.mh","\\.mk","\\.ml","\\.mm","\\.mn","\\.mo","\\.mp","\\.mq","\\.mr","\\.ms","\\.mt","\\.mu","\\.mv","\\.mw","\\.mx","\\.my","\\.mz","\\.na","\\.nc","\\.ne","\\.nf","\\.ng","\\.ni","\\.nl","\\.no","\\.np","\\.nr","\\.nu","\\.nz","\\.om","\\.pa","\\.pe","\\.pf","\\.pg","\\.ph","\\.pk","\\.pl","\\.pm","\\.pn","\\.pr","\\.ps","\\.pt","\\.pw","\\.py","\\.qa","\\.re","\\.ro","\\.rs","\\.ru","\\.rw","\\.sa","\\.sb","\\.sc","\\.sd","\\.se","\\.sg","\\.sh","\\.si","\\.sk","\\.sl","\\.sm","\\.sn","\\.so","\\.sr","\\.ss","\\.st","\\.su","\\.sv","\\.sx","\\.sy","\\.sz","\\.tc","\\.td","\\.tf","\\.tg","\\.th","\\.tj","\\.tk","\\.tl","\\.tm","\\.tn","\\.to","\\.tr","\\.tt","\\.tv","\\.tw","\\.tz","\\.ua","\\.ug","\\.uk","\\.us","\\.uy","\\.uz","\\.va","\\.vc","\\.ve","\\.vg","\\.vi","\\.vn","\\.vu","\\.wf","\\.ws","\\.ye","\\.yt","\\.za","\\.zm","\\.zw")
  if (TRUE %in% str_detect(url, country_code_top_level_domains)) {
    top_level_domain <- na.omit(str_extract(url,paste("(",country_code_top_level_domains,")$", sep = "", collapse = "|")))[1]  
  } else {
    top_level_domain <- str_extract(url, "\\..*$")  
  }
   
  
  top_level_country_domain
  
  
  Hostname <- NULL
  for (potentialHostname in 1:length(hostname)) {
    isHostname <- str_detect(hostname[potentialHostname], paste(top_level_domain, "$", sep = ""))
    if (is.null(Hostname)) {
      if(isHostname) {
        Hostname <- hostname[potentialHostname]
      } else {next}    
    } else {
      if(isHostname) {
        Hostname <- append(Hostname, hostname[potentialHostname])
      } else {next}
    }
  }
  Hostname
  if (top_level_domain == "uk" || top_level_domain == "com")
    str_remove(country_code_top_level_domains, "uk" )
  top_level_domain 
  '
  
  return(hostname)
}

#url <- "https://www.ereceptionist.co.uk"
#get_hostnames_from_url(url)


#### GENERAL ####

outersect <- function(x, y) {
  sort(c(x[!x%in%y],
         y[!y%in%x]))
}

simpleCap <- function(string) {
  s <- strsplit(string, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

tidy_coloumn_names <- function(df) {
  
  names <- names(df)
  names <- 
    names(df) <- 
      gsub('([[:upper:]])|\\.|\\_', ' \\1', names)
  
  cleanNames <- NULL
  for (name in 1:length(names)) {
    if(is.null(cleanNames)) {
      cleanNames <- simpleCap(names[name])
    } else {
      cleanNames <- append(cleanNames, simpleCap(names[name]))
    }
  }
  
  names(df) <- cleanNames
  
  
}

'%ni%' <- Negate('%in%')

#### APP ####

apply_filter_logic <- function(data, logic, dimension, operator, expression) {
  if (!logic || (logic && (dimension == "" || is.null(dimension)))) {data <- data} else {
    if (operator == "EXACT") {
      data <- filter_at(data, vars(dimension), all_vars(. == expression))
    } else if (operator == "REGEXP") {
      data <- filter_at(data, vars(dimension), all_vars(grepl(expression,.)))
    }
  }
  return(data)
}


#### APP GA #####

remove_all_power_tool_filters_from_view <- function(accountId, propertyId, viewId) {
  viewFilters <- ga_filter_view_list(accountId, propertyId, viewId)
  viewFilters <- filter(viewFilters, str_detect(filterRef.name, "DDL PowerTool"))
  filterIds <- viewFilters$id 
  filterIds <- str_extract(filterIds, ":.*")
  filterIds <- str_remove(filterIds, ":")
  for (id in 1:length(filterIds)) {
    ga_filter_delete(
      accountId,
      propertyId,
      viewId,
      filterIds[id]
    )
  }
}

ga_view_update <- function(
  view_update,
  accountId, 
  webPropertyId,
  viewId,
  method = c("PATCH", "PUT")
){
  method <- match.arg(method)
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  views <- gar_api_generator(
    baseURI = url,
    http_header = method,
    path_args = list(
      accounts = accountId,
      webproperties = propertyId,
      profiles = viewId
    ),
    data_parse_function = function(x) x
  )
  return(views(the_body = view_update))
}

ga_property_update <- function(
  property_update,
  accountId,
  propertyId,
  method = c("PATCH", "PUT")
){
  method <- match.arg(method)
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  property <- gar_api_generator(
    baseURI = url,
    http_header = method,
    path_args = list(
      accounts = accountId,
      webproperties = propertyId
    ),
    data_parse_function = function(x) x
  )
  return(property(the_body = property_update))
}


ga_account_id <- function(account_name) {
  filter(ga_accounts(), name == account_name)$id  
} 

ga_account <- function(account_name) {
  ga_account_list() %>% 
    filter(accountName == account_name)
}

ga_property <- function(account_name, property_name, type = "name") {
  if(type == "name") {
    ga_account_list() %>% 
      filter(accountName == account_name) %>%
      subset(webPropertyName %in% property_name)  
  } else if (type == "id") {
    ga_account_list() %>% 
      filter(accountName == account_name) %>%
      subset(webPropertyId %in% property_name)
  }
  
}

ga_property_id <- function(account_name, property_name) {
  unique(ga_property(account_name, property_name)$webPropertyId)
}

#Just filters by view at property level (dig in by viewId for ID)
ga_view_id <- function(account_name, property_name, view_name) {
  ga_property(account_name, property_name) %>%
    filter(viewName %in% view_name)
}

goal_loop <- function(account) {
  
  accountId <- unique(account$accountId)
  propertyIds <- account$webPropertyId
  viewIds <- account$viewId
  
  goals <- NULL
  goal <- NULL
  
  for (propertyId in 1:length(propertyIds)) {
    if (is.null(goals)) {
      #print("START")
      goals <- ga_goal_list(accountId, propertyIds[propertyId], viewIds[propertyId])
      if (dim(goals)[2] == 0) {
        #print("No goals")
        next
      } else if (dim(goals)[2] == 13) {
        goals$urlDestinationDetails.url <- NA  
        goals$urlDestinationDetails.caseSensitive <- NA    
        goals$urlDestinationDetails.matchType <- NA         
        goals$urlDestinationDetails.firstStepRequired <- NA
        goals$urlDestinationDetails.steps <- NA
        #PRINT
      } else if (dim(goals)[2] == 16) {
        goals$eventDetails.useEventValue <- NA
        goals$eventDetails.eventConditions <- NA
        #print(paste("dim vars was 16 is now ", dim(goals)[2], sep = " "))
      } else {
        goals <- goals
        print(colnames(goals))
        #print(paste("PROPERTY: ",propertyIds[propertyId], "|| VIEW: ",viewIds[propertyId]))
        #print(paste("dim vars is", dim(goals)[2], sep = " "))
      }
    } else {
      goal <- ga_goal_list(accountId, propertyIds[propertyId], viewIds[propertyId])
      if (dim(goal)[2] == 0) {
        #print("No goals")
        next
      } else if (dim(goal)[2] == 13) {
        goal$urlDestinationDetails.url <- NA  
        goal$urlDestinationDetails.caseSensitive <- NA    
        goal$urlDestinationDetails.matchType <- NA         
        goal$urlDestinationDetails.firstStepRequired <- NA
        goal$urlDestinationDetails.steps <- NA
        #PRINT
        goals <- rbind(goals, goal)
      } else if (dim(goal)[2] == 16) {
        goal$eventDetails.useEventValue <- NA
        goal$eventDetails.eventConditions <- NA
        #print(paste("dim vars was 16 is now ", dim(goal)[2], sep = " "))
        goals <- rbind(goals, goal)
      } else {
        #print(paste("dim vars is", dim(goal)[2], sep = " "))
        #print(colnames(goal))
        #print(paste("PROPERTY: ",propertyIds[propertyId], "|| VIEW: ",viewIds[propertyId]))
        goals <- rbind(goals, goal)
      }
    }
  }
  
  return(goals)
  
  
}

content_grouping_loop <- function(ga_content_grouping_data, account_name, property_names = NULL, view_names = NULL) {
  view_loop <- function(propertyId) {
    viewIds <- filter(ga_property(account_name, property_names), viewName %in% view_names)$viewId
    account <- data.frame(
      propertyName = property_names,
      propertyId = propertyId,
      viewName = view_names,
      viewId = viewIds,
      stringsAsFactors = FALSE
    )
    na_meta <- data.frame(contentGroup1 = NA,contentGroup2 = NA,contentGroup3 = NA,contentGroup4 = NA,contentGroup5 = NA)
    na_meta_in <- na_meta
    meta <- NULL
    for (view in 1:length(viewIds)){
      if (is.null(meta)) {
        #separate content data into view and replace not set with NA
        na_content <- ga_content_grouping_data %>%
          filter(viewId %in% viewIds[view]) %>%
          replace_with_na_all(condition = ~.x == "(not set)")
        for (col in 1:(ncol(na_content)-2)) {
          na_meta[col] <- !(all(is.na(na_content[[col]])))
        }
        meta <- na_meta
      } else {
        na_content <- ga_content_grouping_data %>%
          filter(viewId %in% viewIds[view]) %>%
          replace_with_na_all(condition = ~.x == "(not set)")
        for (col in 1:(ncol(na_content)-2)) {
          na_meta_in[col] <- !(all(is.na(na_content[[col]])))
        }
        metaIn <- na_meta_in
        meta <- rbind(meta, metaIn)
      }
    }
    return(cbind(account, meta))
  }
  
  if (is.null(view_names)) {
    
    propertyIds <- ga_property_id(account_name, property_names)
    
    
    propertyMeta <- NULL
    for (property in 1:length(propertyIds)) {
      if (is.null(propertyMeta)) {
        propertyMeta <- view_loop(propertyIds[property])  
      } else {
        propertyMeta <- rbind(propertyMeta, view_loop(propertyIds[property]))
      }
      
    }
    
    return(propertyMeta)
   
    
  } else if (length(view_names) > 1 ) {
    return(view_loop(ga_property_id(account_name, property_names)))
  } else {
    
    propertyId <- ga_property_id(account_name, property_names)
    viewId <- ga_view_id(account_name, property_names, view_names)$viewId
    account <- data.frame(
      propertyName = property_names,
      propertyId,
      viewName = view_names,
      viewId,
      stringsAsFactors = FALSE
    )
    
    na_content <- ga_content_grouping_data %>% replace_with_na_all(condition = ~.x == "(not set)")
    meta <- data.frame(
      contentGroup1 = NA,
      contentGroup2 = NA,
      contentGroup3 = NA,
      contentGroup4 = NA,
      contentGroup5 = NA
    )
    
    for (col in 1:(ncol(na_content)-1)) {
      meta[col] <- !(all(is.na(na_content[[col]])))
    }
    
    return(cbind(account, meta))   
  }
  
}

customMetric <- function(name = NULL, 
                            index = NULL,
                            scope = NULL, 
                            active = NULL,
                            id = NULL,
                            accountId = NULL,
                            webPropertyId = NULL){
  
  assert_that_ifnn(name, is.string)
  assert_that_ifnn(scope, is.string)
  assert_that_ifnn(active, is.flag)
  assert_that_ifnn(id, is.string)
  
  if(!is.null(index)){
    index <- as.numeric(index)
    assert_that(is.scalar(index),
                index <= 200,
                index > 0)
  }
  
  structure(
    list(id = id,
         name = name,
         index = index,
         scope = scope,
         active = active,
         accountId = accountId,
         webPropertyId = webPropertyId),
    class = "customMetric_ga"
  )
}

is.customMetric <- function(x){
  inherits(x, "customMetric_ga")
}

as.customMetric <- function(x){
  class(x) <- c("customMetric_ga", class(x))
  x
}

customDimension <- function(name = NULL, 
                            index = NULL,
                            scope = NULL, 
                            active = NULL,
                            id = NULL,
                            accountId = NULL,
                            webPropertyId = NULL){
  
  assert_that_ifnn(name, is.string)
  assert_that_ifnn(scope, is.string)
  assert_that_ifnn(active, is.flag)
  assert_that_ifnn(id, is.string)
  
  if(!is.null(index)){
    index <- as.numeric(index)
    assert_that(is.scalar(index),
                index <= 200,
                index > 0)
  }
  
  structure(
    list(id = id,
         name = name,
         index = index,
         scope = scope,
         active = active,
         accountId = accountId,
         webPropertyId = webPropertyId),
    class = "customDimension_ga"
  )
}

is.customDimension <- function(x){
  inherits(x, "customDimension_ga")
}

as.customDimension <- function(x){
  class(x) <- c("customDimension_ga", class(x))
  x
}

ga_custom_dimension_create <- function(name, 
                                  index,
                                  accountId,
                                  webPropertyId,
                                  active,
                                  scope = c("HIT","SESSION","USER","PRODUCT")){
  scope <- match.arg(scope)
  accountId <- as.character(accountId)
  
  cd <- customDimension(name = name,
                        index = index,
                        accountId = accountId,
                        webPropertyId = webPropertyId,
                        scope = scope,
                        active = active)
  
  api <- gar_api_generator("https://www.googleapis.com/analytics/v3/management",
                           "POST",
                           path_args = list(
                             accounts = accountId,
                             webproperties = webPropertyId,
                             customDimensions = ""
                           ),
                           data_parse_function = function(x) x)
  api(the_body = cd)
  
  
}

ga_custom_metric_create <- function(name, 
                                       index,
                                       accountId,
                                       webPropertyId,
                                       active,
                                       scope = c("HIT", "PRODUCT")){
  scope <- match.arg(scope)
  accountId <- as.character(accountId)
  
  cm <- customDimension(name = name,
                        index = index,
                        accountId = accountId,
                        webPropertyId = webPropertyId,
                        scope = scope,
                        active = active)
  
  api <- gar_api_generator("https://www.googleapis.com/analytics/v3/management",
                           "POST",
                           path_args = list(
                             accounts = accountId,
                             webproperties = webPropertyId,
                             customMetrics = ""
                           ),
                           data_parse_function = function(x) x)
  api(the_body = cm)
  
  
}


#This extracts the regex expression from the filters we want to remove because they are not maximimising filter character space in GA -->
filter_out <- function(ga_filter_table) {
  
  ga_filter_table <- ga_filter_table %>%
    filter(!str_detect(name, "- DDL PowerTool"))
  
  if (is.null(ga_filter_table) || nrow(ga_filter_table) == 0) {return(NULL)} else {
    filters <- ga_filter_table$excludeDetails.expressionValue
    small_filters <- NULL
    for (regex in 1:length(filters)) {
      if (is.null(small_filters)) {
        if (nchar(filters[regex]) < 200) {
          small_filters <- filters[regex]
        } else {next}  
      } else {
        if (nchar(filters[regex]) < 200) {
          small_filters_in <- filters[regex]
          small_filters <- append(small_filters, small_filters_in)
        } else {next}
      }
    }
    return(small_filters)
  }
}

#works as value for limit_filter_input
get_maximum_filter_number <- function(ga_filter_table, filter_in, filter_out) {
  #Check if there are any exsisting values in filters and if so remove from filter_in
  exsistingFilter <- ga_filter_table$excludeDetails.expressionValue
  exsistingFilter <- unlist(str_split(str_remove_all(exsistingFilter, "(^\\^\\(|\\)\\$$)"), "\\|"))
  filter_in <- filter_in[filter_in %ni% exsistingFilter] 
  #get maximum number of filters
  if(is.null(filter_in)) {
    filter_in <- filter_in
    number_of_filters = 0
  } else {
    filter_in <- paste(filter_in, collapse = "|")
    if (nchar(filter_in) < 251) {
      if (is.null(ga_filter_table)) {
        filter_in <- paste("^(", filter_in ,")$", sep = "")  
      } else {
        filter_in <- paste("^(", filter_in ,")$", sep = "")    # PLUS OTHER FILTERS
      }  
    } else {
      if (is.null(ga_filter_table)) {
        filter_in <- filter_in
      } else { 
        remove_already_exsiting_filter_in <- paste(filter_out, collapse = "|")
        filter_in <- paste(remove_already_exsiting_filter_in , filter_in, collapse = "|") 
      }
      maximum_characters_in_ga_filter <- 251
      number_of_filters = ceiling(nchar(filter_in)/maximum_characters_in_ga_filter)
    }
  }
  return(number_of_filters)
}

#Takes a vector of things to be filtered from GA and puts into a list of regex (ensureing list doesnt exceed 255 limit) -->
prepare_list_for_filters <- function(ga_filter_table, filter_in, filter_out, limit_filter_input) {
  
  #Check if there are any exsisting values in filters and if so remove from filter_in
  exsistingFilter <- ga_filter_table$excludeDetails.expressionValue
  exsistingFilter <- unlist(str_split(str_remove_all(exsistingFilter, "(^\\^\\(|\\)\\$$)"), "\\|"))
  filter_in <- filter_in[filter_in %ni% exsistingFilter] 
  
  #Cretate the filter exlusion regex list "filter_in"
  if(is.null(filter_in)) {
    filter_in <- filter_in
  } else {
    filter_in <- paste(filter_in, collapse = "|")
    if (nchar(filter_in) < 251) {
      if (is.null(ga_filter_table)) {
        filter_in <- paste("^(", filter_in ,")$", sep = "")  
      } else {
        filter_in <- paste("^(", filter_in ,")$", sep = "")    # PLUS OTHER FILTERS
      }  
    } else {
      if (is.null(ga_filter_table)) {
        filter_in <- filter_in
      } else { 
        remove_already_exsiting_filter_in <- paste(filter_out, collapse = "|")
        filter_in <- paste(remove_already_exsiting_filter_in , filter_in, collapse = "|") 
      }
      maximum_characters_in_ga_filter <- 251
      filter_in <- strsplit(filter_in, "\\|")[[1]]
      regex_list <- NULL
      for (expression in 1:limit_filter_input) {
        if(is.null(regex_list)) {
          regex <- NULL
          for (each_filter_in in 1:length(filter_in)) {
            if (is.null(regex)) {
              filter_in[each_filter_in]
              regex <- filter_in[each_filter_in]
              stringCount <- nchar(regex)
            } else {
              regex <- paste(regex, paste("|", filter_in[each_filter_in], sep = ""), sep = "") 
              totalStringCount <- stringCount + nchar(regex)
              if (totalStringCount > maximum_characters_in_ga_filter) {
                regex <- str_remove(regex, paste("\\|", filter_in[each_filter_in], sep = ""))
                filter_in <- filter_in[-(each_filter_in-1):0]
                break
              } else {
                regex <- regex
                totalStringCount <- totalStringCount
              } 
            }
          }
          regex_list <- paste("^(",regex,")$", sep = "")
        } else {
          regex <- NULL
          for (each_filter_in in 1:length(filter_in)) {
            if (is.null(regex)) {
              regex <- filter_in[each_filter_in]
              stringCount <- nchar(regex)
            } else {
              regex <- paste(regex, paste("|", filter_in[each_filter_in], sep = ""), sep = "") 
              totalStringCount <- stringCount + nchar(regex)
              if (totalStringCount > maximum_characters_in_ga_filter) {
                regex <- str_remove(regex, paste("\\|", filter_in[each_filter_in], sep = ""))
                filter_in <- filter_in[-(each_filter_in-1):0]
                break
              } else {
                regex <- regex
                totalStringCount <- totalStringCount
              } 
            }
          }
          regex_list <- append(regex_list, paste("^(",regex,")$", sep = ""))
        }
      }
      filter_in <- regex_list
    }
  }
  return(filter_in)
}

ga_hygiene_filter_update <- function(
  accountId,
  webPropertyId,
  viewId,
  limit_filter_input,
  hygiene_ga_filters,
  ga_filter_table,
  filter_in,
  filter_out,
  regex_filter_list,
  filter_field,
  filter_type = NULL #Currently works on "EXCLUDE"  
) {
  
  #### Check if already exsisting powerTool filter
  
  viewFilters <- arrange(hygiene_ga_filters, rank)
  names <- viewFilters$name
  exp <- paste(tolower(str_replace_all(filter_field, "_", " ")), "([0-9]|[1-9][0-9]) - DDL PowerTool")
  if (any(str_detect(names, exp))) {
    #Get the last number from field_name powerTool filter
    names <- na.omit(str_extract(names, exp))
    last_filter_number = as.numeric(last(str_extract(names, "[0-9]")))
  } else {
    last_filter_number = 0  
  }
  
  #### Do filter operations
  if (!is.null(ga_filter_table) && !is.null(filter_out) && (!is.null(filter_in) || is.null(filter_in))) {
    print(paste(filter_field,": filter_out and replace filters and or add filters from report"), sep = "")
    #Remeber exsiting rank
    firstRank <- filter_out$rank[1]
    
    #filter_out exsiting filters
    filterIds <- filter_out$id
    print(paste("Removing exsisting filters"), paste(filterIds, collapse = ", "))
    for (id in 1:length(filterIds)) {
      response <- ga_filter_delete(
        accountId,
        webPropertyId,
        viewId,
        filterId = filterIds[id],
        filter_outFromView = TRUE
      ) 
      print(response)
    }
    #Create New filters
    print(paste("Creating", limit_filter_input, "new filters" ))
    for (newFilter in 1:limit_filter_input) {
      filter <- list(
        name = paste("EXCLUDE:", tolower(str_replace_all(filter_field, "_", " ")), as.character(newFilter + last_filter_number), "- DDL PowerTool"),
        type = "EXCLUDE",
        excludeDetails = list(
          field = filter_field,
          matchType = "MATCHES",
          expressionValue = regex_filter_list[newFilter],
          caseSensative = "FALSE"
        )
      ) 
      response <- ga_filter_add(filter, accountId, webPropertyId, viewId, linkFilter = TRUE)
      print(response)
      viewFilterLink <- list(rank = (firstRank + newFilter - 1))
      response <- ga_filter_update_filter_link(viewFilterLink, accountId, webPropertyId , viewId, linkId = response$id, method = "PUT")
    }
  } else if (!is.null(ga_filter_table) && is.null(filter_out) && !is.null(filter_in)) {
    print(paste(filter_field,": Current filters are satifactory: add filters from report"), sep = "")
    #Remeber exsiting rank
    firstRank <- filter_out$rank[1]
    #Create New filters
    print(paste("Creating", limit_filter_input, "new filters" ))
    
    for (newFilter in 1:limit_filter_input) {
      print(filter_field)
      filter <- list(
        name = paste("EXCLUDE:", tolower(str_replace_all(filter_field, "_", " ")), newFilter + last_filter_number, "- DDL PowerTool"),
        type = "EXCLUDE",
        excludeDetails = list(
          field = filter_field,
          matchType = "MATCHES",
          expressionValue = regex_filter_list[newFilter],
          caseSensative = "FALSE"
        )
      ) 
      response <- ga_filter_add(filter, accountId, webPropertyId, viewId, linkFilter = TRUE)
      print(response)
      viewFilterLink <- list(rank = (firstRank + newFilter - 1))
      response <- ga_filter_update_filter_link(viewFilterLink, accountId, webPropertyId , viewId, linkId = response$id, method = "PUT")
    }
  } else if (is.null(ga_filter_table) && !is.null(filter_in)) {
    print(paste(filter_field,": No current filters -- add filters from report"), sep = "")
    filter_list <- hygiene_ga_filters %>%
      filter(type == "EXCLUDE")
    exsistingRank <- last(filter_list$rank)
    print(paste("Creating", limit_filter_input, "new filters" ))
    for (newFilter in 1:limit_filter_input) {
      filter <- list(
        name = paste("EXCLUDE:", tolower(str_replace_all(filter_field, "_", " ")), newFilter + last_filter_number, "- DDL PowerTool"),
        type = "EXCLUDE",
        excludeDetails = list(
          field = filter_field,
          matchType = "MATCHES",
          expressionValue = regex_filter_list[newFilter],
          caseSensative = "FALSE"
        )
      ) 
      response <- ga_filter_add(filter, accountId, webPropertyId, viewId, linkFilter = TRUE)
      print(response)
      viewFilterLink <- list(rank = (exsistingRank + newFilter))
      response <- ga_filter_update_filter_link(viewFilterLink, accountId, webPropertyId , viewId, linkId = response$id, method = "PUT")
    }
  } else {
    print(paste(filter_field,": No action required"), sep = "")
    response <- NULL
  }
  return(response)
}


extract_query_parameters_from_url <- function(pii){
  query_param <- str_extract(pii, "\\?.+=.+(&.+=.+)?+")
  query_param <- unlist(str_split(query_param, "&"))
  query_param <- str_extract(query_param, ".*=")
  query_param <- str_remove(query_param, "^\\?|=$")
  return(query_param)
}

##### APP GTM #######

gtm_account_id <- function(account_name) {
  filter(gtm_accounts_list(), name == account_name)$accountId  
}

gtm_container_id <- function(account_name, container_name) {
  container <- gtm_containers_list(gtm_account_id(account_name))$containers
  return(container$id)
}

get_containers <- function(containers, container_name) {
  subset(containers, name %in% container_name)
}

unnest_tags_old <- function(tags) {
  tags <- tags
  
  #print("full tags")
  #print(as_tibble(tags))
  #print("tags parameter")
  #print(as_tibble(tags["parameter"][[1]]))
  
  tags[["firingTriggerId"]] <- vapply(tags[["firingTriggerId"]], paste, collapse = ", ", character(1L))
  tags[["blockingTriggerId"]] <- vapply(tags[["blockingTriggerId"]], paste, collapse = ", ", character(1L))
  
  #if (tags$parameter)
  if(!is.null(tags$parameter)) {
    tags  <- tags %>% 
    tidyr::unnest(parameter, names_repair = "minimal", names_sep="." )
    
    if (!is.null(tags$parameter.list)) {
      tags  <- tags %>% 
        tidyr::unnest_longer(parameter.list, names_repair = "check_unique")
      
      parameter.list <- tags$parameter.list
      names(parameter.list) <- c("parameter.list.type", "parameter.list.map") 
      tags <- tags %>% select(-parameter.list) 
      tags_1 <- tags[1:9]
      tags_2 <- parameter.list
      tags_3 <- tags[10:length(tags)]
      tags <- cbind(tags_1, tags_2, tags_3)
    
      if(!is.null(tags$parameter.list.map)) {
        
        tags <- tags %>% tidyr::unnest_longer(parameter.list.map, names_repair = "minimal")
        
        parameter.list.map <- tags$parameter.list.map
        names(parameter.list.map) <- c("parameter.list.map.type", "parameter.list.map.key", "parameter.list.map.value")
      }  
    }
  }
  if (!is.null(tags[["teardownTag"]])) {
    tags[["teardownTag"]] <- vapply(tags[["teardownTag"]], paste, collapse = ", ", character(1L))
    names(tags$teardownTag) <- "teardownTag.tagName"
  } 
  if (!is.null(tags[["priority"]]) && !is.null(tags$parameter.list.map)) {
    priority <- tags[["priority"]]
    names(priority) <- c("priority.type", "priority.value")
    
    tags <- tags %>% select(-parameter.list.map) %>% select(-priority)
    
    tags_1 <- tags[1:10]
    tags_2 <- parameter.list.map
    tags_3 <- tags[11:length(tags)]
    tags_4 <- priority
    
    tags <-cbind(tags_1, tags_2, tags_3, tags_4)
  } else {
    
    priority.type <- NA 
    priority.value <- NA
    tags <- tags %>% select(-parameter.list.map)
    tags_1 <- tags[1:10]
    tags_2 <- parameter.list.map
    tags_3 <- tags[11:length(tags)]
    
    
    tags <-cbind(tags_1, tags_2, priority.type, priority.value)
  }
  
  
  return(tags)
  
}
unnest_tags <- function(tags) {
  
  colClasses <- lapply(tags,class)
  for (col in 1:length(colClasses)) {
    if (colClasses[col][[1]] == "list") {
      colName <- names(colClasses[col])
      #print(paste(colName, ": ", class(tags[[colName]][[1]])))
      
      for (row in 1:length(tags[[colName]])) {
        if (class(tags[[colName]][[1]]) == "data.frame") {
          tags  <- tags %>% 
            tidyr::unnest(all_of(colName), names_repair = "minimal", names_sep="." )

          innerTagsCols <- tags[names(tags) %like% colName]
          for (dataFrameCol in 1:length(names(innerTagsCols))) {
            if (class(innerTagsCols[dataFrameCol])[1] == "tbl_df") {
              tags <- tags %>%
                tidyr::unnest_longer(all_of(names(innerTagsCols[dataFrameCol])), names_repair = "check_unique")  
            }
          }
        }
      }
      for (row in 1:length(tags[[colName]])) {
        if (class(tags[[colName]][[row]]) == "character") {
          tags[[colName]] <- vapply(tags[[colName]], paste, collapse = ", ", character(1L))
          
        }  
      }
    }
  }
  return(tags)
}
unnest_triggers <- function(triggers) {
  
  if (!is.null(triggers$parameter)) {
    triggers  <- triggers %>% 
      tidyr::unnest(filter, keep_empty=TRUE, names_repair = "minimal", names_sep="." )  %>%
      tidyr::unnest(filter.parameter, keep_empty=TRUE, names_repair = "minimal", names_sep="." ) %>%
      tidyr::unnest_longer(parameter, names_repair = "minimal")
    
    parameter <- triggers$parameter
    names(parameter) <- c("parameter.type", "parameter.key", "parameter.value")
    triggers <- select(triggers, -parameter)
    triggers <- cbind(triggers, parameter)
    
    
    if(!is.null(triggers$customEventFilter)) {
      triggers  <- triggers %>% 
        tidyr::unnest(customEventFilter,keep_empty=TRUE, names_repair = "minimal", names_sep="." )  %>%
        tidyr::unnest(customEventFilter.parameter, keep_empty=TRUE, names_repair = "minimal", names_sep="." )
    }
    if (!is.null(triggers$waitForTags)) {
      waitForTags <- triggers$waitForTags
      names(waitForTags) <- c("waitForTags.type", "waitForTags.value")  
      triggers <- select(triggers, -waitForTags) 
      triggers <- cbind(triggers, waitForTags)
    } else {
      waitForTags.type <- NA 
      waitForTags.value <- NA
    }
    if (!is.null(triggers$checkValidation)) {
      checkValidation <- triggers$checkValidation
      names(checkValidation) <- c("checkValidation.type", "checkValidation.value")  
      triggers <- select(triggers, -checkValidation)
      triggers <- cbind(triggers, checkValidation)
    } else {
      checkValidation.type <- NA 
      checkValidation.value<- NA
    }
    if (!is.null(triggers$waitForTagsTimeout)) {
      waitForTagsTimeout <- triggers$waitForTagsTimeout
      names(waitForTagsTimeout) <- c("waitForTagsTimeout.type", "waitForTagsTimeout.value")
      triggers <- select(triggers, -waitForTagsTimeout)
      triggers <- cbind(triggers, waitForTagsTimeout)
    } else {
      waitForTagsTimeout.type <- NA 
      waitForTagsTimeout.value <- NA
    }
    if (!is.null(triggers$uniqueTriggerId)) {
      uniqueTriggerId <- triggers$uniqueTriggerId
      names(uniqueTriggerId) <- c("uniqueTriggerId.type", "uniqueTriggerId.value")  
      triggers <- select(triggers, -uniqueTriggerId)
      triggers <- cbind(triggers, uniqueTriggerId)
    } else {
      uniqueTriggerId.type <- NA 
      uniqueTriggerId.value <- NA
    }
    
    
    #triggers <- triggers %>%
      #dplyr::select(-customEventFilter) 
  
    
  } else {
    triggers  <- triggers %>% 
      tidyr::unnest(filter,keep_empty=TRUE, names_repair = "minimal", names_sep="." )  %>%
      tidyr::unnest(filter.parameter, keep_empty=TRUE, names_repair = "minimal", names_sep="." )
    
    parameter.type <- NA
    parameter.key <- NA
    parameter.value <- NA
    triggers <- cbind(triggers, parameter.type, parameter.key,parameter.value)
    
    if(!is.null(triggers$customEventFilter)) {
      triggers  <- triggers %>% 
        tidyr::unnest(customEventFilter,keep_empty=TRUE, names_repair = "minimal", names_sep="." )  %>%
        tidyr::unnest(customEventFilter.parameter, keep_empty=TRUE, names_repair = "minimal", names_sep="." )
    }
    if (!is.null(triggers$waitForTags)) {
      waitForTags <- triggers$waitForTags
      names(waitForTags) <- c("waitForTags.type", "waitForTags.value")  
      triggers <- select(triggers, -waitForTags) 
      triggers <- cbind(triggers, waitForTags)
    } else {
      waitForTags.type <- NA 
      waitForTags.value <- NA
    }
    if (!is.null(triggers$checkValidation)) {
      checkValidation <- triggers$checkValidation
      names(checkValidation) <- c("checkValidation.type", "checkValidation.value")  
      triggers <- select(triggers, -checkValidation)
      triggers <- cbind(triggers, checkValidation)
    } else {
      checkValidation.type <- NA 
      checkValidation.value<- NA
    }
    if (!is.null(triggers$waitForTagsTimeout)) {
      waitForTagsTimeout <- triggers$waitForTagsTimeout
      names(waitForTagsTimeout) <- c("waitForTagsTimeout.type", "waitForTagsTimeout.value")
      triggers <- select(triggers, -waitForTagsTimeout)
      triggers <- cbind(triggers, waitForTagsTimeout)
    } else {
      waitForTagsTimeout.type <- NA 
      waitForTagsTimeout.value <- NA
    }
    if (!is.null(triggers$uniqueTriggerId)) {
      uniqueTriggerId <- triggers$uniqueTriggerId
      names(uniqueTriggerId) <- c("uniqueTriggerId.type", "uniqueTriggerId.value")  
      triggers <- select(triggers, -uniqueTriggerId)
      triggers <- cbind(triggers, uniqueTriggerId)
    } else {
      uniqueTriggerId.type <- NA 
      uniqueTriggerId.value <- NA
    }
    
    #triggers <- triggers %>%
    #  dplyr::select(-customEventFilter) %>% 
  
  }
  
  return(triggers)
}
unnest_variables <- function(variables) {
  variables  <- variables %>% 
    tidyr::unnest(parameter, names_repair = "minimal", .sep="." )
  
  variables <- variables %>%
    tidyr::unnest_longer(parameter.list, names_repair = "minimal")
  
  parameter.list <- variables$parameter.list
  names(parameter.list) <- c("parameter.list.type", "parameter.list.map")
  variables <- select(variables, -parameter.list)
  variables_1 <- variables[1:8]
  variables_2 <- parameter.list
  #names(variables_2) <- c("parameter.list.type", "parameter.list.map")
  variables_3 <- variables[9:10]
  variables <- cbind(variables_1, variables_2, variables_3)
  
  variables <- variables %>%
    tidyr::unnest_longer(parameter.list.map, names_repair = "minimal")
  
  parameter.list.map <- variables$parameter.list.map
  names(parameter.list.map) <- c("parameter.list.map.type", "parameter.list.map.key", "parameter.list.map.value")
  
  variables <- select(variables, -parameter.list.map)
  
  variables_1 <- variables[1:9]
  variables_2 <- parameter.list.map
  variables_3 <- variables[10:11]
  variables <- cbind(variables_1, variables_2, variables_3)
  
}

unnest_all_columns <- function(df) {
  colClasses <- lapply(df, class)  
  while(isTRUE(any(unlist(colClasses) %in% "list")) || isTRUE(any(unlist(colClasses) %in% "data.frame"))) {  
    colClasses <- lapply(df, class) 
    
    
    for (col in 1:ncol(df)) {
      
      #print(names(df[col]))
      colName <- names(colClasses[col])
      
      if (colClasses[[col]] == "list") {
        
        if (isTRUE(all(unlist(lapply(df[[col]], class)) %in% "NULL"))) {
          df[[col]] <- NA
        } else {  
          if ( isTRUE(any(unlist(lapply(df[[col]], class)) %in% "data.frame"))) {
            
            df  <- df %>% 
              tidyr::unnest(all_of(colName), names_repair = "minimal", names_sep="." )
            
            innerTagsCols <- df[names(df) %like% colName]
            innerColClasses <- unlist(lapply(innerTagsCols, class))
            while(isTRUE(any(innerColClasses %in% "list")) ||isTRUE(any(innerColClasses %in% "data.frame"))) {
              innerTagsCols <- df[names(df) %like% colName]
              innerColClasses <- unlist(lapply(innerTagsCols, class))
              for (innerCol in 1:ncol(innerTagsCols)) {
                if (innerColClasses[[innerCol]] == "list") {
                  df <- df %>%
                    tidyr::unnest_longer(all_of(names(innerTagsCols[innerCol])), names_repair = "check_unique")  
                } else if (innerColClasses[[innerCol]] == "data.frame") {
                  
                  innerColName <- names(innerTagsCols)[innerCol]
                  #print(paste(innerColName, ": ", class(df[[innerColName]])))  
                  column <- df[[innerColName]]
                  for (col in 1:length(names(column))){names(column)[col] <- paste(innerColName,".", names(column)[col], sep = "")}
                  df <- df[ , names(df) != innerColName]
                  df <- cbind(df, column)
                }  
              }  
            }
            
          } else if (isTRUE(any(unlist(lapply(df[[col]], class)) %in% "character"))) {
            df[[colName]] <- vapply(df[[colName]], paste, collapse = ", ", character(1L))
            
          }
        }
        
      } else if (colClasses[[col]] == "data.frame") {
        
        #print(paste(colName, ": ", class(df[[colName]])))  
        column <- df[[colName]]
        for (col in 1:length(names(column))){names(column)[col] <- paste(colName,".", names(column)[col], sep = "")}
        df <- df[ , names(df) != colName]
        df <- cbind(df, column)
        
      }
    }
  }
  return(df)
} 


gtm_pull_and_bind_old <- function(container, unnestType) {
  #Note: tags and triggers will unnest in the loop, but variables unnest after for loop.
  
  accountId <- container$accountId[[1]]
  containerIds <- container$containerId
  
  if (unnestType == "tag") {
    
    allData <- NULL
    incomingData <- NULL
    
    for (containerId in 1:length(containerIds)) {
      if (is.null(allData)) {
        allData <- gtm_tags_list(accountId, containerIds[containerId])$tags
        allData <- unnest_tags(allData)
        print("First incomingData")
        print(tibble(allData))
      } else {
        incomingData <- gtm_tags_list(accountId, containerIds[containerId])$tags
        incomingData <- unnest_tags(incomingData)
        print("next incomingData")
        print(tibble(incomingData))
        if (identical(sort(names(incomingData)), sort(names(allData)))) {
          incomingData <- incomingData
          print("allData same")
        } else {
          outNames <- outersect(names(incomingData), names(allData))
          if (length(names(incomingData)) >  length(names(allData))) {
            print("new incomingData bigger than old incomingData")
            for (name in 1:length(outNames)) {
              allData[[outNames[name]]] <- NA
              print("added columns to old allData")
            }
          } else {
            for (name in 1:length(outNames)) {
              incomingData[[outNames[name]]] <- NA
              print("added columns to new allData")
            }
          }
        }
        allData <- rbind(allData, incomingData)
      }      
    }
  } else if (unnestType == "trigger") {
    
    allData <- NULL
    incomingData <- NULL
    
    for (containerId in 1:length(containerIds)) {
      if (is.null(allData)) {
        allData <- gtm_triggers_list(accountId, containerIds[containerId])$triggers
        allData <- unnest_triggers(allData)
        print("First incomingData")
      } else {
        incomingData <- gtm_triggers_list(accountId, containerIds[containerId])$triggers
        incomingData <- unnest_triggers(incomingData)
        print("next incomingData")
        if (identical(sort(names(incomingData)), sort(names(allData)))) {
          incomingData
          print("allData same")
        } else {
          outNames <- outersect(names(incomingData), names(allData))
          if (length(names(incomingData)) >  length(names(allData))) {
            print("new incomingData bigger than old incomingData")
            for (name in 1:length(outNames)) {
              allData[[outNames[name]]] <- NA
              print("added columns to old allData")
            }
          } else {
            for (name in 1:length(outNames)) {
              incomingData[[outNames[name]]] <- NA
              print("added columns to new allData")
            }
          }
        }
        allData <- rbind(allData, incomingData)
      }      
    }
  } else if (unnestType == "variable") {
    allData <- NULL
    incomingData <- NULL
    for (containerId in 1:length(containerIds)) {
      if (is.null(allData)) {
        allData <- gtm_variables_list(accountId, containerIds[containerId])$variables
        print("First incomingData")
      } else {
        incomingData <- gtm_variables_list(accountId, containerIds[containerId])$variables
        print("next incomingData")
        if (identical(sort(names(incomingData)), sort(names(allData)))) {
          incomingData
          print("allData same")
        } else {
          outNames <- outersect(names(incomingData), names(allData))
          if (length(names(incomingData)) >  length(names(allData))) {
            print("new incomingData bigger than old incomingData")
            for (name in 1:length(outNames)) {
              allData[[outNames[name]]] <- NA
              print("added columns to old allData")
            }
          } else {
            for (name in 1:length(outNames)) {
              incomingData[[outNames[name]]] <- NA
              print("added columns to new allData")
            }
          }
        }
        allData <- rbind(allData, incomingData)
      }      
    }
    allData <- unnest_variables(allData) 
  }
  
  return(allData)
  
}

gtm_pull_and_bind <- function(container_df, unnest_type) { 
  
  gtm_get <- function(unnest_type, accountId, containerIds, integer) {
    if (unnest_type == "tag") {
      df <- gtm_tags_list(accountId, containerIds[integer])$tags
    } else if (unnest_type == "trigger") {
      df <- gtm_triggers_list(accountId, containerIds[integer])$triggers
    } else if (unnest_type == "variable") {
      df <- gtm_variables_list(accountId, containerIds[integer])$variables
    }
    return(df)
  }
  
  accountId <- container_df$accountId[[1]]
  containerIds <- container_df$containerId
  if (length(containerIds) > 6) {
    Sys.sleep(100)
  }
  
  allData <- NULL
  for (container in 1:length(containerIds)) {
    if (container%%6==0) {
      Sys.sleep(100)
    }
    if (is.null(allData)) {
      allData <- gtm_get(unnest_type, accountId, containerIds, container)
      allData <- unnest_all_columns(allData)
      print(names(allData))
    } else {
      incomingData <- gtm_get(unnest_type, accountId, containerIds, container)
      incomingData <- unnest_all_columns(incomingData)
      #print(names(incomingData))
      
      #print(identical(sort(names(incomingData)), sort(names(allData))))
      
      if (!identical(sort(names(incomingData)), sort(names(allData)))) {
        outNames <- outersect(names(incomingData), names(allData))
        #print(outNames)
        namesInAllData <- outNames[outNames %in% names(allData)]
        if (!identical(namesInAllData, character(0))){
          for (outName in 1:length(namesInAllData)) {
            incomingData[[namesInAllData[outName]]] <- NA
          }
        }
        namesInIncomingData <- outNames[!(outNames %in% names(allData))]
        if (!identical(namesInIncomingData, character(0))){
          for (outName in 1:length(namesInIncomingData)) {
            allData[[namesInIncomingData[outName]]] <- NA
          }
        }
        #print(names(allData))
        #print(names(incomingData))
        #print(identical(sort(names(incomingData)), sort(names(allData)))) 
      } else {
        incomingData <- incomingData
      }
      allData <- rbind(allData, incomingData)
    }
  }
  return(allData)
}

gtm_tags_list <- function(accountId, containerId) {
  url <- paste0("https://www.googleapis.com/tagmanager/v1/accounts/",accountId,"/containers/",containerId,"/tags")
  # tagmanager.tags.list
  f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
  f()
}

gtm_containers_list <- function(accountId) {
  url <-
    paste0(
      "https://www.googleapis.com/tagmanager/v1/accounts/",
      accountId,
      "/containers/"
    )
  # tagmanager.tags.list
  f <-
    googleAuthR::gar_api_generator(
      url,
      "GET",
      data_parse_function = function(x)
        x
    )
  f()
}
