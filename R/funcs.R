
#' A text analysis Function
#'
#' This function allows you to readily analyze texts using a dictionary of choice from the package.
#' @param data The data set with texts to analyze.
#' @param text_column The name of the column in data with the texts. Defaults to 'text'.
#' @param dict_name The name of the dictionary used for the analysis. Defaults to 'concrete'.
#' @param lemmatize Whether to lemmatize the words in the text (TRUE/FALSE). Defaults to NULL, in which case the dictionary's default is chosen (see `dict_info`).
#' 
#' @keywords lexicon, dictionary
#' @export
#' @examples
#' analyze_text(data.frame(text="Lord, what fools these mortals be!"))
#' 
#' analyze_text(data.frame(bard="Lord, what fools these mortals be!"),text_column='bard',dict='VAD')

analyze_text <- function(data,text_column = 'text', dict_name='concrete', lemmatize = NULL){
  if(sum(Dictionorm::dict_info$dict_names%in%dict_name)==0){
    message('Error, dictionary not found. See dict_info$dict_names for included dictionary names.')
  }
  else{
    lem_flag <- TRUE
    if(!is.null(lemmatize)){
      if(lemmatize!=TRUE&lemmatize!=FALSE){
        lem_flag <- FALSE
      }
    }
    
    if(lem_flag!=TRUE){
      message('Error, lemmatize must be TRUE/FALSE/NULL')
    }
    else{
      if(is.null(lemmatize)){
        dict_entry <- Dictionorm::dict_info$dict_entry[Dictionorm::dict_info$dict_names%in%dict_name]
      }
      else{
        if(lemmatize==TRUE){
          dict_entry <- "lemmas"
        }
        else{
          if(lemmatize==FALSE){
            dict_entry <- "tokens"
          }
        }
      }
      propped <- preproc_text(data,text_column = text_column,dict_entry=dict_entry)
      dict_type <- Dictionorm::dict_info$dict_type[Dictionorm::dict_info$dict_names%in%dict_name]
      norm_column <- Dictionorm::dict_info$norm_column[[which(Dictionorm::dict_info$dict_names%in%dict_name)]]
      out_names <- Dictionorm::dict_info$out_norm_column[[which(Dictionorm::dict_info$dict_names%in%dict_name)]]
      dict <- Dictionorm::all_dicts[[dict_name]]
      
      if(dict_type=='norms'){
        names(dict)[grep('word',names(dict),ignore.case = T)] <- 'Word'
        responses <- norm_dicts(propped,dict,column=norm_column)
        colnames(responses)[2:(1+length(out_names))] <- out_names
      }
      else{
        if(dict_type=='LIWCalike'){
          responses <- liwcalike_dicts(propped,dict)
          colnames(responses)[2:(1+length(out_names))] <- out_names
        }
      }
      return(responses)
    }
  }
}

#' A function for pre-processing text for analysis
#'
#' This function pre-processes a text for analysis by tokenizing or lemmatizing the text.
#' @param data The data set with texts to analyze.
#' @param text_column The name of the column in data with the texts. Defaults to 'text'.
#' @param dict_entry Whether to split text into tokens or lemmas. Defaults to 'tokens'.
#' 
#' @keywords lexicon, dictionary
#' @export
#' @examples
#' preproc_text(data.frame(text="For sweetest things turn sourest by their deeds"))
#' 
#' preproc_text(data.frame(text="For sweetest things turn sourest by their deeds"),dict_entry='lemmas')

preproc_text <- function(data,text_column = 'text',dict_entry='tokens'){
  if(dict_entry=='lemmas'){
      propped <- lemmatize_preproc(data,text_column)
      return(propped)
    }
  else{
    if(dict_entry=='tokens'){
      propped <- tokenize_preproc(data,text_column)
      return(propped)
      }
    else{
      message('Error. Text processing (`dict_entry`) must be either as tokens / lemmas.') 
      }
    }
}

tokenize_preproc <- function(data,text_column,word_column){
  text_toks <-  data[[text_column]] %>% 
    quanteda::tokens() %>%
    quanteda::tokens_tolower()
  return(text_toks)
}

lemmatize_preproc <- function(data,text_column,word_column){
  text_lemmas <- data[[text_column]] %>% 
    textstem::lemmatize_strings() %>% 
    quanteda::tokens() %>%
    quanteda::tokens_tolower()
  return(text_lemmas)
}

#' A function for text analysis with norm based dictionaries
#'
#' This function performs an analysis on pre-processed text, calculating the levels of the selected norm-based dictionary in the text.
#' This function isn't limited to dictionaries in the package and can be used with additional custom norm dictionaries (as opposed to the analyze_text function).
#' @param now_toks The tokens / lemmas from the text pre-processing.
#' @param now_dict The norms dictionary to use for analysis. Can be fitting dictionaries from the package or any norm dictionary with the format of a data.frame with one column 'Word' which includes tokens / lemmas and other columns providing norm based scores (e.g. concreteness level per word).
#' @param column The column/columns in the dictionary to be used in the analysis.
#' @param word_column The column in the dictionary which is the index of tokens / lemmas Defaults to 'Word'.
#' 
#' 
#' @keywords lexicon, dictionary
#' @export
#' @examples
#' proc_toks <- preproc_text(data.frame(text="Uneasy lies the head that wears the crown."))
#' norm_col <-  dict_info$norm_column[[which(dict_info$dict_names=="concrete")]]
#' result <- norm_dicts(proc_toks,now_dict=concrete,column=norm_col)
#' colnames(result)[2] <- dict_info$out_norm_column[[which(dict_info$dict_names=="concrete")]]
#' result
#' 
#' proc_lems <- preproc_text(data.frame(text="Uneasy lies the head that wears the crown."),dict_entry = 'lemmas')
#' norm_col <-  dict_info$norm_column[[which(dict_info$dict_names=="VAD")]]
#' result <- norm_dicts(proc_toks,now_dict=VAD,column=norm_col)
#' colnames(result)[2:4] <- dict_info$out_norm_column[[which(dict_info$dict_names=="VAD")]]
#' result


norm_dicts <- function (now_toks,now_dict,column,word_column='Word'){
  dict <- now_dict[,c(word_column,column)]
  
  all_df <- tibble(docname=names(now_toks))
  now_words <- dict[[word_column]] %>% tolower()
  for (i in (1:(length(dict)-1))) {
    now_norms <- dict[[column[i]]]
    all_m_count <- numeric(length(now_toks))
    for (tok_i in 1:length(now_toks)) {
      now_m_count <- mean(now_norms[now_words%in%now_toks[[tok_i]]],na.rm=T)
      all_m_count[tok_i] <- now_m_count
    }
    now_df <- tibble(V1=all_m_count)
    colnames(now_df) <- paste0('m_',column[i])
    all_df <- cbind(all_df,now_df)
  }
  
  return(all_df)
}    



#' A function for text analysis with simple count dictionaries
#'
#' This function performs an analysis on pre-processed text, calculating the proportions of words per dictionary in the text.
#' This function isn't limited to dictionaries in the package and can be used with additional custom dictionaries (as opposed to the analyze_text function).
#' @param now_toks The tokens from the text pre-processing.
#' @param now_dict The simple ("LIWCalike") dictionary to use for analysis. Can be fitting dictionaries from the package or any dictionary with the format of a quanteda::dictionary() (e.g. a dictionary of words that represent negative emotion).
#' 
#' @keywords lexicon, dictionary
#' @export
#' @examples
#' 
#' proc_toks <- preproc_text(data.frame(text="I wasted time, and now doth time waste me."))
#' liwcalike_dicts(proc_toks,AFINN)
#' 
#' 


liwcalike_dicts <- function (now_toks,now_dict){
  all_count <- quanteda::dfm_lookup(quanteda::dfm(now_toks),dictionary = now_dict)/quanteda::ntoken(now_toks)*100
  all_df <- quanteda::convert(all_count,'data.frame') %>% as_tibble()
  colnames(all_df)[1] <- 'docname'
  return(all_df)
}    


