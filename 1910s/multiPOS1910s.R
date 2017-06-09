#tim schott
#a script to: perform POS tagging on multiple files at one time. 
#and:output those tags to text files rather than just the data. 
library("rJava", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("openNLP", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library(NLP)
library(stringr)
library(tm)
library(gsubfn)
library(plyr)

#tweaked version of text clean to deal with annoying extra chars. 
text.clean<-function(text){
  #first, convert text to lower
  text.lower<-tolower(text)
  #remove tabs and tab-like spacings
  text.untabbed<-gsub("\\t+|\\s+", " ", text.lower)
  #pad newlines with spaces
  text.newline<-gsub("$"," ",text.untabbed)
  #then, split the text into characters
  text.letters<-unlist(strsplit(text.newline, ""))
  #then find which characters aren't letters
  bad.index<-which(!text.letters %in% letters)
  #then, find which characters are hyphens
  #hyphen.index<-which(text.letters=="-")
  #then, convert hyphens into spaces
  #text.letters[hyphen.index]<-" "
  #then, find the location of the spaces
  space.index<-which(text.letters==" ")
  #find which things in bad.index are spaces
  spaces.in.bad<-which(bad.index %in% space.index)
  #remove the spaces from bad.index
  bad.index<-bad.index[-spaces.in.bad]
  #remove all of the non letters from the character vector
  text.letters<-text.letters[-bad.index]
  #collapse the character vector back into the text
  text.all<-paste(text.letters, collapse="")
  #remove any remaining white space (leading/trailing)
  text.final<-gsub("\\s+", " ", text.all)
  text.final<-gsub("^\\s+|\\s+$", "", text.final)
  text.final<-gsub(" revcompus ", "", text.final) #replace us bc its honestly not worth the occurences. 
  text.final<-gsub(" st ", "", text.final)
  text.final<-gsub(" c ", "", text.final)
  text.final<-gsub(" ct ", "", text.final)
  text.final<-gsub(" u ", "", text.final)
  text.final<-gsub(" s ", "", text.final)
  text.final<-gsub(" supl ", "", text.final)
  text.final<-gsub(" ed ", "", text.final)
  text.final<-gsub(" ampc ", "", text.final)
  text.final<-gsub(" c ", "", text.final)
  #finally, return the cleaned text
  return(text.final)
}

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()



files<-list.files()
files<-files[1:11] ##change

#results_df <- data.frame(matrix(ncol=5, nrow=length(files)))

name_vec<-c("")
adv_vec<-c(0)
adv_percent<-c(0)
adj_vec<-c(0)
adj_percent<-c(0)
ger_vec<-c(0)
ger_percent<-c(0)
count_vec<-c(0)


get_pos_stats<-function(x){
  for(i in 1:length(x)){
    case_name<-x[i]
    adv_cnt<-0
    adj_cnt<-0
    ger_cnt<-0
    
    raw_case<-scan(case_name,what="char",sep="\n")
    clean_case<-text.clean(raw_case)
    
    s <- as.String(clean_case)
    sent_token_annotator <- Maxent_Sent_Token_Annotator()
    word_token_annotator <- Maxent_Word_Token_Annotator()
    a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
    pos_tag_annotator <- Maxent_POS_Tag_Annotator()
    a3 <- annotate(s, pos_tag_annotator, a2)
    a3w <- subset(a3, type == "word")
    tags <- sapply(a3w$features, '[[', "POS")
    casetagged<-sprintf("%s/%s", s[a3w], tags)
    
    for(z in 1:(length(tags))){
      if(tags[z]=="RB" || tags[z] == "RBR" || tags[z] == "RBS"){
        adv_cnt = adv_cnt +1
      }
      if(tags[z]=="JJ" || tags[z] == "JJR" || tags[z] == "JJS"){
        adj_cnt = adj_cnt +1
      }
      if(tags[z] == "VBG"){
        ger_cnt = ger_cnt+1
      }
    }
    #fill our data frame
    name_vec[i] <- case_name
    adj_vec[i] <-adj_cnt
    adj_percent[i] <-(adj_cnt/(length(tags)))*100
    adv_vec[i] <- adv_cnt
    adv_percent[i] <-(adv_cnt/(length(tags)))*100
    ger_vec[i] <- ger_cnt
    ger_percent[i] <-(ger_cnt/(length(tags)))*100
    count_vec[i]<-length(tags)
    
  }
  my_df <-data.frame(name_vec, count_vec, adj_vec, adj_percent, adv_vec, adv_percent,
                     ger_vec, ger_percent)
  colnames(my_df)<-c("case", "#words", "#adj", "%adj", "#adv", "%adv", "#ger", "%ger")
  write.csv(my_df, file = "1910s_stats.csv")
  #return(my_df)
}
get_pos_stats(files)

adj_tags<-c("")
adv_tags<-c("")
ger_tags<-c("")
adj_indices<-c(0)
adv_indices<-c(0)
ger_indices<-c(0)
new_name <-c("")

get_pos_tags<-function(x){
  for(i in 1:length(x)){
    case_name<-x[i]
    adj_indices<-c(0)
    adv_indices<-c(0)
    ger_indices<-c(0)
    raw_case<-scan(case_name,what="char",sep="\n")
    clean_case<-text.clean(raw_case)
    
    s <- as.String(clean_case)
    sent_token_annotator <- Maxent_Sent_Token_Annotator()
    word_token_annotator <- Maxent_Word_Token_Annotator()
    a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
    pos_tag_annotator <- Maxent_POS_Tag_Annotator()
    a3 <- annotate(s, pos_tag_annotator, a2)
    a3w <- subset(a3, type == "word")
    tags <- sapply(a3w$features, '[[', "POS")
    casetagged<-sprintf("%s/%s", s[a3w], tags)
    
    
    for(i in (1: length(tags))){
      if(tags[i]=="JJ" || tags[i] == "JJR" || tags[i] == "JJS"){
        adj_indices<-c(adj_indices, i)
      }
    }
    for(i in (1: length(tags))){
      if(tags[i]=="RB" || tags[i] == "RBR" || tags[i] == "RBS"){
        adv_indices<-c(adv_indices, i)
      }
    }
    for(i in (1: length(tags))){
      if(tags[i] == "VBG"){
        ger_indices<-c(ger_indices, i)
      }
    }
    
    #collect word-tag pairs
    adj_tags<-casetagged[adj_indices]
    adv_tags<-casetagged[adv_indices]
    ger_tags<-casetagged[ger_indices]
    
    case_split<-unlist(strsplit(case_name, "[.]"))
    mystring<-c("tags")
    case_collapsed<-paste(case_split[2:4], collapse="")
    tags_name<-paste(case_collapsed, mystring)
    #print(case_name)
    cat(adv_tags, file =tags_name, sep = "\n")
    #print(adj_tags[1:10])
    #print(adv_tags[1:10])
    #print(ger_tags[1:10])
    write(adj_tags, file =tags_name, append=TRUE)
    write(ger_tags, file =tags_name, append=TRUE)
  }
}
get_pos_tags(files)
