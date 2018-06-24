# CSV読み込み
df_conversion <- read.csv("~/conversion.csv", header=T, fileEncoding = "UTF-8", stringsAsFactors=F)
df_recommend <- read.csv("~/recommend.csv", header=T, fileEncoding = "UTF-8", stringsAsFactors=F)

month <- 6
start_day <- 1
end_day <- 2

# 日付の回数だけ処理を繰り返す
for(day in start_day:end_day){
  # 今回のイテレーションの日付
  date_str <- paste(month,"/",day, sep = "")
  print(paste("date:", date_str, sep = ""))
  # 今回のイテレーションの日付分のデータを抜き出す
  tmp1 <- (df_recommend$date == date_str)
  df_recommend_today <- df_recommend[tmp1,]
  tmp2 <- (df_conversion$date == date_str)
  df_conversion_today <- df_conversion[tmp2,]

  # ユニークなユーザ数だけ繰り返す
  uids <- unique(df_recommend_today$uid)
  for(uid in uids){
    print(paste("uid:", uid, sep = ""))
    # コンバージョン全数
    total_true <- 0
    # レコメンド全数
    total_positive <- 0
    # レコメンドとコンバージョンの一致数
    true_positive <- 0
    # 適合率
    precision <- 0
    # 再現率
    recall <- 0
    
    # 今回のイテレーション日付で、今回のイレテーションのユーザへのレコメンド
    df_recommend_confuse <- df_recommend_today[(df_recommend_today$uid == uid),]
    df_recommend_confuse <- df_recommend_confuse[order(df_recommend_confuse$score, decreasing=T),]
    print(df_recommend_confuse)
    # 今回のイテレーション日付で、今回のイレテーションのユーザの実コンバージョン
    df_conversion_confuse <- df_conversion_today[(df_conversion_today$uid == uid),]
    print(df_conversion_confuse)
    
    for(conv_id in df_conversion_confuse$iid){
      # print(conv_id)
      for(rec_id in df_recommend_confuse$iid){
        if(conv_id == rec_id){
          true_positive <- true_positive + 1
        }
      }
    }
    total_positive <- nrow(df_recommend_confuse)
    total_true <- nrow(df_conversion_confuse)
    percision <- true_positive / total_positive
    recall <- true_positive / total_true
    print(paste("total_positive = ", total_positive, sep=""))
    print(paste("total_true = ", total_true, sep=""))
    print(paste("true_positive = ", true_positive, sep=""))
    print(paste("percision = ", percision, sep=""))
    print(paste("recall = ", recall, sep=""))
  }
  
}