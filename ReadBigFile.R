# microbenchmark(
#   readr  = read_csv_chunked('medium.csv', callback = DataFrameCallback$new(function(x, pos) subset(x, unif > 9000)), col_types = 'dd', progress = F),
#   readr2 = read_csv_chunked('medium.csv', callback = DataFrameCallback$new(function(x, pos) subset(x, unif > 9000)), col_types = 'dd', progress = F, chunk_size = 1000000),
#   sqldf  = read.csv.sql('medium.csv', sql = 'select * from file where unif > 9000', eol = '\n'),
#   awk    = read.csv(pipe("awk 'BEGIN {FS=\",\"} {if ($2 > 9000) print $0}' medium.csv")),
#   awk2   = read_csv(pipe("awk 'BEGIN {FS=\",\"} {if ($2 > 9000) print $0}' medium.csv"), col_types = 'dd', progress = F),
#   check  = function(values) all(sapply(values[-1], function(x) all.equal(values[[1]], x))),
#   times  = 10L
# )


#CASE 1
#bs_chnn_kpi <-read.csv(pipe("awk 'BEGIN {FS=\",\"} {if ( $1 > 0 ) print $0}' dataset/bs_chnn_kpi.csv"), header=FALSE, sep = ';')
#bs_chnn_kpi <-read.csv(pipe("awk 'BEGIN {FS=\",\"} {if $2~/..\.0[3-5]/ print $0}' dataset/bs_chnn_kpi.csv"), header=FALSE, sep = ';')

#CASE 2
bs_chnn_kpi <-read_delim_chunked('dataset/bs_chnn_kpi.csv', 
                                 callback = DataFrameCallback$new(function(x, pos) subset(x, select = c(1,2,25,26,27))), 
                                 col_types = NULL,
                                 progress = F, 
                                 chunk_size = 10000, 
                                 delim = ';',
                                 quote = "")
#-----------------------------------------------------------------------------------------------------------------------------



















