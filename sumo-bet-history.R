library(rvest)
library(stringr)
library(tidyverse)

list.files("html", pattern = ".+\\.html$") %>% 
	lapply(
		function(file_name) file_name %>% 
			paste("html", ., sep = "/") %>% 
			readChar(., file.info(.)$size) %>% 
			read_html() %>% 
			html_nodes("table.history-result-main") %>% 
			html_table() %>% 
			do.call(rbind, .) %>% 
			mutate(basho = substr(file_name, 1, 6))
	) %>% 
	do.call(rbind, .) %>% 
	transmute(
		basho,
		ts = sprintf(
			"%s-%s-%02d %s",
			substr(basho, 1, 4),
			substr(basho, 5, 6),
			as.integer(str_extract(X1, "^\\d+")),
			str_extract(X1, "\\d{2}:\\d{2}$")
		),
		stake = as.numeric(str_extract(X2, "\\d+\\.?\\d*")),
		rikishi = str_extract(X4, "\\w+$"),
		paid_out = as.numeric(str_extract(X5, "\\d+\\.?\\d*"))
	) %>% 
	#summarise_if(is.numeric, sum)
	#View()
	write_csv(
		path = "sumo-bet_history.csv",
		append = TRUE
	)
