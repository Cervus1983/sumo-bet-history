library(rvest)
library(stringr)
library(tidyverse)

"html/marathonbet/2018.03" %>% 
	list.files(
		pattern = ".+\\.html$",
		full.names = TRUE
	) %>% 
	lapply(
		function(fn) fn %>% 
			read_html() %>% 
			html_nodes("table.history-result") %>% 
			html_table(header = TRUE) %>% 
			.[[1]] %>% 
			.[c(TRUE, FALSE), ]
	) %>% 
	do.call(rbind, .) %>% 
	select(-4) %>% 
	transmute(
		ts = Date %>% 
			as.POSIXct(format = "%d %b%H:%M") %>% 
			format("%Y-%m-%d %H:%M"),
		winner = sub("^Winner - ", "", Bet),
		stake = as.numeric(str_extract(`Total Stake`, "\\d+\\.?\\d*")),
		return = as.numeric(str_extract(Return, "\\d+\\.?\\d*")),
		status = Status,
		odds = Odds
	) %>% 
	#View() %>% 
	write_csv("csv/marathonbet/2018.03.csv")

"csv/marathonbet/2018.03.csv" %>% 
	read_csv() %>% 
	group_by(status) %>% 
	summarise(
		bets = n(),
		stake = sum(stake),
		return = sum(return)
	) %>% 
	rbind(
		.,
		data.frame(
			status = "Total",
			t(colSums(.[, -1]))
		)
	)
