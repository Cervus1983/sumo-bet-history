library(rvest)
library(stringr)
library(tidyverse)

# https://www.marathonbet.com/en/myaccount/myaccount.htm
"html/marathonbet/2018.05" %>% 
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
		#winner = sub("^Winner - ", "", Bet),
		winner = str_match(Bet, "Fight Result \\(2 way\\) - (.+) To Win")[,2],
		stake = as.numeric(str_extract(`Total Stake`, "\\d+\\.?\\d*")),
		return = as.numeric(str_extract(Return, "\\d+\\.?\\d*")),
		status = Status,
		odds = Odds
	) %>% 
	#View()
	write_csv("csv/marathonbet/2018.05.csv")

# https://1xbet.com/en/office/history/
"html/1xbet/2018.05.html" %>% 
	read_html() %>% 
	html_nodes("table") %>% 
	lapply(
		function(x) x %>% 
			html_table(fill = TRUE) %>% 
			.[c(1, 4, 7), -1] %>% 
			gather(var, val) %>% 
			.[c(1:2, 5:6, 9, 11:12), 2] %>% 
			t() %>% 
			as_tibble() %>% 
			transmute(
				ts = str_match(V1, "(\\d{2}\\.\\d{2}\\.\\d{4}) \\| (\\d{2}:\\d{2})") %>% 
					.[2:3] %>% 
					paste(collapse = " ") %>% 
					as.POSIXct(format = "%d.%m.%Y %H:%M") %>% 
					format("%Y-%m-%d %H:%M"),
				rikishi1 = str_match(V2, "^.+[a-z]([A-Z][a-z]+)")[, 2],
				rikishi2 = str_match(V2, "^.+[a-z][A-Z][a-z]+ - ([A-Z][a-z]+)")[, 2],
				outcome = V3,
				stake = as.numeric(str_extract(V4, "\\d+\\.?\\d*")),
				odds = as.numeric(V5),
				status = V6,
				return = as.numeric(V7)
			)
	) %>% 
	do.call(rbind, .) %>% 
	replace_na(replace = list(return = 0)) %>% 
	#View()
	write_csv("csv/1xbet/2018.05.csv")

balance <- function(fn) fn %>% 
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

"csv" %>% 
	list.files(pattern = "\\.csv$", full.names = TRUE, recursive = TRUE) %>% 
	setdiff(., "csv/marathonbet/2018.01.csv") %>% 
	sapply(
		balance,
		simplify = FALSE
	)
