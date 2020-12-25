fun is_older (date1 : (int * int * int), date2 : (int * int * int)) =
    let
	fun day_of_year (date : (int * int * int)) =
	    (#1 date * 365) + (#2 date) * 31 + (#3 date)
    in
	day_of_year(date1) < day_of_year(date2)
    end;
	
fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else
	let
	    val tl_num = number_in_month(tl dates, month)
	in
	    if #2 (hd dates) = month
	    then 1 + tl_num
	    else tl_num
	end;

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null dates orelse null months
    then 0
    else 
	number_in_month(dates, hd months) + number_in_months(dates, tl months);

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	let
	    val tl_dates = dates_in_month(tl dates, month)
	in
	    if #2 (hd dates) = month
	    then (hd dates) :: tl_dates
	    else tl_dates
	end;

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null dates orelse null months
    then []
    else (*if months is []*)
	dates_in_month(dates, hd months) @ dates_in_months(dates, tl months);


fun get_nth (xs : string list, index : int) =
    if index = 1
    then hd xs
    else get_nth(tl xs, index-1);

(*January 20, 2013*)
fun date_to_string (date : (int * int * int)) =
    let
	val month_string = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
    in
	get_nth(month_string, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end;	

fun number_before_reaching_sum (sum : int, xs : int list) = (*return the index that sum of the first index is less than the given sum, such as f 3 [1, 2, 3] = 1, because for index=2 -> 1+2=3*)
    if null xs orelse sum <= hd xs
    then 0
    else
	1 + number_before_reaching_sum(sum - (hd xs), tl xs);

val days_of_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

fun what_month (day : int) =
    number_before_reaching_sum(day, days_of_month) + 1;
    
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
	what_month(day1) :: month_range(day1+1, day2);

fun oldest (xs : (int * int * int) list) =
    if null xs
    then NONE
    else
	let
	    val tl_old = oldest(tl xs)
	in
	    if isSome tl_old andalso is_older(valOf tl_old, hd xs)
	    then tl_old
	    else SOME (hd xs) 
	end;

fun remove_dup (xs : int list) = (* return a no-dup list *)
    let
	fun find_dup(now : int, tail : int list) =
	    if null tail
	    then false
	    else
		if now = hd tail
		then true
		else find_dup(now, tl tail)
    in
	if null xs
	then []
	else
	    let
		val tl_rm = remove_dup(tl xs)
	    in
		if find_dup(hd xs, tl xs)
		then tl_rm
		else hd xs :: tl_rm
	    end
    end;

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    let
	val months = remove_dup(months)
    in
	number_in_months(dates, months)
    end;

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
    let
	val months = remove_dup(months)
    in
	dates_in_months(dates, months)
    end;

val days_of_month_leapyear = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
				 
fun reasonable_date (date : (int * int * int)) =
    let
	fun get_n (index : int, xs : int list) =
	    if index = 1
	    then hd xs
	    else get_n(index-1, tl xs)
	fun is_leap_year (year : int) =
	    ((year mod 400) = 0 orelse (year mod 4) = 0) andalso (year mod 100) <> 0
	fun check_year (year : int) =
	    year > 0
	fun check_month (month : int) =
	    month > 0 andalso month < 12
	fun check_day (day : int, year : int) =
	    if day < 0
	    then false
	    else
		if is_leap_year(year) 
		then
		    day <= (get_n(#2 date, days_of_month_leapyear))
		else
		    day <= (get_n(#2 date, days_of_month))
    in
	check_year(#1 date) andalso check_month(#2 date) andalso check_day(#3 date, #1 date)
    end;
