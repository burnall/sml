fun is_older(date_a : int * int * int, date_b : int * int * int) =
    (#1 date_a < #1 date_b) orelse
    (#1 date_a = #1 date_b) andalso (#2 date_a < #2 date_b) orelse
    (#1 date_a = #1 date_b) andalso (#2 date_a = #2 date_b) andalso (#3 date_a < #3 date_b) 

fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else number_in_month(tl dates, month) + (if (#2 (hd dates) = month) then 1 else 0)

fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
       let 
           val tl_dates = dates_in_month(tl dates, month) 
       in 
           if (#2 (hd dates) = month) then hd dates :: tl_dates else tl_dates
       end 

(* Utility function *)
fun append(a : (int * int * int) list, b : (int * int * int) list) =
    if null a
    then b
    else (hd a) :: append(tl a, b)


fun dates_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else append(dates_in_month(dates, hd months), dates_in_months(dates, tl months))

fun get_nth(items : string list, index : int) =
    if index = 1
    then hd items
    else get_nth(tl items, index - 1)

fun date_to_string(year : int, month : int, day : int) = 
    let 
        val months = ["January", "February", "March", "April", "May", "June", 
            "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, month) ^ " " ^ (Int.toString day) ^ ", " ^ (Int.toString year)  
    end 

fun number_before_reaching_sum(limit : int, items : int list) = 
    if limit <= hd items
    then 0
    else 1 + number_before_reaching_sum(limit - hd items, tl items)

fun what_month(day_of_year : int) = 
    let 
        val month_lengths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
        1 + number_before_reaching_sum(day_of_year, month_lengths)
    end

fun month_range(day_a : int, day_b : int) = 
    if day_a > day_b
    then []
    else what_month day_a :: month_range(day_a + 1, day_b)

fun oldest(dates : (int * int * int) list) =
    if null dates 
    then NONE
    else
        let 
            fun min_date(dates, current_min) =
                if null dates
                then current_min
                else 
                    let 
                        val rest_min = min_date(tl dates, current_min)
                    in
                        if is_older(hd dates, rest_min)
                        then hd dates
                        else rest_min
                    end
        in
            SOME (min_date(tl dates, hd dates))
        end       
