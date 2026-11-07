# =====================================================================
#  tests.R  -  tests the Gujarati number + meaning logic WITHOUT a microphone
#  Run in RStudio: open this file (same folder as app.R) and press Source,
#  or in the Console:  source("tests.R", encoding = "UTF-8")
# =====================================================================
Sys.setenv(AGARBATTI_TEST = "1")
source("app.R", encoding = "UTF-8")       # loads the functions only; the web app is not started

pass <- 0; fail <- 0
check <- function(name, ok, detail = "") {
  if (isTRUE(ok)) { pass <<- pass + 1; cat("PASS ", name, "\n", sep = "") }
  else { fail <<- fail + 1; cat("FAIL ", name, "  ", detail, "\n", sep = "") }
}

# ---------- A. number words alone -------------------------------------
num_of <- function(phrase) {
  q <- parse_numbers(unlist(split_clauses(phrase)))
  if (length(q) == 1) q[[1]]$value else NA_real_
}
numbers <- list(
  "બે" = 2, "બે સો" = 200, "બસો" = 200, "પાંચસો" = 500, "એક પોઇન્ટ આઠ" = 1.8,
  "શૂન્ય પોઇન્ટ એક પાંચ" = 0.15, "દોઢસો" = 150, "અઢીસો" = 250, "સવાસો" = 125,
  "એકસો પચાસ" = 150, "પાંચસો વીસ" = 520, "અઠ્ઠોતેર" = 78, "બે હજાર પાંચસો" = 2500,
  "ત્રણ પોઇન્ટ બે" = 3.2, "સાડા ત્રણ" = 3.5, "દોઢ" = 1.5, "સો" = 100,
  "twenty five" = 25, "one hundred fifty" = 150, "two hundred" = 200, "2.5" = 2.5,
  "\u0AE8\u0AE6\u0AE6" = 200, "five hundred" = 500, "વીસ" = 20, "નેવું" = 90, "સિત્તેર" = 70)
for (p in names(numbers)) check(paste0("number: ", p, " = ", numbers[[p]]), isTRUE(abs(num_of(p) - numbers[[p]]) < 1e-9),
                                paste("got", num_of(p)))

# ---------- B. whole sentences -----------------------------------------
# say, form before (named list), expected changes (named list), clarification needed?
cases <- list(
  list("પરફ્યુમ બે કિલો છે અને તેનો ભાવ પાંચસો રૂપિયા છે.", list(), list(perfume_kg = 2, perfume_price = 500), FALSE),
  list("પાંચસો રૂપિયાના ભાવે બે કિલો પરફ્યુમ છે.", list(), list(perfume_kg = 2, perfume_price = 500), FALSE),
  list("પરફ્યુમનો ભાવ પાંચસો છે, વજન બે કિલો છે.", list(), list(perfume_kg = 2, perfume_price = 500), FALSE),
  list(paste("બે સો પેકેટ બનાવવાના છે. કાચી અગરબત્તીનો ભાવ અઠ્ઠોતેર છે. પરફ્યુમનો ભાવ પાંચસો છે.",
             "પેકેટનું વજન દોઢસો ગ્રામ રાખવાનું છે. DEP ત્રણ પોઇન્ટ બે કિલો છે. પરફ્યુમ એક પોઇન્ટ આઠ કિલો છે.",
             "DEP નો ભાવ સો રૂપિયા છે."), list(),
       list(packets = 200, raw_price = 78, perfume_price = 500, packet_kg = 0.15, dep_kg = 3.2, perfume_kg = 1.8, dep_price = 100), FALSE),
  list("પરફ્યુમ બે કિલો છે... નહીં, એક પોઇન્ટ આઠ કિલો છે.", list(), list(perfume_kg = 1.8), FALSE),
  list("પરફ્યુમનો ભાવ પાંચસો ને બદલે પાંચસો વીસ કરો.", list(perfume_kg = 1.8, perfume_price = 500, packets = 200), list(perfume_price = 520), FALSE),
  list("પરફ્યુમ બે કિલો, પરફ્યુમનો ભાવ પાંચસો, બસો પેકેટ", list(dep_kg = 3.2), list(perfume_kg = 2, perfume_price = 500, packets = 200), FALSE),
  list("ભાવ પાંચસો રાખો.", list(perfume_price = 400), list(), TRUE),
  list("perfume two kilo price five hundred rupees", list(), list(perfume_kg = 2, perfume_price = 500), FALSE),
  list("DEP ત્રણ point બે kilo", list(), list(dep_kg = 3.2), FALSE),
  list("perfume 2 kg price 500", list(), list(perfume_kg = 2, perfume_price = 500), FALSE),
  list("પેકેટનું વજન દોઢસો ગ્રામ", list(), list(packet_kg = 0.15), FALSE),
  list("પેકેટનું વજન અઢીસો ગ્રામ", list(), list(packet_kg = 0.25), FALSE),
  list("પેકેટનું વજન એકસો પચાસ ગ્રામ", list(), list(packet_kg = 0.15), FALSE),
  list("પેકેટનું વજન શૂન્ય પોઇન્ટ એક પાંચ", list(), list(packet_kg = 0.15), FALSE),
  list("પેકેટનું વજન શૂન્ય પોઇન્ટ એક પાંચ કિલો", list(), list(packet_kg = 0.15), FALSE),
  list("એક પોઇન્ટ આઠ કિલો પરફ્યુમ", list(), list(perfume_kg = 1.8), FALSE),
  list("પેકેટનું વજન દોઢસો", list(), list(), TRUE),
  list("એક પેકેટનું વજન એકસો પચાસ ગ્રામ", list(), list(packet_kg = 0.15), FALSE),
  list("પેકિંગ ચાર્જ બે રૂપિયા", list(), list(pack_charge = 2), FALSE),
  list("લેબલિંગનો ખર્ચ દોઢ રૂપિયો", list(), list(pack_charge = 1.5), FALSE),
  list("બસો પેકેટ", list(), list(packets = 200), FALSE),
  list("ત્રણસો નંગ", list(), list(packets = 300), FALSE),
  list("કાચી અગરબત્તીનો ભાવ સિત્તેર", list(), list(raw_price = 70), FALSE),
  list("ડી.ઈ.પી. ત્રણ કિલો", list(), list(dep_kg = 3), FALSE),
  list("DEP ઓઇલ ત્રણ કિલો ભાવ સો", list(), list(dep_kg = 3, dep_price = 100), FALSE),
  list("અત્તર બે કિલો", list(), list(perfume_kg = 2), FALSE),
  list("પેકિંગનો ખર્ચ એક રૂપિયો અને પેકેટ બસો", list(), list(pack_charge = 1, packets = 200), FALSE),
  list("પરફ્યુમનો ભાવ પાંચસો, પરફ્યુમનો ભાવ પાંચસો", list(), list(perfume_price = 500), FALSE),
  list("DEP નો ભાવ એકસો વીસ", list(perfume_kg = 1.8), list(dep_price = 120), FALSE),
  list("પરફ્યુમ ભાવ", list(), list(), FALSE),
  list("પરફ્યુમ બે", list(), list(), TRUE),
  list("પરફ્યુમ \u0AE8 કિલો, ભાવ \u0AEB\u0AE6\u0AE6", list(), list(perfume_kg = 2, perfume_price = 500), FALSE),
  list("પરફ્યુમ સાડા ત્રણ કિલો", list(), list(perfume_kg = 3.5), FALSE),
  list("પરફ્યુમ અઢી કિલો", list(), list(perfume_kg = 2.5), FALSE),
  list("DEP બે હજાર પાંચસો ગ્રામ", list(), list(dep_kg = 2.5), FALSE),
  list("કાચી અગરબત્તીનું વજન પચ્ચીસ કિલો", list(raw_price = 78), list(), FALSE),
  list("પરફ્યુમ પાંચસો રૂપિયા કિલો", list(), list(perfume_price = 500), FALSE),
  list("ભાવ પાંચસો રૂપિયા કિલો", list(), list(), TRUE),
  list("પેકેટ એકસો પચીસ ગ્રામ", list(), list(packet_kg = 0.125), FALSE),
  list("પરફ્યુમનો ભાવ પાંચસો નહીં છસો", list(), list(perfume_price = 600), FALSE),
  list("પેકેટ 200", list(), list(packets = 200), FALSE),
  list("ડીઈપી ત્રણ પોઈન્ટ બે કિલો અને ભાવ સો રૂપિયા", list(), list(dep_kg = 3.2, dep_price = 100), FALSE),
  list("પરફ્યુમ એક ... પોઇન્ટ ... આઠ કિલો", list(), list(perfume_kg = 1.8), FALSE),
  list("packets two hundred", list(), list(packets = 200), FALSE),
  list("perfume weight one point eight kg", list(), list(perfume_kg = 1.8), FALSE),
  list("પેકેટનું વજન શૂન્ય પોઇન્ટ એક પાંચ ગ્રામ", list(), list(), TRUE),
  list("પરફ્યુમનો ભાવ પાંચસો વજન બે", list(), list(perfume_price = 500, perfume_kg = 2), FALSE),
  list("અને પેકેટ ત્રણસો બનાવવાના છે", list(perfume_kg = 1.8), list(packets = 300), FALSE),
  list("ના, પરફ્યુમનું વજન એક પોઇન્ટ આઠ કરો", list(perfume_kg = 2), list(perfume_kg = 1.8), FALSE),
  list("પેકેટનું વજન સો ગ્રામ", list(), list(packet_kg = 0.1), FALSE),
  list("અગરબત્તીનો ભાવ પંચોતેર", list(), list(raw_price = 75), FALSE)
)
for (i in seq_along(cases)) {
  tc <- cases[[i]]
  res <- nlu_process(tc[[1]], pending = list(), current = tc[[2]], use_llm = FALSE)
  same_names <- setequal(names(res$updates), names(tc[[3]]))
  same_vals <- same_names && all(vapply(names(tc[[3]]), function(f) abs(res$updates[[f]] - tc[[3]][[f]]) < 1e-9, logical(1)))
  ask_ok <- identical(length(res$asks) > 0, tc[[4]])
  # fields not mentioned stay as they were: only fields in `updates` can change
  untouched_ok <- all(setdiff(names(tc[[2]]), names(res$updates)) %in% names(tc[[2]]))
  check(sprintf("case %02d: %s", i, substr(tc[[1]], 1, 45)), same_vals && ask_ok && untouched_ok,
        paste0("got ", paste(names(res$updates), unlist(res$updates), sep = "=", collapse = ", "),
               " | ask=", length(res$asks) > 0))
}

# ---------- C. conversations over several turns (state is kept) --------
apply_turn <- function(form, pending, say) {
  r <- nlu_process(say, pending = pending, current = form, use_llm = FALSE)
  for (f in names(r$updates)) form[[f]] <- r$updates[[f]]
  list(form = form, pending = r$asks, res = r)
}
t1 <- apply_turn(list(), list(), "પરફ્યુમ બે કિલો અને તેનો ભાવ પાંચસો.")
t2 <- apply_turn(t1$form, t1$pending, "ના, પરફ્યુમનું વજન એક પોઇન્ટ આઠ કરો.")
t3 <- apply_turn(t2$form, t2$pending, "અને પેકેટ ત્રણસો બનાવવાના છે.")
check("follow-up 1-3: weight corrected to 1.8, price kept 500, packets 300",
      isTRUE(t3$form$perfume_kg == 1.8) && isTRUE(t3$form$perfume_price == 500) && isTRUE(t3$form$packets == 300) &&
        length(t3$form) == 3)
a1 <- apply_turn(list(perfume_price = 400, dep_price = 100), list(), "ભાવ પાંચસો રાખો.")
check("ambiguous price asks a question and changes nothing", length(a1$pending) == 1 && length(a1$res$updates) == 0)
a2 <- apply_turn(a1$form, a1$pending, "પરફ્યુમ")
check("spoken answer 'પરફ્યુમ' sets perfume price 500 only", isTRUE(a2$form$perfume_price == 500) && isTRUE(a2$form$dep_price == 100) && length(a2$pending) == 0)
b1 <- apply_turn(list(), list(), "પેકેટનું વજન દોઢસો")
b2 <- apply_turn(b1$form, b1$pending, "ગ્રામ")
check("unit question answered by 'ગ્રામ' gives 0.150 kg", isTRUE(abs(b2$form$packet_kg - 0.15) < 1e-9))

# ---------- D. the AI answer is only accepted if it passes validation --
bad <- finalize_updates(list(perfume_kg = -3, packets = 2.5, dep_price = "abc", packet_kg = 0, foo = 7, raw_price = 1e9))
check("invalid / unknown AI values are all rejected", length(bad$updates) == 0)

# ---------- E. the mandatory price test from the calculator ------------
v <- list(perfume_kg = 1.8, perfume_price = 500, dep_kg = 3.2, dep_price = 100, packets = 200,
          packet_kg = 0.15, raw_price = 78, pack_charge = 1)
check("calculator: no validation errors", length(validate_inputs(v)) == 0)
r <- calculate_costs(v)
check("calculator: cost per packet is exactly Rs 16.85", format_inr(r$cost_per_packet) == "\u20b916.85", format_inr(r$cost_per_packet))
check("calculator: total batch cost Rs 3,370.00", format_inr(r$total_cost) == "\u20b93,370.00")

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
