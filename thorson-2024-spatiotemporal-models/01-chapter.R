
# Section 1.2 Time-to-event model for individual timelines ---------------------

# Lifetime density function
lifetime_density <- function (a, b, k) {
	# Write expression for 1 - S(a) = F(a) = Pr[(D_i - B_i) < a] = CDF
	gompertz_mortality <- expression ( 1-exp(b/k*(1-exp(k*a))) )
	# Symbolic derivative f(a) = d/dt F(a) = density function
	density_function <- D(gompertz_mortality, "a")
	# Evaluate derivative at arguments
	eval(density_function)
}

# Parameters
b <- 0.001 # Initial annual mortality rate
k <- 0.1 # Accelaration in mortality rate
birth_rate <- 0.2 # Average animals born per year
n_t <- 100 # Number of years to simulate (?)

# Calculate peak in lifetime density (used in rejection sampling)
peak <- optimize(
	f = lifetime_density,
	interval = c(0, n_t),
	maximum = TRUE,
	b = b,
	k = k
)
peak
# $maximum
# [1] 46.05172
# 
# $objective
# [1] 0.03715767


# Rejection sampling for age at death
simulate_lifespan <- function (b, k) {
	while (TRUE) {
		A <- runif(n = 1, min = 0, max = 150)
		M <- lifetime_density(a = A, b = b, k = k)
		rand <- runif(n = 1, min = 0, max = peak$maximum)
		if (rand < M) {
			return(A)
		}
	}
}

# Sample births using sequence of exponential distributions
birth <- vector()
death <- vector()
while ( max(0, birth, na.rm = TRUE) < n_t) {
	birth <- c(birth, max(0, birth, na.rm = TRUE) + rexp(n = 1, rate = birth_rate))
}

# Sample deaths from lifespan after birth
for (i in seq_along(birth)) {
	death[i] <- birth[i] + simulate_lifespan(b = b, k = k)
}

# Define
survival_function <- function (a, b, k) {
	exp((b / k) * (1 - exp(k * a)))
}

# Define
ages <- seq(0, 100, 0.1)
survival_to_ages <- survival_function(ages, b = b, k = k)
# Plot
plot(x = ages, y = survival_to_ages, type = "l")
# Define
density_at_ages <- lifetime_density(a = ages, b = b, k = k) 
# Lines
lines(x = ages, y = density_at_ages, col = "red")
# Define
individual <- seq_along(birth)
d <- tibble::tibble(ind = individual, birth = birth, death = death) |>
	tidyr::pivot_longer(
		cols = !ind,
		names_to = "name",
		values_to = "value"
	) |>
	dplyr::arrange(ind, name) |>
	dplyr::mutate(ind = as.double(ind)) |>
	dplyr::group_by(ind)
# Plot
ggplot2::ggplot(d, ggplot2::aes(x = value, y = ind, group = ind)) +
	ggplot2::geom_line()
# Define
d2 <- tibble::tibble(birth = birth, death = death)
d2
# Define
incr <- 0.01
year <- seq(0, 150, incr)
pop <- numeric(length(year))
# Define: How many births occured in each interval? Deaths?
for (i in seq_along(year)) {
	# How many births?
	incr_births <- d2 |>
		dplyr::filter(birth >= year[i] & birth < year[i] + incr) |>
		nrow()
	# How many deaths?
	incr_deaths <- d2 |>
		dplyr::filter(death >= year[i] & death < year[i] + incr) |>
		nrow()
	# How many alive?
	if (i == 1) {
		pop[i] <- 0
	} else {
		pop[i] <- pop[i - 1] + incr_births - incr_deaths
	}
}
pop
plot(x = year, y = pop, type = "l")
