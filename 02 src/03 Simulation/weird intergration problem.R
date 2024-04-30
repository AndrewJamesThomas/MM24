library(ggplot2)

# POC
eq <- function(x) x^2
curve(eq, from=0, to=3)


# Solve this problem three different ways:
# what is the probabliity of a fair coin landing heads 30/100 times?

N <- 1000
P <- 0.5
p_ <- 0.45

# Use Binomial Distribution
pbinom(p_*N, N, P) %>% round(2)

# Use monte carlo simulation
mean(rbinom(N, N, P) <= p_*N) %>% round(2)

# Use normal distribution
pnorm(p_*N, mean=P*N, sd=sqrt(N*P*(1-P))) %>% round(2)

# use quadratic approximation
df <- rbinom(N, N, P)
df <- tibble(x = df) %>% 
  count(x, sort=TRUE) %>% 
  mutate(y = n/N)

# build poly model
df <- df %>% 
  mutate(x2 = x^2,
         x3 = x^3,
         x4 = x^4,
         x5 = x^5) %>% 
  select(-c(n))

model <- lm(y ~ ., data=df)
summary(model)

# this hsould look nice and normal
df$pred <- predict(model, df)
ggplot(data=df) +
  geom_point(aes(x=x, y=y)) +
  geom_smooth(aes(x=x, y=y)) +
  geom_vline(xintercept=P*N, color="dark red")

c <- coef(model) %>% as.vector
eq <- function(x) c[1] + x*c[2] + c[3]*x^2 + c[4]*x^3 + c[5]*x^4 + c[6]*x^5
curve(eq, from=460, to=540)
integrate(eq, lower=400, upper=450)

