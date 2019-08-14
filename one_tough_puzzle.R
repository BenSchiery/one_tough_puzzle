rm(list = ls())

rot <- function(x){
  n <- ceiling(log10(x + 1))
  r <- x %% 10
  (x - r) / 10 + r * 10^(n - 1) 
}

to.digits <- function(x){
  n <- ceiling(log10(x + 1))
  v <- vector("numeric", length = n)
  for(i in 1:n){
    r <- x %% 10
    x <- (x - r) / 10
    v[i] <- r
  }
  rev(v)
}

to.piece <- function(x){
  v <- to.digits(x)[1:4]
  w <- rep(NA, times = 9)
  w[c(8, 4, 2, 6)] <- v
  matrix(w, nrow = 3)
}

rot.piece <- function(m, n = 1){
  v <- m[c(8, 4, 2, 6)]
  w <- rep(NA, times = 9)
  for(i in 1:n){
    v <- c(v[length(v)], v[-length(v)])
  }
  w[c(8, 4, 2, 6)] <- v
  matrix(w, nrow = 3)
}

assemble <- function(q){
  A <- matrix(nrow = 9, ncol = 9)
  ro <- ceiling(row(A) / 3)
  co <- ceiling(col(A) / 3)
  for(x in 1:3){
    for(y in 1:3){
      ind <- 3 * (y - 1) + x
      A[(ro == x) & (co == y)] <- q[[ind]]
    }
  }
  A
}

link.count <- function(A){
  z1 <- c(20, 47)
  z2 <- z1 + 3
  z3 <- z2 + 3
  z4 <- c(12, 39, 66)
  z5 <- z4 + 3
  Z1 <- c(z1, z2, z3, z4, z5)
  Z2 <- c(c(z1, z2, z3) + 9, c(z4, z5) + 1)
  links <- t(apply(X = cbind(Z1, Z2), 
                   MARGIN = 1,
                   FUN = function(k){
                     c(A[k[1]], A[k[2]])
                   }))
  sum(abs(links[,1] - links[,2]) == 4)
}

mutate <- function(q){
  swap.prob <- 1/8
  swap <- runif(1) < swap.prob
  while(swap){
    w <- sample(9, size = 2)
    q[w] <- q[rev(w)]
    swap <- runif(1) < swap.prob
  }
  
  rot.prob <- 1/8
  to.rot <- which(rbinom(n = 9, 
                         size = 2, 
                         prob = rot.prob) == 1)
  if(length(to.rot) > 0){
    q[to.rot] <- lapply(X = q[to.rot],
                        FUN = function(x){
                          n <- sample(3, size = 1)
                          x <- rot.piece(m =  x, n = n)
                        })
  }
  
  q
}

# 1 = Fc
# 2 = Fd
# 3 = Fh
# 4 = Fs
# 5 = Mc
# 6 = Md
# 7 = Mh
# 8 = Ms

pop <- 1000
gen.lim <- 200
auto.survive <- F

a <- 1167
b <- 1275
c <- 1388
d <- 1487
e <- 2156
f <- 2368
g <- 3267
h <- 3468
i <- 3475

pieces <- c(a,b,c,d,e,f,g,h,i)
pieces <- sapply(X = pieces, 
                 FUN = function(x){
                   r <- 0.7
                   while(runif(1) < r){
                     x <- rot(x)
                   }
                   x
                 })

queue <- lapply(X = pieces,
                FUN = function(x){
                  to.piece(x)
                })
queue <- queue[sample(9)]

##############
#### loop ####
##############

best.lct <- 0
gen <- 1
while(best.lct != 12){
  if(gen > gen.lim){
    pieces <- c(a,b,c,d,e,f,g,h,i)
    pieces <- sapply(X = pieces, 
                     FUN = function(x){
                       r <- 0.7
                       while(runif(1) < r){
                         x <- rot(x)
                       }
                       x
                     })
    
    queue <- lapply(X = pieces,
                    FUN = function(x){
                      to.piece(x)
                    })
    queue <- queue[sample(9)]
    gen <- 1
  } # restart with a fresh seed
  
  q.list <- lapply(X = 1:pop,
                   FUN = function(k){
                     queue
                   }) # whole population is identical to the last gen's winner
  q.list <- lapply(X = q.list,
                   FUN = mutate) # mutate the whole population
  if(auto.survive){
    q.list[[1]] <- queue
  } # ensure that one of this generation remains unmutated
  
  a.list <- lapply(X = q.list,
                   FUN = assemble) # assemble everyone
  
  lct <- sapply(X = a.list, FUN = link.count) # count everyone's valid links
  
  winner <- which(lct == max(lct)) # the members of the population with maximal link count
  if(length(winner) > 1){
    winner <- sample(winner, size = 1)
  } # pick one of the winners if there are many
  best.lct <- lct[winner] # record the best link count
  queue <- q.list[[winner]] # record the winner
  
  cat("\r    \r", "Gen: ", gen, ",  Best this gen: ", best.lct, sep = "")
  if(gen == gen.lim){
    cat("\n")
    print(assemble(queue))
  }
  gen <- gen + 1
}

assemble(queue)
